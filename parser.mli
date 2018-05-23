(** The purpose of the [Parser] is to parse the XML specification into
   semantically equivalent OCaml structures, with little to no change and
   preserving all information. Information that can be inferred without
   analyzing other declarations should preferably be inferred here. *)

(** Here follows an explaination of some of the most puzzling aspects of the
X11 spec.


{2 Types}

Types in the spec refer to two different kinds of types, which we'll call
{b primitive} and {b composite}.
Types are used to give a wire representation to the values defined in the spec,
so they define size, alignment, and signedness in the case of ints.

{3 Primitive types}
Primitive types such as [INT8], [CARD16] and [float] are used to define the
wire representation of numbers. All types except for [void] should map to a
certain type in the output bindings.

The [void] type is used for defining things where the type doesn't matter
(like padding) or where it cannot be known (like in [GetProperty]).

{3 Composite types}
Composite types are aggregations of primitive types in the form of simple
fields, lists, or expressions, and padding and alignment information.

Their size is known at compile time except for generic events, requests, and
responses, which might include a variable length list as their last element.
Requests may also include expression fields, whose value depends on the value
of other fields in the request struct.

Structs and requests may include a switch, which uses another field to
determine which additional fields should be included in the struct.

Unions are an earlier version of switches, which require additional processing
to determine which field is included (the method used depends on the extension,
of course: xkb uses a field named [type] which refers to an enum while xproto
has an ad hoc field in the containing struct).
{b NOTE}: These will have to be manually patched into switches somehow.


{2 Enums}

Enums are basically named values. They do not represent types, as they do not
have a wire representation of their own: they can only be used in conjunction
with a primitive type, which defines the size and wire representation of the
selected value.

Enums are used for two different purposes: {b enumerations} and {b bitmasks}.
An enum may be one or both at the same time, depending on whether it's referred
to as [enum]/[altenum] or [mask]/[altmask] in a field type. [enumref]s in
expressions count as an [enum] reference.

{3 Enumerations}
Enumerations are simple mappings from names to values. Only one value from an
enumeration is allowed at a time.

{3 Bitmasks}
Bitmasks contain both [bit] values and [value] values.
{ul {- [bit] is the 0-based position of the set bit in the resulting value
       ([3] is [0b1000], or [1 lsl 3]).}
    {- [value] is a useful literal default, such as [NoEvent] in
       [xproto:EventMask].}}
A bitmask can be either a list of [bit]s or a single [value].


{2 Namespaces}

The declarations have a few separate namespaces:

- types
- enums
- events
- generic events
- errors
- requests

{3 Name clashes}
Name clashes {i within} these namespaces are allowed between different
extensions. To disambiguate in the case that a declaration references a
name exported by two or more extensions currently in scope, the extension's
ID (here aliased as [file_name]) is prefixed to the name with a colon, such
as [xproto:PIXMAP].

{b NOTE}: some extensions DO NOT follow this rule, in which case I assumed
that names defined in the current extension take precedence over the rest.
*)


type pad =
  [ `Bytes of int
  | `Align of int ]

(** Padding bytes in a packet. *)
type padding =
  { pad       : pad
  ; serialize : bool
  (* This flag is only used in xkb to mark "true", if omitted it'll always be
     false. The documentation mentions that it's only needed for ABI
     compatibility with legacy (legacy what???) and thus should not be used
     in new pads. I don't really know what it means. *)
  }


(* Something to do with an alignment checker. TODO check this out:
   https://cgit.freedesktop.org/xcb/proto/commit/?id=c499401bdac3b87bd4f9cd4bc64cfd1781ab447f *)
type required_start_align =
  { align  : int
  ; offset : int option }


type enum_vals = (string * int64) list
type enum_bits = (string * int) list


(** See above for documentation. *)
type enum =
  { vals : enum_vals
  ; bits : enum_bits }


type binop =
  [ `Add | `Sub | `Mul | `Div
  | `Bit_and | `Bit_left_shift ]

type unop =
  [ `Bit_not ]

type expression =
  [ `Binop of binop * expression * expression
    (** Perform a binary operation on the results of the expressions. *)
  | `Unop of unop * expression
    (** Perform a unary operation on the result of the expression. *)
  | `Field_ref of string
    (** The value of another field in the same structure. *)
  | `Param_ref of string * string
    (** The value of a field in a structure that contains the current one.
       (name, type) *)
  | `Enum_ref  of string * string
    (** The value of an identifier in an enum. (enum, item) *)
  | `Sum_of of string * expression option
    (** Sum of the elements in a list field. The expression, if present,
       should be applied to each list element in the element's context before
       summing it. *)
  | `Current_ref
    (** The current element in a Sum_of expression. *)
  | `Pop_count of expression
    (** The number of set bits. (e.g. 0b01101 -> 3) *)
  | `Value of int
    (** A literal value. *)
  | `Bit of int
    (** A literal mask index. *)
  ]


type allowed_vals =
  [ `Enum of string
  | `Mask of string
  | `Alt_enum of string
  | `Alt_mask of string ]

(** A field could contain any type, including enumerated which don't have a
   default wire representation.
   For basic and composite types, the [allowed] field will be empty and the
   [typ] field will contain the name of the type, while for enumerated types
   the [allowed] field will contain the name of the enum or the mask, while the
   [typ] field will be a basic type that determines its wire representation. *)
type field_type =
  { typ     : string
  ; allowed : allowed_vals option }


type static_field =
  [ `Pad of padding
  | `Field of string * field_type
  | `List of string * field_type * expression
  (** The expression defines the length of the list. *)
  | `File_descriptor of string ]

type dynamic_field =
  [ static_field
  | `List_var of string * field_type
  (** A list of variable length. It is only present in structs that don't
     have a fixed size (generic events and requests) and it's the last element
     of the struct. *)
  ]

type request_field =
  [ dynamic_field
  | `Expr of string * field_type * expression
  (** A field whose value is computed based on the other fields. *)
  ]



type cond =
  [ `Bit_and of expression
  (** Test that the switch expression & the case expression != 0 *)
  | `Eq of expression
  (** Test that the switch expression == the case expression *)
  ]

(** A field that uses an expression to determine which fields are included in
   the enclosing struct. *)
type switch =
  { align : required_start_align option
  ; cond  : cond
  ; cases : case list }

(** Essentially an if statement which uses an operation that takes the
   switch expression and the case expression, and includes the fields it
   contains in the switch if it's true.
   If there are multiple expressions, they should be chained with ORs. *)
and case =
  { exprs   : expression list
  ; name    : string option
  (* Why would a case expression ever need a goddamn name?
     What is it for? XInput only knows. *)
  ; align_c : required_start_align option
  ; fields  : static_field list
  ; switch  : (string * switch) option }



(** A 32-byte struct. The first byte contains the code in the first 7 least
   significant bits and a flag in the most significant that is set when the
   event was generated from a SendEvent request.
   Its third and fourth bytes contain the least significant bits of the
   sequence number of the last request issued by the client.
   The code is 7-bit. *)
type event =
  { no_sequence_number : bool
  (** A special case for the KeymapNotify event in xproto, which signals that
     the event struct does not contain a sequence number. *)
  ; align  : required_start_align option
  ; fields : static_field list }

(** The same as a normal event, but can also contain lists with no specified
   length. *)
type generic_event =
  { no_sequence_number : bool
  ; align  : required_start_align option
  ; fields : dynamic_field list }


type allowed_events =
  { extension    : string
  (** The extension-name ([name] here) of the extension the events are defined
     in. *)
  ; opcode_range : int * int
  (** Only events that have opcodes within this range are allowed. *)
  }


(** A 32-byte struct. Its first byte is 0 to differentiate it from events,
   the second contains the error code and the third and fourth contain
   the least significant bits of the sequence number of the request.
   The code is 8-bit, unlike in events. *)
type error =
  { align  : required_start_align option
  ; fields : static_field list }


type struct_fields =
  { fields : static_field list
  ; switch : (string * switch) option }


type request_fields =
  { align  : required_start_align option
  ; fields : request_field list
  ; switch : (string * switch) option }

type reply =
  { align  : required_start_align option
  ; fields : dynamic_field list
  ; switch : (string * switch) option }


type request =
  { combine_adjacent : bool
  (** Whether multiple requests can be combined without affecting the
     semantics of the request. *)
  ; params : request_fields
  ; reply  : reply option }



type declaration =
  [ `Import of string
  (** Expose the types declared in another extension to the current one.
     The argument is the [file_name] field in the extension_info of the
     referenced module. *)

  | `X_id of string
  (** Declare a type alias to u32 representing a generic X resource ID. *)

  | `X_id_union of string * string list
  (** Declare an union type of XIDs. *)

  | `Enum of string * enum
  (** Declare an enum or a bit mask. *)

  | `Type_alias of string * string
  (** Alias a type to a new name. (new, old) *)
  (* One might be led to think that type aliases to another type alias are not
     allowed, and you'd be right if it weren't for xkb. *)

  | `Event of string * int * event
  (** Something happened on the X server, and the client was informed.
     For most events we only need to generate the parsing code, but we need to
     be able to serialize those defined in event structs. *)

  | `Generic_event of string * int * generic_event
  (** Generic events are events that don't have a fixed size. They have a
     completely separate namespace from normal ones, so we need to account
     for collisions. *)

  | `Event_struct of string * allowed_events list
  (** Events that we need to be able to serialize. This creates a new struct
      type that can be used in requests and such. *)

  | `Event_alias of string * int * string
  (** Alias an event with a new event name and number. (new, num, old) *)

  | `Error of string * int * error
  (** Something went wrong with a request made by the client. These only need
     to be parsed. *)

  | `Error_alias of string * int * string
  (** Alias an error with a new event name and number. (new, num, old) *)

  | `Struct of string * struct_fields
  (** Declare a data structure. Clients are probably expected to be able to
     both encode and decode them, but we could also analyze their usage and
     only output code for either encoding or decoding (or none, if they're
     never used). *)

  | `Union of string * static_field list
  (** Represents a field in another struct that may be any one of the fields
     listed in the union. It should be as big as the biggest element in the
     list. *)

  | `Request of string * int * request
  (** A request the client makes to the server. Should be encoded as a function
     that takes whatever parameters are listed in the params field.
     Some requests send a reply back to the client. *)
  ]


type extension_info =
  { name : string
  (** The extension name in camelcase. *)

  ; file_name : string
  (** The filename of the extension's XML description file. *)

  ; query_name : string
  (** Name used by QueryExtension whether the extension is supported on a
     given server. *)

  ; multiword : bool
  (** Whether the resulting C function name prefixes should be composed of
     a single alphanumeric string, or multiple strings separated by _. *)
  (* I'm nor sure why they added this shady flag instead of just using an
     attribute to define the exact C name prefix. *)

  ; version : int * int
  (** (major, minor) version *)
  }


type protocol_file =
  | Core of declaration list
  | Extension of extension_info * declaration list


val parse_file : string -> protocol_file

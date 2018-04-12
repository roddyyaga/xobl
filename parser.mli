(** The purpose of the Parser is to parse the XML specification into
 * semantically equivalent OCaml structure, with little to no change and
 * preserving all information. Information that can be inferred without
 * analyzing other declarations should preferably be inferred here. *)

(** I should probably clarify some of the concepts that are introduced in the
 * spec here, because the official documentation sure as hell doesn't.
 *
 * "Types" are used for two different purposes in the spec, but the
 * documentation doesn't make much of a distinction between these purposes and
 * often uses them interchangeably. I'll try to clear that up here.
 * There's essentially three kinds of "types" in the X11 spec:
 * - basic types
 * - enumerated types
 * - composite types
 *
 * Basic types are stuff like uint8, float32, bool, etc.; they're all
 * different representation of numbers. Their purpose is mostly to define the
 * wire representation of the numbers of that type, i.e. the size in bytes and
 * how to parse and serialize them.
 * XIDs and XID unions are included in this category, because they all map to
 * u32.
 *
 * Enumerated types, to which enums and masks belong, are used to constrain the
 * values that a request may take. They may be used to offer some useful
 * named defaults, or to make sure that you don't send a request that contains
 * an illegal value.
 * Enumerated types don't have a default wire representation: they have to be
 * assigned a basic type to be used anywhere. Basically this means that
 * we'll have to keep track of all the basic types assigned to a certain enum
 * throughout the codebase and generate multiple conversion functions as
 * needed.
 *
 * Composite types are the structs. Like primitive types, their purpose is to
 * define the wire representation of a record of values. They include
 * information such as the order of the values, padding bytes, alignment of
 * certain values, and more. [FINISH THIS]
 * *)


type pad =
  [ `Bytes of int
  | `Align of int ]

(** Padding bytes in a packet. *)
type padding =
  { pad       : pad
  ; serialize : bool
  (* This flag is only used in xkb to mark "true", if omitted it'll always be
   * false. The documentation mentions that it's only needed for ABI
   * compatibility with legacy (legacy what???) and thus should not be used
   * in new pads. I don't really know what it means. *)
  }


(* Something to do with an alignment checker. Should probably check out this
 * thread: https://lists.freedesktop.org/archives/xcb/2015-November/010557.html
 * The author of XEB is confused as to what this exactly is. XCB-types just
 * parses it into an "Alignment" type without giving an explaination and
 * without ever using it in XHB.
 * Most other bindings just ignore it.
 * I think we can safely conclude that nobody except whoever sent the patch
 * to add it knows what this is for. *)
type required_start_align =
  { align  : int
  ; offset : int option }


type enum_items = (string * int) list

type mask =
  { vals  : enum_items
  ; flags : enum_items }

type enum =
  [ `Mask of mask
  (** Masks are can have both "bit" and "value" items, where the bit items
   * are the 0-based position of the bit in the bitmask (e.g. 2 is 1 << 2),
   * and the value items are useful predefined values. *)
  | `Enum of enum_items
  (** Simple mappings from constants to ints. *)
  ]


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
     * (name, type) *)
  | `Enum_ref  of string * string
    (** The value of an identifier in an enum.
     * (enum, item) *)
  | `Sum_of of string * expression option
    (** Sum of the elements in a list field. The expression, if present,
     * should be applied to each list element in the element's context before
     * summing it. *)
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

(** The type of a field is defined by two things: the concrete type,
 * which could be any basic type or a XID union, and an enum or mask, if
 * present.
 * When [allowed] is empty, the type will be the one in the * [typ] field.
 * When [allowed] contains either Enum or Mask, the field will only accept
 * values from that enum or mask, and the [typ] field will only serve to decide
 * how to serialize and parse the value.
 * When it is Alt_enum or Alt_mask, the field will accept both values from
 * the enum and raw values of the type indicated by [typ]. The enum/mask values
 * will be serialized based on [typ] like above. *)
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
   * have a fixed size (generic events and requests) and it's the last element
   * of the struct. *)
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
 * the enclosing struct. *)
type switch =
  { align : required_start_align option
  ; cond : cond
  ; cases : case list }

(** Essentially an if statement which uses an operation that takes the
 * switch expression and the case expression, and includes the fields it
 * contains in the switch if it's true.
 * If there are multiple expressions, they should be chained with ORs. *)
and case =
  { exprs : expression list
  ; name : string option
  (* Why would a case expression ever need a goddamn name?
   * What is it for? XInput only knows. *)
  ; align_c : required_start_align option
  ; fields : static_field list
  ; switch : (string * switch) option }



(** A 32-byte struct. The first byte contains the code in the first 7 least
 * significant bits and a flag in the most significant that is set when the
 * event was generated from a SendEvent request.
 * Its third and fourth bytes contain the least significant bits of the
 * sequence number of the last request issued by the client.
 * The code is 7-bit. *)
type event =
  { no_sequence_number : bool
  (** A special case for the KeymapNotify event in xproto, which signals that
   * the event struct does not contain a sequence number. *)
  ; align  : required_start_align option
  ; fields : static_field list }

(** The same as a normal event, but can also contain lists with no specified
 * length. *)
type generic_event =
  { no_sequence_number : bool
  ; align  : required_start_align option
  ; fields : dynamic_field list }


type allowed_events =
  { extension    : string
  (** The extension the events are defined in. *)
  (* Once again XInput breaks all conventions by referring to an
   * extension not by its "header" field ("file_name" here),
   * but by its "extension-name". *)
  ; opcode_range : int * int
  (** Only events that have opcodes within this range are allowed. *)
  }


(** A 32-byte struct. Its first byte is 0 to differentiate it from events,
 * the second contains the error code and the third and fourth contain
 * the least significant bits of the sequence number of the request.
 * The code is 8-bit, unlike in events. *)
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
   * semantics of the request. *)
  ; params : request_fields
  ; reply  : reply option }



type declaration =
  | Import of string
  (** Expose the types declared in another extension to the current one.
   * The argument is the "file_name" field in the extension_info of the
   * referenced module. *)

  | X_id of string
  (** Declare a type alias to u32 representing a generic X resource ID. *)

  | X_id_union of string * string list
  (** Declare an union type of XIDs. *)

  | Enum of string * enum
  (** Declare an enum or a bit mask. *)

  | Type_alias of string * string
  (** Alias a type (basic type, XID union or enum/mask) to a new name.
   * (new, old) *)
  (* One might be led to think that type aliases to another type alias are not
   * allowed, and you'd be right if it weren't for xkb. *)

  | Event of string * int * event
  (** Something happened on the X server, and the client was informed.
   * For most events we only need to generate the parsing code, but we need to
   * be able to serialize those defined in event structs. *)

  | Generic_event of string * int * generic_event
  (** Generic events are events that don't have a fixed size. They have a
   * completely separate namespace from normal ones, so we need to account
   * for collisions. *)

  | Event_struct of string * allowed_events list
  (** Events that we need to be able to serialize. This creates a new struct
    * type that can be used in requests and such. *)

  | Event_alias of string * int * string
  (** Alias an event with a new event name and number. (new, num, old) *)

  | Error of string * int * error
  (** Something went wrong with a request made by the client. These only need
   * to be parsed. *)

  | Error_alias of string * int * string
  (** Alias an error with a new event name and number. (new, num, old) *)

  | Struct of string * struct_fields
  (** Declare a data structure. Clients are probably expected to be able to
   * both encode and decode them, but we could also analyze their usage and
   * only output code for either encoding or decoding (or none, if they're
   * never used). *)

  | Union of string * static_field list
  (** Represents a field in another struct that may be any one of the fields
   * listed in the union. It should be as big as the biggest element in the
   * list. *)

  | Request of string * int * request
  (** A request the client makes to the server. Should be encoded as a function
   * that takes whatever parameters are listed in the params field.
   * Some requests send a reply back to the client. *)


type extension_info =
  { name : string
  (** The extension name in camelcase. *)

  ; file_name : string
  (** The filename of the extension's XML description file. *)

  ; query_name : string
  (** Name used by QueryExtension whether the extension is supported on a
   * given server. *)

  ; multiword : bool
  (** Whether the resulting C function name prefixes should be composed of
   * a single alphanumeric string, or multiple strings separated by _. *)
  (* I'm nor sure why they added this shady flag instead of just using an
   * attribute to define the exact C name prefix. *)

  ; version : int * int
  (** (major, minor) version *)
  }


type protocol_file =
  | Core of declaration list
  | Extension of extension_info * declaration list


val parse_file : string -> protocol_file

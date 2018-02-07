(** Padding bytes in a packet. *)
type pad_type =
  [ `Bytes | `Align ]

type padding =
  { typ : pad_type
  ; amount : int
  ; serialize : bool }


(** Something to do with an alignment checker. Should probably check out this
 * thread: https://lists.freedesktop.org/archives/xcb/2015-November/010557.html
 * Most other bindings seem to just ignore it.
 * The author of XEB is confused as to what this exactly is, and xcb-types just
 * parses it without giving an explaination. XHB doesn't even reference the
 * Alignment type generated by xcb-types.
 * I think we can safely conclude that nobody except whoever sent the patch
 * knows what this is.
 * *)
type required_start_align =
  { align  : int
  ; offset : int option }


(** Unary and binary operations. *)
type op = [ `Add | `Sub | `Mul | `Div | `Bit_and | `Bit_left_shift ]
type unop = [ `Bit_not ]


(** An expression that yields a value. *)
type expression =
  [ `Op of op * (expression * expression)
    (** Perform a binary operation on the results of the expressions. *)
  | `Unop of unop * expression
    (** Perform a unary operation on the result of the expression. *)
  | `Field_ref of string
    (** The value of another field in the same structure. *)
  | `Param_ref of string * string
    (** The value of a field in a structure that contains the current one. *)
  | `Enum_ref of string * string
    (** The value of an identifier in an enum. *)
  | `Population_count of expression
    (** The number of set bits. (e.g. 0b01101 -> 3) *)
  | `Sum_of of string * expression option
    (** Sum of the elements in a list field. *)
  | `Current_ref
    (** The current element in a Sum_of expression. *)
  | `Value of int
    (** A literal value. *)
  | `Bit of int
    (** A literal bitmask index. *)
  ]


type allowed_val =
  [ `Enum | `Alt_enum
  | `Mask | `Alt_mask ]


type 'a field_t =
  { name    : string
  ; typ     : string
  ; allowed : (allowed_val * string) option
  ; value   : 'a }


type field = unit field_t

type expr_field = (expression list) field_t

type list_field = (expression option) field_t


type field_type =
  [ `Fd of string
  | `Pad of padding
  | `Field of field
  | `List of list_field
  | `Required_start_align of required_start_align
  | `Doc ]


type case_type =
  [ `Bit
  (** In a switch, the contained fields are included if
    * switch expression & bitcase expression != 0 *)
  | `Int
  (** In a switch, the contained fields are included if
    * switch expression == case expression *)
  ]

(** A field that uses an expression to determine which fields are included in
 * the enclosing struct. *)
type switch =
  { name : string
  ; expr : expression
  ; align : required_start_align option
  ; cases : case list }

(** Essentially an if statement which uses an operation that takes the
 * switch expression and the case expression, and includes the fields it
 * contains in the switch if it's true.
 * If there are multiple expressions, they should be chained with ORs. *)
and case =
  { typ : case_type
  ; name_c : string option
  ; exprs : expression list
  ; align_c : required_start_align option
  ; fields : field_type list
  ; switch : switch option }


type x_struct =
  { name : string
  ; fields : field_type list
  ; switch : switch option }


type event_type_selector =
  { extension : string
  ; xge : bool
  ; opcode_min : int
  ; opcode_max : int }


type event_struct =
  { name : string
  ; allowed : event_type_selector list }


type field_or_expr_field =
  [ `Field of field_type
  | `Expr_field of expr_field ]


type request_children =
  { fields : field_or_expr_field list
  (*
  ; align : required_start_align option
  *)
  ; switch : switch option }


type struct_contents =
  { fields : field_type list
  (*
  ; align : required_start_align option
  *)
  ; switch : switch option }


type request =
  { name : string
  ; opcode : int
  ; combine_adjacent : bool
  ; children : request_children
  ; reply : struct_contents option }


type error =
  { name : string
  ; number : int
  (*
  ; align : required_start_align option
  *)
  ; fields : field_type list }


type event =
  { name : string
  ; code : int
  ; no_sequence_number : bool
  ; xge : bool
  (*
  ; align : required_start_align option
  *)
  ; fields : field_type list }


type enum_bitmask =
  { bits : (string * int) list; vals : (string * int) list }

type enum =
  [ `Bitmask of enum_bitmask
  | `Enum of (string * int) list ]



(* Base types and their mappings:
 * CARD8  -> u8
 * CARD16 -> u16
 * CARD32 -> u32
 * CARD64 -> u64
 * INT8   -> i8
 * INT16  -> i16
 * INT32  -> i32
 * INT64  -> i64
 * BYTE   -> char/byte
 * BOOL   -> boolean
 *)


type declaration =
  [ `Import of string
  (** Let an extension reference the types declared in another extension.
   * The string is the same as the "name" field of the extension info of the
   * referenced module.
   * The documentation states that types from xproto are implicitly imported,
   * but import declarations for xproto are still present in most of the
   * extensions, and the release notes for 1.0RC2 mention that the code
   * generator stopped importing xproto implicitly.
   * I'm not sure which source is to be trusted. *)

  | `X_id of string
  (** Declare a type alias to u32 representing a generic X resource ID. *)

  | `Type_alias of string * string
  (** Alias a type to a new name. *)

  | `Event_alias of string * (string * int)
  (** Alias an event with a new event name and number. *)

  | `Error_alias of string * (string * int)
  (** Alias an error with a new error name and number. *)

  | `X_id_union of string * string list
  (** Declare an union type of X resources, which should be valid XIDs. *)

  | `Enum of string * enum
  (** Enum like in C. By default the values start at 0 and increase by 1.
   * These are used both for bitmasks and enums that represent values.
   * Straight enums are easy to handle: they're simply a mapping from a
   * constant to an int.
   * Bitmasks can have both "bit" and "value" items, where the bit items
   * are the 0-based position of the bit in the bitmask (e.g. 0 -> 0b01,
   * 1 -> 0b10, etc) and value items are useful predefined values (e.g.
   * 0 -> empty, 15 -> all flags). *)

  | `Event of event
  (*  *)

  | `Struct of x_struct
  | `Event_struct of event_struct
  | `Union of x_struct
  | `Request of request
  | `Error of error
  ]


(** Extension information containing their name and version.
 * This is not present in the xproto file containing the core protocol
 * specification. *)
type extension_info =
  { file : string
  (* The filename of the extension's XML description file. *)

  ; name : string
  (* The extension name in camelcase. *)

  ; xname : string
  (* Name used by QueryExtension whether the extension is supported on a
   * given server. *)

  ; multiword : bool
  (* Whether the resulting C function name prefixes should be composed of
   * a single alphanumeric string, or multiple strings separated by _.
   * I'm nor sure why they added this flag instead of just using an
   * attribute to define the exact C name prefix. *)

  ; version : int * int
  (* major * minor version *)
  }

type pad_type =
  [ `Bytes | `Align ]
  (** Padding bytes in a packet *)

type padding =
  { typ : pad_type
  ; amount : int
  ; serialize : bool }


type required_start_align =
  { align  : int
  ; offset : int option }


type op =
  [ `Add | `Sub | `Mul | `Div | `Bit_and | `Bit_left_shift ]


type unop =
  [ `Bit_not ]


type expression =
  [ `Op of op * (expression * expression)
  | `Unop of unop * expression
  | `Field_ref of string
  | `Param_ref of string * string
  | `Enum_ref of string * string
  | `Pop_count of expression
  | `Sum_of of string * expression option
  | `List_element_ref
    (* What is this??? *)
  | `Value of int
  | `Bit of int ]


type allowed_vals =
  [ `Enum | `Alt_enum
  | `Mask | `Alt_mask ]


type field =
  { name : string
  ; typ  : string
  ; allowed : (allowed_vals * string) option }


type expr_field =
  { name : string
  ; typ  : string
  ; allowed : (allowed_vals * string) option
  ; children : expression list }


type list_var =
  { name : string
  ; typ  : string
  ; allowed : (allowed_vals * string) option
  ; children : expression option }


type field_type =
  [ `Fd of string
  | `Pad of padding
  | `Field of field
  | `List of list_var
  | `Required_start_align of required_start_align
  | `Doc ]


type case_type = [ `Bit | `Int ]


type case =
  { typ : case_type
  ; expressions : expression list
  ; fields : field_type list
  ; switches : switch list
  ; name_ : string option }

and switch =
  { name : string
  ; expr : expression
  ; align : required_start_align option
  ; cases : case list }


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
  ; switch : switch option }


type struct_contents =
  { fields : field_type list
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
  ; fields : field_type list }


type event =
  { name : string
  ; number : int
  ; no_sequence_number : bool
  ; xge : bool
  ; fields : field_type list }


type enum =
  [ `Bitmask of (string * int) list * (string * int) list
  (** The first list contains the "value" items, the second the "bit" items. *)
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

  | `Struct of x_struct
  (**
   *)

  | `Event_struct of event_struct
  | `Union of x_struct
  | `Request of request
  | `Event of event
  (*  *)

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
  (* Name used by QueryExtension whether the extension is supposted on a
   * given server. *)

  ; multiword : bool
  (* Whether the resulting C function name prefixes should be composed of
   * a single alphanumeric string, or multiple strings separated by _.
   * I'm nor sure why they added this flag instead of just using an
   * attribute to define the exact C name prefix. *)

  ; version : int * int
  (* major * minor version *)
  }

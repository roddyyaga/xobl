(** Documentation is not supported yet. *)
type doc = string


(** Padding bytes in a packet. *)
type pad_type =
  [ `Bytes | `Align ]

type padding =
  { typ : pad_type
  ; amount : int
  ; serialize : bool }


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
  [ `Enum
  | `Mask
  | `Alt_enum
  | `Alt_mask
  (* The documentation for these says that the values that these enums and
   * masks constrain may be within the range of the enum/mask, "but they need
   * not be". What the flying fuck does that even mean?
   * After looking at the spec files for a while, I think I have a vague idea.
   * The enums referenced provide some useful constants that have a special
   * meaning, such as "no window" being 0. That being said, this is a terrible
   * way to put it. *)
  ]


type 'a field_t =
  { name    : string
  ; typ     : string
  ; allowed : (allowed_val * string) option
  ; value   : 'a }


type field = unit field_t

type expr_field = (expression list) field_t

type list_field = (expression option) field_t


type field_type =
  [ `File_descriptor of string
  | `Pad of padding
  | `Field of field
  | `List of list_field
  | `Expr of expr_field
  (** This should only be used in a request struct description. *)
  ]


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
  { name  : string
  ; expr  : expression
  ; align : required_start_align option
  ; cases : case list }

(** Essentially an if statement which uses an operation that takes the
 * switch expression and the case expression, and includes the fields it
 * contains in the switch if it's true.
 * If there are multiple expressions, they should be chained with ORs. *)
and case =
  { typ     : case_type
  ; name_c  : string option
  ; exprs   : expression list
  ; align_c : required_start_align option
  ; fields  : field_type list
  ; switch  : switch option }


type x_struct =
  { name : string
  ; fields : field_type list
  ; switch : switch option
  (* Thanks to Xinput, we also need to handle structs with fields that may
   * or may not be there. I guess we can handle those with option types in
   * OCaml. *)
  }


(** Defines what events may be sent. *)
type allowed_events =
  { extension    : string
  (** The extension the events are defined in. *)
  ; opcode_range : int * int
  (** Only events that have opcodes within this range are allowed. *)
  }


type request_struct =
  { align  : required_start_align option
  ; fields : field_type list
  ; switch : switch option
  ; doc    : doc option }

type request =
  { name : string
  ; opcode : int
  ; combine_adjacent : bool
  ; params : request_struct
  ; reply : request_struct option }


(** A 32-byte struct. Its first byte is 0 to differentiate it from events,
 * the second contains the error code and the third and fourth contain
 * the least significant bits of the sequence number of the request. *)
type error =
  { name   : string
  ; code   : int
  (** The 8-bit code that identifies the event. *)
  ; align  : required_start_align option
  ; fields : field_type list }


(** A 32-byte struct. The first byte contains the code in the first 7 least
 * significant bits and a flag in the most significant that is set when the
 * event was generated from a SendEvent request.
 * Its third and fourth bytes contain the least significant bits of the
 * sequence number of the last request issued by the client. *)
type event =
  { name : string
  ; code : int
  (** The 7-bit code that identifies the event. *)
  ; no_sequence_number : bool
  (** A special case for the KeymapNotify event in xproto, which signals that
   * the event struct does not contain a sequence number. *)
  ; align  : required_start_align option
  ; fields : field_type list
  ; doc    : doc option }


type enum_item = string * int

type enum_bitmask =
  { bits : enum_item list
  ; vals : enum_item list }

(** Used both for bitmasks and enums that represent values. *)
type enum =
  [ `Bitmask of enum_bitmask
  (** Bitmasks are can have both "bit" and "value" items, where the bit items
   * are the 0-based position of the bit in the bitmask (e.g. 2 is 1 << 2),
   * and the value items are useful predefined values. *)
  | `Enum of enum_item list
  (** Simple mappings from constants to ints. *)
  ]



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
   * referenced module. *)
  (* The documentation states that types from xproto are implicitly imported,
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

  | `Enum of string * enum * doc
  (** Declare an enum. See the documentation for the enum type above. *)

  | `Struct of x_struct
  (** Declare a data structure. Clients are probably expected to be able to
   * both encode and decode them, but we could also analyze their usage and
   * only output code for either encoding or decoding (or none, if they're
   * never used). *)

  | `Union of x_struct
  (** Basically tagged unions. They're being phased out in favor of
   * switch fields, so we could probably try to fold them as switches into
   * the structs that use them? *)

  | `Event of event
  (** An event received by the X server. For most events we only need to
   * generate code that parses them, but for those defined in Event_structs
   * we also need to be able to create them. *)

  | `Error of error
  (** Signals the client that there was a problem with a request it sent.
   * We only need to be able to parse these correctly. *)

  | `Generic_event of event
  (** Generic events are events that don't have a fixed length. I'm pretty sure
   * they have a completely separate namespace from normal ones. *)

  | `Event_struct of string * allowed_events list
  (** SendExtensionEvent from XInput needs to be able to send events as though
   * they were received over the wire, so event structs define which events
   * the client should be able to recreate. *)
  (* Yet another extension to the spec hand-crafted for a single request
   * in XInput. I'm starting to hate XInput. *)

  | `Request of request
  (** A request the client makes to the server. Should be encoded as a function
   * that takes whatever parameters are listed in the params field.
   * Some requests send a reply back to the client. *)
  ]


(** Extension information containing their name and version.
 * This is not present in the xproto file containing the core protocol
 * specification. *)
type extension_info =
  { file : string
  (** The filename of the extension's XML description file. *)

  ; name : string
  (** The extension name in camelcase. *)

  ; xname : string
  (** Name used by QueryExtension whether the extension is supported on a
   * given server. *)

  ; multiword : bool
  (** Whether the resulting C function name prefixes should be composed of
   * a single alphanumeric string, or multiple strings separated by _. *)
  (* I'm nor sure why they added this shady flag instead of just using an
   * attribute to define the exact C name prefix. *)

  ; version : int * int
  (** major * minor version. *)
  }

type prim =
  | Card8
  | Card16
  | Card32
  | Char
  | Bool
  | Byte

type type_ =
  | Primitive of prim
  | Alias of string

type pad =
  | Pad_bytes of int
  | Pad_align of int

type binop = Multiply

type expression =
  | Value of int
  | Binop of binop * expression * expression

type struct_item =
  | Field of { type_ : string; name : string }
  | List of { type_ : string; name : string; length : string }
  | Pad of pad

type enum_item =
  | Enum_value of int
  | Enum_bit of int

type declaration =
  | Xidtype of string
  | Typedef of { old_name : string; new_name : string }
  | Struct of string * struct_item list
  | Enum of string * enum_item list


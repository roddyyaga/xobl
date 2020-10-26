type prim = Card8 | Card16 | Card32 | Char | Bool | Byte

type type_ = Primitive of prim | Alias of string

type pad = Pad_bytes of int | Pad_align of int

type binop = Multiply

type expression = Value of int | Binop of binop * expression * expression

type struct_item =
  | Field of { type_ : string; name : string }
  | List of { type_ : string; name : string; length : string }
  | Pad of pad

type enum_item = Enum_value of int | Enum_bit of int

type declaration =
  | Xidtype of string
  | Typedef of { old_name : string; new_name : string }
  | Struct of string * struct_item list
  | Enum of string * enum_item list

module Attr = Patche.Attr
open Patche.Xml
open Patche.Lazy_list_patche
open Patche.Lazy_list_patche.Infix

let import =
  let& import = el_b "import" data in
  return (`Import import)

let xidtype =
  let& name = el_a "xidtype" Attr.(run (str "name")) in
  return name

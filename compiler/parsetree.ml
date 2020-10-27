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

let import = el_b "import" data => fun import -> `Import import

let xidtype =
  el_a "xidtype" Attr.(run (str "name")) => fun name -> `Xidtype name

let xidunion =
  el_ab "xidunion" Attr.(run (str "name")) (many (el_b "type" data))
  => fun x -> `Xidunion x

let typedef =
  el_a "typedef" Attr.(run (tuple2 (str "newname") (str "oldname")))
  => fun x -> `Typedef x

let allowed_eventstruct =
  let open Attr in
  map4
    (fun a b c d -> `Allowed_eventstruct (a, b, c, d))
    (str "extension") (bool "xge") (int "opcode-min") (int "opcode-max")

let eventstruct =
  el_ab "eventstruct"
    Attr.(run (str "name"))
    (many (el_a "allowed" (Attr.run allowed_eventstruct)))
  => fun x -> `Event_struct x

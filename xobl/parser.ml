open Patche
open Patche.Xml
open Patche.Infix

type expression =
  [ `Binop of string * expression * expression
  | `Unop of string * expression
  | `Fieldref of string
  | `Paramref of string * string
  | `Enumref of string * string
  | `Popcount of expression
  | `Sumof of string * expression option
  | `Listelement_ref
  | `Value of int
  | `Bit of int
  ]

let mk_binop (op, (e1, e2)) = `Binop (op, e1, e2)
let mk_unop (op, e) = `Unop (op, e)
let mk_fieldref n = `Fieldref n
let mk_paramref (t, n) = `Paramref (n, t)
let mk_enumref (n, e) = `Enumref (n, e)
let mk_value v = `Value v
let mk_sumof (n, e) = `Sumof (n, e)
let mk_bit b = `Bit b
let mk_popcount e = `Popcount e

let mk_expression e = `Expression e

type pad = [ `Bytes of int | `Align of int ]

let pad_bytes v = `Bytes v
let pad_align v = `Align v

type allowed =
  [ `Enum of string
  | `Mask of string
  | `Altenum of string
  | `Altmask of string ]

let mk_allowed_enum e = `Enum e
let mk_allowed_mask m = `Mask m
let mk_allowed_altenum e = `Altenum e
let mk_allowed_altmask m = `Altmask m

type field =
  [ `Fd of string
  | `Pad of bool * pad
  | `Field of string * string * allowed option
  | `List of string * string * allowed option * expression
  ]

let mk_fd name = `Fd name
let mk_pad (serialize, b_a) = `Pad (serialize, b_a)
let mk_field (name, typ, allowed) = `Field (name, typ, allowed)
let mk_list ((name, typ, allowed), expression) =
  `List (name, typ, allowed, expression)

type toplevel =
  [ `Import of string
  | `Xidtype of string
  | `Xidunion of string * string list
  | `Typedef of string * string
  | `Enum of string * (string * int64) list
  | `Eventcopy of string * int * string
  | `Errorcopy of string * int * string
  | `Eventstruct of string * (string * bool * (int * int)) list
  | `Union of string * field list
  ]

let mk_import i = `Import i
let mk_xidtype t = `Xidtype t
let mk_xidunion (name, union) = `Xidunion (name, union)
let mk_typedef (new_name, old_name) = `Typedef (new_name, old_name)
let mk_eventcopy (new_name, number, old_name) =
  `Eventcopy (new_name, number, old_name)
let mk_errorcopy (new_name, number, old_name) =
  `Errorcopy (new_name, number, old_name)
let mk_allowed_eventstruct ext xge min max = ext, xge, (min, max)
let mk_eventstruct (name, allowed) = `Eventstruct (name, allowed)
let mk_enum (name, items) = `Enum (name, items)
let mk_union (name, fields) = `Union (name, fields)

type extension_info =
  { name : string
  ; file_name : string
  ; query_name : string
  ; multiword : bool
  ; version : int * int }

type protocol_file =
  | Core of toplevel list
  | Extension of extension_info * toplevel list

let mk_core d = Core d
let mk_extension (info, d) = Extension (info, d)

let import =
  let& import = el "import" data in
  return (`Import import)

let xidtype =
  let& name = el_empty "xidtype" Attr.(return (str "name")) in
  return (`Xidtype name)

let xidunion =
  el_attr "xidunion" Attr.(return (str "name")) (many (el "type" data))
  => mk_xidunion

let typedef =
  el_empty "typedef" Attr.(tuple2 (str "newname") (str "oldname"))
  => mk_typedef

let try_parse_int s =
  int_of_string_opt s
  |> Option.to_result ~none:"failed to parse int"

let try_parse_int64 s =
  Int64.of_string_opt s
  |> Option.to_result ~none:"failed to parse int64"

let enum =
  el_attr "enum" Attr.(return (str "name"))
    (many (el_attr "item" Attr.(return (str "name"))
      (el "value" data |> pipe_result try_parse_int64)
    ))
  => mk_enum

let copy name =
  el_empty name Attr.(tuple3 (str "name") (int "number") (str "ref"))

let eventcopy =
  copy "eventcopy" => mk_eventcopy

let errorcopy =
  copy "errorcopy" => mk_errorcopy

let eventstruct =
  el_attr "eventstruct" Attr.(return (str "name"))
    (many (el_empty "allowed" Attr.(map4 mk_allowed_eventstruct
      (str "extension") (bool "xge")
      (int "opcode-min") (int "opcode-max"))
    ))
  => mk_eventstruct

let expression = fix @@ fun expression ->
  let binop =
    el_attr "op" Attr.(return (str "op")) (expression &>>& expression)
    => mk_binop
  in
  let unop =
    el_attr "unop" Attr.(return (str "op")) expression
    => mk_unop
  in
  let fieldref =
    el "fieldref" data => mk_fieldref
  in
  let paramref =
    el_attr "paramref" Attr.(return (str "type")) data
    => mk_paramref
  in
  let enumref =
    el_attr "enumref" Attr.(return (str "ref")) data
    => mk_enumref
  in
  let popcount =
    el "popcount" expression => mk_popcount
  in
  let sumof =
    el_attr "sumof" Attr.(return (str "ref")) (opt expression)
    => mk_sumof
  in
  let listelement_ref =
    el_unit "listelement-ref"|> discard_with `Listelement_ref
  in
  let value =
    el "value" data |> pipe_result try_parse_int => mk_value
  in
  let bit =
    el "bit" data |> pipe_result try_parse_int => mk_value
  in
  choice
    [ binop
    ; unop
    ; fieldref
    ; paramref
    ; enumref
    ; popcount
    ; sumof
    ; listelement_ref
    ; value
    ; bit
    ]

let field =
  let pad_attrs =
    let open Attr in
    let pad = (int "bytes" => pad_bytes) <|> (int "align" => pad_align) in
    tuple2 (bool_f "serialize") pad
  in
  let allowed =
    let open Attr in
    (str "enum" => mk_allowed_enum) <|>
    (str "mask" => mk_allowed_mask) <|>
    (str "altenum" => mk_allowed_altenum) <|>
    (str "altmask" => mk_allowed_altmask)
    |> or_ None
  in
  let field_attrs = Attr.(tuple3 (str "name") (str "type") allowed) in
  choice
    [ el_empty "fd" Attr.(return (str "name")) => mk_fd
    ; el_empty "pad" pad_attrs => mk_pad
    ; el_empty "field" field_attrs => mk_field
    ; el_attr "list" field_attrs expression => mk_list
    ]

let union =
  el_attr "union" Attr.(return (str "name")) (many field)
  => mk_union

let declaration =
  choice
    [ import
    ; xidtype
    ; xidunion
    ; typedef
    ; enum
    ; eventcopy
    ; errorcopy
    ; eventstruct
    ; union
    ]

let core =
  let attrs = Attr.(return (str "header")) in
  let xcb_xproto = el_start "xcb" attrs |> satisfies (( = ) "xproto") in
  xcb_xproto >>& many declaration &>> el_end
  => mk_core

let extension =
  let attrs =
    let mk_extension_info name file_name query_name multiword major minor =
      let version = major, minor in
      { name; file_name; query_name; multiword; version }
    in
    Attr.map6 mk_extension_info
      (Attr.str "extension-name")
      (Attr.str "header")
      (Attr.str "extension-xname")
      (Attr.bool_f "extension_multiword")
      (Attr.int "major-version")
      (Attr.int "minor-version")
  in
  el_start "xcb" attrs &>>& many declaration &>> el_end
  => mk_extension

let x =
  dtd >>& (core <|> extension) &>> eoi

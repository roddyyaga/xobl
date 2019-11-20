open Patche
open Patche.Xml

type expression =
  [ `Unop of string * expression
  | `Value of int ]

let mk_unop (op, e) = `Unop (op, e)
let mk_value v = `Value v

let mk_expression e = `Expression e

type toplevel =
  [ `Import of string
  | `Xidtype of string
  | `Xidunion of string * string list
  | `Typedef of string * string
  | `Enum of string * (string * int64) list
  | `Eventcopy of string * int * string
  | `Errorcopy of string * int * string
  | `Eventstruct of string * (string * bool * (int * int)) list
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
  el "import" data
  => mk_import

let xidtype =
  el_empty "xidtype" Attr.(return (str "name"))
  => mk_xidtype

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
  let unop =
    el_attr "unop" Attr.(return (str "op")) expression
    => mk_unop
  in
  let value =
    el "value" data |> pipe_result try_parse_int => mk_value
  in
  choice [ value; unop ]

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
    ]

let core =
  let attrs = Attr.(return (str "header")) in
  let xcb_xproto = el_start "xcb" attrs |> satisfies (( = ) "xproto") in
  xcb_xproto >>& many declaration &>> el_end
  => mk_core

let extension =
  let attrs =
    let extension_info name file_name query_name multiword major minor =
      let version = major, minor in
      { name; file_name; query_name; multiword; version }
    in
    Attr.map6 extension_info
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
  dtd >>& core <|> extension &>> eoi

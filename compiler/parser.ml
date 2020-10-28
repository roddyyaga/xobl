let mk_import import = `Import import

let mk_xidunion (name, types) = `Xidunion (name, types)

let mk_xidtype name = `Xidtype name

let mk_typedef (new_name, old_name) = `Typedef (new_name, old_name)

let mk_allowed_event extension xge opcode_min opcode_max =
  `Allowed_eventstruct (extension, xge, opcode_min, opcode_max)

let mk_extension_info name file_name query_name multiword major minor =
  (name, file_name, query_name, multiword, major, minor)

let mk_core decls = `A decls

let mk_extension x = `B x

let try_parse_int s =
  int_of_string_opt s |> Option.to_result ~none:"failed to parse int"

let try_parse_int64 s =
  Int64.of_string_opt s |> Option.to_result ~none:"failed to parse int64"

open Patche
open Patche.Xml
open Patche.Infix

let import = el_b "import" data

let xidtype = el_a "xidtype" (Attr.str "name")

let xidunion = el_ab "xidunion" Attr.(str "name") (many (el_b "type" data))

let typedef = el_a "typedef" Attr.(tuple2 (str "newname") (str "oldname"))

let eventstruct =
  el_ab "eventstruct" (Attr.str "name")
    (many
       (el_a "allowed"
          (map4 mk_allowed_event (Attr.str "extension") (Attr.bool "xge")
             (Attr.int "opcode-min") (Attr.int "opcode-max"))))

let doc =
  el_discard "doc" |> discard_with (`Docs "docs parsing not implemented")

let inp str =
  let i =
    Xmlm.make_input ~strip:true (`String (0, str)) |> Lazy_list.of_xml_input
  in
  match Lazy.force i with
  | Lazy_list.Nil ->
      failwith "what"
  | Lazy_list.Cons (_, rest) ->
      rest

let%test "doc" =
  run doc (inp "<doc>a</doc>") = Ok (`Docs "docs parsing not implemented")

(*
let%test "doc 2" =
  run doc (inp "<doc></doc>") = Ok (`Docs "docs parsing not implemented")
  *)

let enum_item =
  let value_item = el_b "value" data |> pipe_result try_parse_int64 in
  let bit_item = el_b "bit" data |> pipe_result try_parse_int in
  (value_item ->> fun value -> `Value value)
  <|> bit_item ->> fun bit -> `Bit bit

let enum =
  el_ab "enum" (Attr.str "name")
    (many (el_ab "item" (Attr.str "name") enum_item) *<> opt doc)

let copy name = el_a name Attr.(tuple3 (str "name") (str "ref") (int "number"))

let eventcopy = copy "eventcopy"

let errorcopy = copy "errorcopy"

let declaration = choice [el "a"]

let core =
  let attrs = Attr.str "header" |> satisfies (( = ) "xproto") in
  el_ab "xcb" attrs (many declaration) ->> snd

let extension =
  let attrs =
    let open Attr in
    map6 mk_extension_info (str "extension-name") (str "header")
      (str "extension-xname")
      (bool "extension_multiword" ~default:false)
      (int "major-version") (int "minor-version")
  in
  el_ab "xcb" attrs (many declaration)

let xcb = dtd *> (core ->> mk_core <|> extension ->> mk_extension) *< eoi

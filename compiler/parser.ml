open Parsetree
open Patche
open Patche.Xml
open Patche.Infix

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

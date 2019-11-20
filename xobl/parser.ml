open Patche
open Patche.Xml

type toplevel =
  [ `Import of string
  | `Xidtype of string
  | `Xidunion of string * string list
  | `Typedef of string * string
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

type extension_info =
  { name : string
  ; file_name : string
  ; query_name : string
  ; multiword : bool
  ; version : int * int }

type protocol_file =
  | Core of toplevel list
  | Extension of extension_info * toplevel list

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

let declaration =
  import <|> xidtype <|> xidunion <|> typedef
  <|> eventcopy <|> errorcopy <|> eventstruct

let core =
  let attrs = Attr.(return (str "header")) in
  let xcb_xproto = el_start "xcb" attrs |> satisfies (( = ) "xproto") in
  xcb_xproto >>& many declaration &>> el_end
  |> pipe (fun d -> Core d)

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
  |> pipe (fun (info, decls) -> Extension (info, decls))

let x =
  dtd >>& core <|> extension &>> eoi

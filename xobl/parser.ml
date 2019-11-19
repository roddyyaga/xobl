open Patche
open Patche.Xml

type toplevel =
  [ `Import of string
  | `Xidtype of string
  | `Xidunion of string * string list
  | `Typedef of string * string
  | `Eventcopy of string * int * string ]

let mk_import i = `Import i
let mk_xidtype t = `Xidtype t

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
  %> mk_import

let xidtype =
  el_empty "xidtype" Attr.(return (str "name"))
  %> mk_xidtype

let xidunion =
  let typ = el_start_empty "type" >>& data &>> el_end in
  el_attr "xidunion" Attr.(return (str "name")) (many typ)
  %> (fun (name, union) -> `Xidunion (name, union))

let typedef =
  el_start "typedef" Attr.(tuple2 (str "newname") (str "oldname")) &>> el_end
  |> pipe (fun (new_name, old_name) -> `Typedef (new_name, old_name))

let eventcopy =
  el_start "eventcopy" Attr.(tuple3 (str "name") (int "number") (str "ref"))
  &>> el_end
  |> pipe
    (fun (new_name, number, old_name) ->
      `Eventcopy (new_name, number, old_name))

let declaration =
  import <|> xidtype <|> xidunion <|> typedef <|> eventcopy

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
  (*
  dtd >>& el_start_empty "xcb" >>& many (import <|> xidtype) &>> el_end
  *)

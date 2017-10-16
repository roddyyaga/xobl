type declaration = 
  [ `Import of string
  (* Import types declared in another file *)

  | `Type_alias of string * string
  (* Alias a type to a new name *)

  | `Event_alias of string * (string * int)
  (* Alias an event with a new event name and number *)

  | `Error_alias of string * (string * int)
  (* Alias an error with a new error name and number *)

  | `X_id of string
  (* Generic X resource ID
   * implemented with an uint32_t in C *)

  | `X_id_union of string * string list
  (* Union of X resource IDs *)

  | `Enum of string (* * string list *)
  (* Enumeration type, where default is 0 for the first
   * and one more than the previous for all the others *)

  | `Struct of string (* * *)
  | `Event_struct of string
  | `Union of string
  | `Request of string
  | `Event of string
  | `Error of string
  ]


type extension =
  { file : string
  (* the filename of the extension's XML description file *)

  ; name : string
  (* extension name (in CamelCase) *)

  ; xname : string
  (* name used by QueryExtension check for the presence of the extension *)

  ; multiword : bool
  (* whether the resulting C prefix should be composed
   * of a single or multiple words separated by underscores. *)

  ; version : int * int
  (* major * minor version *)

  ; declarations : declaration list }


let print_declaration x =
  let t, name =
    match x with
    | `Import file ->
        "import", file
    | `Type_alias (old, nw) ->
        "type-alias", old ^ " -> " ^ nw
    | `Event_alias (old, (nw, num)) ->
        "event-alias", old ^ " -> " ^ nw ^ "(" ^ string_of_int num ^ ")"
    | `Error_alias (old, (nw, num)) ->
        "error-alias", old ^ " -> " ^ nw ^ "(" ^ string_of_int num ^ ")"
    | `X_id name ->
        "X-id", name
    | `X_id_union (name, types) ->
        "X-id-union", name ^ " -> " ^ String.concat " | " types
    | `Enum name ->
        "enum", name
    | `Struct name ->
        "struct", name
    | `Event_struct name ->
        "event-struct", name
    | `Union name ->
        "union", name
    | `Request name ->
        "request", name
    | `Event name ->
        "event", name
    | `Error name ->
        "error", name
  in
  print_string t;
  print_char ' ';
  print_string name





let node_text = function
  | Xml.PCData txt :: [] -> txt
  | _ -> failwith "no data found"

let declaration_of_xml =
  let failure = Failure "unrecognized element" in
  function
  | Xml.PCData _ ->
      raise failure
  | Xml.Element (name, attrs, children) ->
      let get_attr x = List.assoc x attrs in
      match name with
      | "import" ->
          `Import (node_text children)

      | "typedef" ->
          let new_name = get_attr "newname" in
          let old_name = get_attr "oldname" in
          `Type_alias (old_name, new_name)

      | "eventcopy" ->
          let new_name = get_attr "name" in
          let old_name = get_attr "ref" in
          let number = int_of_string @@ get_attr "number" in
          `Event_alias (old_name, (new_name, number))

      | "errorcopy" ->
          let new_name = get_attr "name" in
          let old_name = get_attr "ref" in
          let number = int_of_string @@ get_attr "number" in
          `Error_alias (old_name, (new_name, number))

      | "xidtype" ->
          let id = get_attr "name" in
          `X_id id

      | "xidunion" ->
          let id = get_attr "name" in
          let get_type = function
            | Xml.Element ("type", [], [Xml.PCData name]) -> name
            | _ -> failwith "unrecognized element in X_id_union" in
          let types = List.map get_type children in
          `X_id_union (id, types)

      | "enum" ->
          let name = get_attr "name" in
          `Enum name
      | "struct" ->
          let name = get_attr "name" in
          `Struct name
      | "eventstruct" ->
          let name = get_attr "name" in
          `Event_struct name
      | "union" ->
          let name = get_attr "name" in
          `Union name
      | "request" ->
          let name = get_attr "name" in
          `Request name
      | "event" ->
          let name = get_attr "name" in
          `Event name
      | "error" ->
          let name = get_attr "name" in
          `Error name
      | _ ->
          raise failure


let extension_of_xml =
  let failure = Failure "not an xcb root element" in
  function
  | Xml.Element ("xcb", attrs, children) ->
      begin try
        let get_attr x = List.assoc x attrs in
        let version =
          let major = int_of_string @@ get_attr "major-version" in
          let minor = int_of_string @@ get_attr "minor-version" in
          major, minor in
        let multiword =
          match List.assoc_opt "extension-multiword" attrs with
          | Some x -> bool_of_string x | None -> false in
        let file = get_attr "header" in
        let xname = get_attr "extension-xname" in
        let name = get_attr "extension-name" in
        let declarations = List.map declaration_of_xml children in
        { file; name; xname; multiword; version; declarations }
      with Not_found -> raise failure end
  | Xml.PCData _ | Xml.Element _ ->
      raise failure


let xproto = function
  | Xml.Element ("xcb", ["header", "xproto"], children) ->
      List.map declaration_of_xml children
  | Xml.PCData _ | Xml.Element _ ->
      failwith "not the core protocol specification"


let is_xproto = function
  | Xml.Element ("xcb", ["header", "xproto"], children) -> true
  | Xml.PCData _ | Xml.Element _ -> false


let () =
  let file = Xml.parse_file Sys.argv.(1) in
  let decls =
    if is_xproto file then
      xproto file
    else 
      let ext = extension_of_xml file in
      ext.declarations in
  List.iter (fun x -> print_declaration x; print_newline ()) decls

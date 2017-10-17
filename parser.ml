type response =
  string * int


type declaration = 
  [ `Import of string
  (* Import types declared in another file *)

  | `Type_alias of string * string
  (* Alias a type to a new name *)

  | `Event_alias of string * response
  (* Alias an event with a new event name and number *)

  | `Error_alias of string * response
  (* Alias an error with a new error name and number *)

  | `X_id of string
  (* Generic X resource ID
   * implemented with an uint32_t in C *)

  | `X_id_union of string * string list
  (* Union of X resource IDs *)

  | `Enum of string * (string * int) list
  (* Enumeration type, where default is 0 for the first
   * and one more than the previous for all the others *)

  | `Struct of string (* * *)
  | `Event_struct of string
  | `Union of string
  | `Request of string * int * bool
  | `Event of response * bool * bool
  | `Error of response
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


let app_with_default f x = function
  | Some a -> f a | None -> x


let print_declaration x =
  let t, name =
    match x with
    | `Import file ->
        "import", file
    | `Type_alias (old, nw) ->
        "type-alias", nw ^ " = " ^ old
    | `Event_alias (old, (nw, num)) ->
        "event-alias",  nw ^ "(" ^ string_of_int num ^ ") = " ^ old
    | `Error_alias (old, (nw, num)) ->
        "error-alias", nw ^ "(" ^ string_of_int num ^ ") = " ^ old
    | `X_id name ->
        "X-id", name
    | `X_id_union (name, types) ->
        "X-id-union", name ^ " = " ^ String.concat " | " types
    | `Enum (name, items) ->
        let to_s (name, n) = name  ^ "(" ^ string_of_int n ^ ")" in
        "enum", name ^ " = " ^ String.concat " | " (List.map to_s items)
    | `Struct name ->
        "struct", name
    | `Event_struct name ->
        "event-struct", name
    | `Union name ->
        "union", name
    | `Request (name, num, _) ->
        "request", name ^ "(" ^ string_of_int num ^ ")"
    | `Event ((name, num), _, _) ->
        "event", name ^ "(" ^ string_of_int num ^ ")"
    | `Error (name, num) ->
        "error", name ^ "(" ^ string_of_int num ^ ")"
  in
  print_string t;
  print_char ' ';
  print_string name


let node_text = function
  | Xml.PCData txt :: [] -> txt
  | _ -> failwith "no data found"


type field =
  [ `Fd of string
  | `Pad of int option * int option * bool
  ]


let parse_field = function
  | Xml.Element ("fd", ["name", name], []) ->
      let _ = `Fd name in
      ()
  | Xml.Element ("pad", attrs, []) ->
      let get_attr x = List.assoc_opt x attrs in
      let bytes = get_attr "bytes" in
      let align = get_attr "align" in
      let serialize = match get_attr "serialize" with
        | Some x -> bool_of_string x | None -> false in
      let _ = `Pad (bytes, align, serialize) in
      ()
  | Xml.Element ("field", attrs, [])
  | Xml.Element ("required_start_align", attrs, []) ->
      ()
  | Xml.Element ("list", attrs, children) ->
      ()
  | Xml.Element ("switch", ["name", name], children) ->
      ()
  | Xml.Element ("doc", _, _) ->
      ()
  | Xml.Element _ | Xml.PCData _ ->
      failwith "unrecognized element"


let declaration_of_xml =
  let failure = Failure "unrecognized element" in
  let open Xml in
  function PCData _ -> raise failure | Element x ->
  match x with
  | "import", [], [PCData file] ->
      `Import file

  | "xidtype", ["name", id], [] ->
      `X_id id

  | "xidunion", ["name", id], children ->
      let get_type = function
        | Element ("type", [], [PCData name]) -> name
        | _ -> failwith "unrecognized element in X_id_union" in
      let types = List.map get_type children in
      `X_id_union (id, types)

  | "enum", ["name", name], children ->
      let rec loop acc last_value = function
        | Element ("doc", _, _) :: rst ->
            loop acc last_value rst
        | Element ("item", ["name", name], value) :: rst ->
            let value = match value with
              | Element (_, [], [PCData value]) :: [] ->
                  int_of_string value
              | [] -> last_value + 1
              | _ -> failwith "unrecognized element in Enum" in
            loop ((name, value) :: acc) value rst
        | [] -> List.rev acc
        | _ -> failwith "unrecognized element in Enum" in
      let items = loop [] (-1) children in
      `Enum (name, items)

  | "struct", ["name", name], children ->
      List.iter parse_field children;
      `Struct name

  | "eventstruct", ["name", name], children ->
      `Event_struct name

  | "union", ["name", name], children ->
      `Union name

  | "typedef", ["oldname", old_name; "newname", new_name], []
  | "typedef", ["newname", new_name; "oldname", old_name], [] ->
      `Type_alias (old_name, new_name)

  | name, attrs, children ->
      let get_attr x = List.assoc x attrs in
      let get_attr_opt x = List.assoc_opt x attrs in
      match name with
      | "event" ->
          let name = get_attr "name" in
          let num = int_of_string @@ get_attr "number" in
          let seq = app_with_default bool_of_string false
            (get_attr_opt "no-sequence-number") in
          let xge = app_with_default bool_of_string false
            (get_attr_opt "xge") in
          let _ = List.map parse_field children in
          `Event ((name, num), seq, xge)

      | "error" ->
          let name = get_attr "name" in
          let num = int_of_string @@ get_attr "number" in
          let _ = List.map parse_field children in
          `Error (name, num)

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

      | "request" ->
          let name = get_attr "name" in
          let opcode = int_of_string @@ get_attr "opcode" in
          let combine_adjacent = app_with_default bool_of_string false
            @@ get_attr_opt "combine-adjacent" in
          `Request (name, opcode, combine_adjacent)

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

let string_split chr str =
  let len = String.length str in
  let rec pos i =
    if i >= len then
      None
    else if str.[i] = chr then
      let left = StringLabels.sub str ~pos:0 ~len:i in
      let right = StringLabels.sub str ~pos:(i + 1) ~len:(len - i - 1) in
      Some (left, right)
    else
      pos (i + 1)
  in pos 0


type 'a table =
  { name : string
  ; ext  : string
  ; mutable refs : 'a }

let add_to_table tbl ext name refs =
  tbl := { ext; name; refs } :: !tbl


module Lookup = struct
  (* Helpers *)
  let by name ext_fname t =
    t.name = name && t.ext = ext_fname

  let find_opt tbl name ext_fname =
    List.find_opt (by name ext_fname) !tbl
    |> CCOpt.map (fun item -> (ext_fname, name, item))

  let find tbl name ext_fname =
    match find_opt tbl name ext_fname with
    | Some i -> i
    | None -> Format.ksprintf invalid_arg "%s:%s" ext_fname name


  let find_qualified tbl name =
    string_split ':' name
    |> CCOpt.map (fun (ext_fname, name) ->
      find tbl name ext_fname
    )

  let find_in_ext tbl name ext =
    find_opt tbl name ext.Types.file_name

  let find_in_imports tbl name ext =
    ext.Types.imports
    |> CCList.find_map (find_opt tbl name)


  let find_name tbl name ext =
    find_qualified tbl name
    |> CCOpt.or_lazy ~else_:(fun () ->
      find_in_ext tbl name ext
    )
    |> CCOpt.or_lazy ~else_:(fun () ->
      find_in_imports tbl name ext
    )
    |> CCOpt.get_lazy (fun () ->
      Format.ksprintf invalid_arg "couldn't find name: %s" name
    )

  let lookup tbl (update : 'a -> 'a) used_ext name =
    let ext, name, item =
      find_name tbl name used_ext
    in
    item.refs <- update item.refs;
    if used_ext.Types.file_name = ext then
      Types.Id name
    else
      Types.Ext_id (ext, name)
end


(* Types *)
let type_table : int table list ref = ref []

let lookup_type_id used_ext name =
  Lookup.lookup type_table (fun x -> x + 1) used_ext name

let lookup_type used_ext name =
  match Prim.of_string name with
  | Some p -> Types.Prim p
  | None   -> Types.Ref (lookup_type_id used_ext name)


(* Enums *)
type enum_refs = { enums : int; masks : int }

let enum_table : enum_refs table list ref = ref []

let lookup_enum ext x =
  let f { enums; masks } = match x with
    | `Enum -> { enums = enums + 1; masks }
    | `Mask -> { masks = masks + 1; enums }
  in
  Lookup.lookup enum_table f ext

let enum_refs ext name =
  let e = List.find (fun x -> x.ext = ext && x.name = name) !enum_table in
  e.refs


(* Events/errors *)
let event_table : unit table list ref = ref []

let lookup_event ext =
  let f () = () in
  Lookup.lookup event_table f ext

let error_table : unit table list ref = ref []

let lookup_error ext =
  let f () = () in
  Lookup.lookup error_table f ext


type event_no =
  { ev_name   : string
  ; ev_ext    : string
  (* NOTE: for some inexplicable reason, the Parser.extension_info field
     to use here is [name] instead of [file_name]. *)
  ; ev_number : int }

let event_no_table : event_no list ref = ref []

let event_name_from_no curr_ext ext no =
  let find ev = ev.ev_ext = ext && ev.ev_number = no in
  List.find_opt find !event_no_table
  |> CCOpt.map (fun ev ->
    if curr_ext.Types.file_name = ext then
      Types.Id ev.ev_name
    else
      Types.Ext_id (ext, ev.ev_name)
  )




let init =
  List.iter (fun ext ->
    ext.Types.declarations |> List.iter (function
      | `X_id name
      | `X_id_union (name, _)
      | `Type_alias (name, _)
      | `Event_struct (name, _)
      | `Struct (name, _)
      | `Union (name, _) ->
        add_to_table type_table ext.Types.file_name name 0

      | `Enum (name, _) ->
        add_to_table enum_table ext.Types.file_name name { enums = 0; masks = 0 }

      | `Event (name, no, _) ->
        add_to_table event_table ext.Types.file_name name ();
        event_no_table := { ev_name = name; ev_ext = ext.Types.name; ev_number = no } :: !event_no_table

      | `Error (name, _, _) ->
        add_to_table error_table ext.Types.file_name name ()

      | `Generic_event (name, _, _) ->
        add_to_table event_table ext.Types.file_name name ()

      | _ -> ()
  ))

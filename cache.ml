open Util


type 'a table =
  { name : string
  ; ext  : string
  ; mutable refs : 'a }

let add_to_table tbl ext name refs =
  tbl := { ext; name; refs } :: !tbl

let lookup tbl (update : 'a -> 'a) used_ext name =
  let get name ext t = t.name = name && t.ext = ext in
  String'.split ':' name
  |> Option.map (fun (ext, name) ->
      let item = List.find_opt (get name ext) !tbl in
      let item = match item with None -> invalid_arg name | Some i -> i in
      item.refs <- update item.refs;
      if used_ext.Types.file_name = ext then
        Types.Id name
      else
        Types.Ext_id (ext, name)
    )
  |> Option.or_lazy (lazy (
      List.find_opt (get name used_ext.Types.file_name) !tbl
      |> Option.map (fun item ->
          item.refs <- update item.refs;
          Types.Id name
        )
    ))
  |> Option.with_default_lazy (lazy (
      used_ext.Types.imports |> List'.first_exn (fun ext ->
        List.find_opt (get name ext) !tbl
        |> Option.map (fun item ->
            item.refs <- update item.refs;
            Types.Ext_id (ext, name)
          )
    )))


(* Types *)
let type_table : int table list ref = ref []

let lookup_type_id used_ext name =
  lookup type_table (fun x -> x + 1) used_ext name

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
  lookup enum_table f ext

let enum_refs () =
  List.map (fun x -> x.refs) !enum_table


(* Events/errors *)
let event_table : unit table list ref = ref []

let lookup_event ext =
  let f () = () in
  lookup event_table f ext

let error_table : unit table list ref = ref []

let lookup_error ext =
  let f () = () in
  lookup error_table f ext


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
  |> Option.map (fun ev ->
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

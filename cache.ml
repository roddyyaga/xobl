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
      let item = List.find (get name ext) !tbl in
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


(* *)
let type_table : int table list ref = ref []

let lookup_type_id used_ext name =
  lookup type_table (fun x -> x + 1) used_ext name

let lookup_type used_ext name =
  match Prim.of_string name with
  | Some p -> Types.Prim p
  | None   -> Types.Ref (lookup_type_id used_ext name)


(* *)
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

      | _ -> ()
  ))

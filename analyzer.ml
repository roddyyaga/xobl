module X = Parser


let files : (string, X.protocol_file) Hashtbl.t =
  Hashtbl.create 24


let enum_usage decls =
  let enums_list =
    List.fold_left (fun acc -> function
      | X.Enum (name, _, _) -> name :: acc
      | _ -> acc)
      []
      decls
  in
  List.iter print_endline enums_list


let get_imports =
  List.fold_left (fun acc -> function
    | X.Import file -> file :: acc
    | _ -> acc) []


let file_loc = "xproto/src/"


type 'a tree =
  | Leaf of 'a
  | Tree of 'a * 'a tree list


let print_tree show =
  let print' depth s =
    print_endline (String.make (depth * 2) ' ' ^ show s)
  in
  let rec p depth = function
    | Leaf s ->
      print' depth s
    | Tree (s, ts) ->
      print' depth s;
      List.iter (p (depth + 1)) ts
  in
  p 0


let decls_of_protocol = function
  | X.Core decls | X.Extension (_, decls) ->
    decls


let rec import_tree f =
  let doc =
    match Hashtbl.find_opt files f with
    | Some d -> d
    | None ->
      let doc = Parser.parse_file (file_loc ^ f ^ ".xml") in
      Hashtbl.add files f doc;
      doc
  in
  let decls = decls_of_protocol doc in
  let imports = get_imports decls in
  match imports with
  | [] -> Leaf f
  | _  -> Tree (f, List.map import_tree imports)


let base_types =
  [ "CARD32"
  ; "CARD16"
  ; "CARD8"
  ; "INT32"
  ; "char"
  ; "float"
  ; "double"
  ]


let get_names imported (d : X.declaration list) =
  List.fold_left (fun acc -> function
    (*
    | X.Enum (name, _, _)
    *)
    | X.X_id name
    (*
    | X.X_id_union (name, _)
    *)
    | X.Struct { Parser.name; _ }
    | X.Union { Parser.name; _ } ->
      name :: acc
    | X.Type_alias (old, n) when List.mem old acc || List.mem old imported || List.mem old base_types ->
      (n ^ " (" ^ old ^ ")") :: n :: acc
    | X.Type_alias (old, n) ->
      failwith ("not found: " ^ old)
    | _ -> acc)
    [] d


let rec names = function
  | Leaf f ->
    let d = decls_of_protocol @@ Hashtbl.find files f in
    get_names [] d
  | Tree (f, ts) ->
    let imported = List.flatten @@ List.map names ts in
    let d = decls_of_protocol @@ Hashtbl.find files f in
    get_names imported d


let () =
  let files = List.tl (Array.to_list Sys.argv) in
  files |> List.iter begin fun file ->
    let t = import_tree file in
    let ns = names t in
    List.iter print_endline ns
  end



  (*
let () =
  let files = List.tl (Array.to_list Sys.argv) in
  files |> List.iter begin fun f ->
    print_endline f;
    let file = Parser.parse_file f in
    let decls = match file with
      | `Core decls -> decls
      | `Extension (_, decls) -> decls
    in
    enum_usage decls
  end
  *)

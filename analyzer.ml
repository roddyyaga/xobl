module X = Parser


let files : (string, X.protocol_file) Hashtbl.t =
  Hashtbl.create 24


let enum_usage decls =
  let enums_list =
    List.fold_left (fun acc -> function
      | `Enum (name, _, _) -> name :: acc
      | _ -> acc)
      []
      decls
  in
  List.iter print_endline enums_list


let get_imports =
  List.fold_left (fun acc -> function
    | `Import file -> file :: acc
    | _ -> acc) []


let file_loc = "xproto/src/"


type tree =
  | Leaf of string
  | Tree of string * tree list


let rec import_tree f =
  let doc =
    match Hashtbl.find_opt files f with
    | Some d -> d
    | None ->
      let doc = Parser.parse_file (file_loc ^ f ^ ".xml") in
      Hashtbl.add files f doc;
      doc
  in
  let decls = match doc with
    | `Core decls | `Extension (_, decls) ->
      decls
  in
  let imports = get_imports decls in
  match imports with
  | [] -> Leaf f
  | _  -> Tree (f, List.map import_tree imports)


let rec print_tree depth = function
  | Leaf s ->
    print_endline (String.make (depth * 2) ' ' ^ s)
  | Tree (s, ts) ->
    print_endline (String.make (depth * 2) ' ' ^ s);
    List.iter (print_tree (depth + 1)) ts


let () =
  let file = Sys.argv.(1) in
  let t = import_tree file in
  print_tree 0 t



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

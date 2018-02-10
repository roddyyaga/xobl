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


(** The base types that are actually used in the extensions.
 * There's more but we won't consider them until they're used in an extension. *)
type base_type =
  | Uint32
  | Uint16
  | Uint8
  | Int32
  | Float64
  | Float32
  | Byte
  | Struct of string
  | List of base_type
  | Alias of string

let base_type_of_string = function
  | "CARD32" -> Some Uint32
  | "CARD16" -> Some Uint16
  | "CARD8"  -> Some Uint8
  | "INT32"  -> Some Int32
  | "double" -> Some Float64
  | "float"  -> Some Float32
  | "char"   -> Some Byte
  | t        -> None


type declaration =
  | Type of string * base_type
  (** A type alias to one of the base types. *)


let mk_struct { X.fields; switch; _ } =
  fields |> List.map (function
    | `Field { X.name; typ; allowed; _ } ->
      `Field (Type (name, Alias typ))

    | `Pad { X.typ; amount; serialize } ->
      `Pad (amount)

    | `List { X.name; typ; allowed; value } ->
      `Field (Type (name, List (Alias typ)))

    | _ ->
      failwith "invalid struct field")


  (*
let base_types =
  [ Type_alias ("CARD32", Uint32)
  ; Type_alias ("CARD16", Uint16)
  ; Type_alias ("CARD8",  Uint8)
  ; Type_alias ("INT32",  Int32)
  ; Type_alias ("double", Float64)
  ; Type_alias ("float",  Float32)
  ; Type_alias ("char",   Byte) ]
*)


let get_names imported (d : X.declaration list) : declaration list =
  List.fold_left (fun acc -> function
    | X.X_id name ->
      Type (name, Uint32) :: acc

    | X.Struct { X.name; _ } ->
      Type (name, Struct name) :: acc

    | X.Type_alias (name, old) ->
      (match base_type_of_string old with
      | Some t ->
        Type (name, t) :: acc
      | None ->
        Type (name, Alias old) :: acc)

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


let files =
  [ "bigreq"
  ; "composite"
  ; "damage"
  ; "dpms"
  ; "dri2"
  ; "dri3"
  ; "ge"
  ; "glx"
  ; "present"
  ; "randr"
  ; "record"
  ; "render"
  ; "res"
  ; "screensaver"
  ; "shape"
  ; "shm"
  ; "sync"
  ; "xc_misc"
  ; "xevie"
  ; "xf86dri"
  ; "xf86vidmode"
  ; "xfixes"
  ; "xinerama"
  ; "xinput"
  ; "xkb"
  ; "xprint"
  ; "xproto"
  ; "xselinux"
  ; "xtest"
  ; "xvmc"
  ; "xv"
  ]


let () =
  (*
  let files = List.tl (Array.to_list Sys.argv) in
  *)
  files |> List.iter begin fun file ->
    let t = import_tree file in
    let ns = names t in
    ()
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

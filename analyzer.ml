module X = Parser

module S = struct
  open StdLabels
  open MoreLabels


  type x_type =
    | Byte
    | Int32
    | Uint8
    | Uint16
    | Uint32
    | Float32
    | Float64
    | Struct of string
    | List of x_type
    | Alias of string list
    | Enum of (string * int) list

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
    | Type of string * x_type
    | Import of string

  type x_module =
    { file_name : string
    ; query_extension_name : string option
    ; version : (int * int) option
    ; declarations : declaration list }


  let file_loc = "xproto/src/"

  let modules : (string, x_module) Hashtbl.t =
    Hashtbl.create 13

  let module_ids : (string, string) Hashtbl.t =
    Hashtbl.create 13


  let rec mk_module f : x_module =
    get_import (get_import_name f)

  and module_of_file = function
    | X.Extension ({ name; xname; version; _ }, d) ->
      { file_name = String.capitalize_ascii name
      ; query_extension_name = Some xname
      ; version = Some version
      ; declarations = declarations d }
    | X.Core d ->
      { file_name = "Xproto"
      ; query_extension_name = None
      ; version = None
      ; declarations = declarations d }


  and get_import_name file =
    match Hashtbl.find_opt module_ids file with
    | Some id -> id
    | None ->
      let fname = Filename.concat file_loc (file ^ ".xml") in
      let doc = module_of_file (Parser.parse_file fname) in
      Hashtbl.add module_ids ~key:file ~data:doc.file_name;
      Hashtbl.add modules ~key:doc.file_name ~data:doc;
      doc.file_name


  and get_import name =
    Hashtbl.find modules name


  and module_of_type id decls =
    let f = function
      | Type (n, _) -> n = id
      | _ -> false
    in
    if List.exists decls ~f then None else
    let rec loop = function
      | [] -> failwith id
      | Import name :: rest ->
        let m = get_import name in
        begin try
          match module_of_type id m.declarations with
          | None -> Some name
          | Some x -> Some x
        with Failure _ -> loop rest end
      | _ :: rest -> loop rest
    in
    loop decls


  and declarations d =
    let loop acc = function
      | X.Import file ->
        let name = get_import_name file in
        Import name :: acc

      | X.X_id name ->
        Type (name, Uint32) :: acc

      | X.Type_alias (new_, old) ->
        let old =
          match base_type_of_string old with
          | Some t -> t
          | None ->
            match module_of_type old acc with
            | None -> Alias [old]
            | Some m -> Alias [m; old]
        in
        Type (new_, old) :: acc

      | X.Enum (name, `Enum items, _) ->
        Type (name, Enum items) :: acc

      | _ ->
        acc
    in
    List.fold_left d ~init:[] ~f:loop


  let snake_cased name =
    if String.uppercase_ascii name = name then
      (* The string is already snake_cased, just make sure it's lowercase. *)
      String.lowercase_ascii name
    else
      (* The string is CamelCased. *)
      let buf = Buffer.create 16 in
      String.iteri name ~f:(fun i -> function
        | c when i = 0 ->
          Buffer.add_char buf (Char.lowercase_ascii c)
        | 'A' .. 'Z' as c ->
          (* We want to make sure something like GLXContext is turned into
           * glxcontext and not g_l_x_context.
           * TODO: GLXContext -> glx_context instead? *)
          let prev = name.[i - 1] in
          if Char.lowercase_ascii prev = prev then (
            Buffer.add_char buf '_';
            Buffer.add_char buf (Char.lowercase_ascii c)
          ) else
            Buffer.add_char buf (Char.lowercase_ascii c)
        | c ->
          Buffer.add_char buf c
      );
      Buffer.contents buf


  let%test_unit _ =
    let ocaml_type = function
      | Byte -> "int"
      | Int32 -> "int32"
      | Uint8 -> "int"
      | Uint16 -> "int"
      | Uint32 -> "int32"
      | Float32 -> "float"
      | Float64 -> "float"
      | Alias ls -> String.concat ls ~sep:"."
      | _ -> failwith "not implemented"
    in
    let m = mk_module "xproto" in
    let open Printf in
    printf "module %s = struct\n" m.file_name;
    begin match m.query_extension_name with None -> () | Some x ->
      printf "  let query_extension_name = %S\n" x
    end;
    begin match m.version with None -> () | Some (maj, min) ->
      printf "  let version = (%d, %d)\n" maj min
    end;
    List.iter m.declarations ~f:(function
      | Import _ -> ()
      | Type (n, Enum items) ->
        let n = snake_cased n in
        print_newline ();
        printf "  type %s = [\n" n;
        List.iter (fun (n, _) -> print_endline ("    | `" ^ n)) items;
        print_endline "  ]";
        printf "  let val_of_%s : %s -> int32 = function\n" n n;
        List.iter (fun (n, i) -> printf "    | `%s -> %dl\n" n i) items
      | Type (n, o) ->
        let n = snake_cased n in
        print_newline ();
        printf "  type %s = %s\n" n (ocaml_type o)
    );
    print_endline "end"
end


(*

let files : (string, X.protocol_file) Hashtbl.t =
  Hashtbl.create 13


let print_enum : X.enum -> unit = function
  | `Bitmask { bits; vals } ->
      print_endline "BITMASK";
      List.iter (fun (n, v) -> print_endline ("VAL " ^ n ^ " : " ^ string_of_int v)) vals;
      List.iter (fun (n, v) -> print_endline ("BIT " ^ n ^ " : " ^ string_of_int v)) bits
  | `Enum items ->
      print_endline "ENUM";
      List.iter (fun (n, v) -> print_endline (n ^ " : " ^ string_of_int v)) items


let enum_usage =
  List.iter (function
    | X.Enum (name, e, _) ->
        print_endline name;
        print_enum e;
        print_newline ()
    | _ -> ())


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
  | Uint8
  | Uint16
  | Uint32
  | Int32
  | Float32
  | Float64
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


let%test "import tree" =
  let tree = import_tree "composite" in
  tree =
    Tree ("composite",
      [ Tree ("xfixes",
        [ Tree ("shape", [Leaf "xproto"])
        ; Tree ("render", [Leaf "xproto"])
        ; Leaf "xproto" ])
      ; Leaf "xproto" ])


let%test_unit _ =
  files |> List.iter begin fun f ->
    let file = Parser.parse_file (file_loc ^ f ^ ".xml") in
    let (name, decls) = match file with
      | Core decls -> ("xproto", decls)
      | Extension ({ name; _ }, decls) -> (name, decls)
    in
    print_endline ("MODULE " ^ name);
    enum_usage decls
  end


let%test_unit _ =
  files |> List.iter begin fun f ->
    let file = Parser.parse_file (file_loc ^ f ^ ".xml") in
    let decls = match file with
      | Core decls -> decls
      | Extension (_, decls) -> decls
    in
    print_string "FILE ";
    print_endline f;
    decls |> List.iter begin function
      | X.Struct { switch = Some { cases; _ }; _ }
      | X.Union  { switch = Some { cases; _ }; _ }
      | X.Request { params = { switch = Some { cases; _ }; _ }; reply = None; _ } ->
          List.iter (fun c -> match c.X.name_c with Some n -> print_endline n | None -> ()) cases
      | X.Request { params = { switch = Some { cases = c1; _ }; _ }; reply = Some { switch = Some { cases = c2; _ }; _ }; _ } ->
          List.iter (fun c -> match c.X.name_c with Some n -> print_endline n | None -> ()) c1;
          List.iter (fun c -> match c.X.name_c with Some n -> print_endline n | None -> ()) c2
      | _ -> ()
    end
  end
*)

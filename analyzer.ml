module X = Parser

open StdLabels
open MoreLabels


type base_type =
  | Byte
  | Int8  | Int16  | Int32
  | Uint8 | Uint16 | Uint32
  | Float32 | Float64

let base_type_of_string = function
  | "char"   -> Some Byte
  | "BYTE"   -> Some Byte
  | "INT32"  -> Some Int32
  | "CARD8"  -> Some Uint8
  | "CARD16" -> Some Uint16
  | "CARD32" -> Some Uint32
  | "float"  -> Some Float32
  | "double" -> Some Float64
  | t        -> None


type enum_items = (string * int) list

type bitmask =
  { vals  : enum_items option
  ; flags : enum_items }


(* FIXME: sometimes enums have the same value for 0; decide what to keep where *)


type 'a x_t =
  | Base of 'a
  | Alias of string
  | Ext_alias of string * string

type declaration =
  | Import of string
  | Type of string * base_type x_t
  | Enum of string * enum_items x_t
  | Bitmask of string * bitmask x_t

type x_module =
  { file_name : string
  ; query_extension_name : string option
  ; version : (int * int) option
  ; declarations : declaration list }


let file_loc = "xproto/src"

let modules : (string, x_module) Hashtbl.t =
  Hashtbl.create 13

let module_ids : (string, string) Hashtbl.t =
  Hashtbl.create 13


let rec module_of_file = function
  | X.Extension ({ name; xname; version; _ }, d) ->
    { file_name = String.capitalize_ascii name
    ; query_extension_name = Some xname
    ; version = Some version
    ; declarations = declaration d }
  | X.Core d ->
    { file_name = "Xproto"
    ; query_extension_name = None
    ; version = None
    ; declarations = declaration d }


and declaration d =
  let loop acc = function
    | X.Import file ->
      let name = get_import_name file in
      Import name :: acc

    | X.X_id name ->
      Type (name, Base Uint32) :: acc

    | X.Enum (name, `Enum items, _) ->
      Enum (name, Base items) :: acc

    | X.Enum (name, `Bitmask { vals; bits }, _) ->
      let vals = match vals with
        | [] -> None | l -> Some l
      in
      Bitmask (name, Base { flags = bits; vals }) :: acc

    | X.Type_alias (name, old) ->
      let t =
        match base_type_of_string old with
        | Some t ->
          Type (name, Base t)
        | None ->
          alias name old acc
      in
      t :: acc

    | _ ->
      acc
  in
  List.fold_left d ~init:[] ~f:loop


and alias name id decls =
  let rec find = function
    | Type (n, _) :: _ when n = id ->
      Some (Type (name, Alias id))
    | Enum (n, _) :: _ when n = id ->
      Some (Enum (name, Alias id))
    | Bitmask (n, _) :: _ when n = id ->
      Some (Bitmask (name, Alias id))
    | [] ->
      None
    | Type _ :: rest | Enum _ :: rest | Bitmask _ :: rest
    | Import _ :: rest ->
      find rest
  in
  let rec find_in_imports = function
    | [] -> failwith ("Identifier not found: " ^ id)
    | Import ext :: rest ->
      let m = get_import ext in
      begin try
        match alias name id m.declarations with
        | Type _ ->
          Type (name, Ext_alias (ext, id))
        | Enum _ ->
          Enum (name, Ext_alias (ext, id))
        | Bitmask _ ->
          Bitmask (name, Ext_alias (ext, id))
        | Import _ ->
          assert false
      with Failure _ -> find_in_imports rest end
    | _ :: rest ->
      find_in_imports rest
  in
  match find decls with
  | Some x -> x
  | None -> find_in_imports decls


(* Entry point *)
and mk_module file =
  let fname = Filename.concat file_loc (file ^ ".xml") in
  let doc = module_of_file (Parser.parse_file fname) in
  Hashtbl.add module_ids ~key:file ~data:doc.file_name;
  Hashtbl.add modules ~key:doc.file_name ~data:doc;
  doc


and get_import_name file =
  match Hashtbl.find_opt module_ids file with
  | Some id -> id
  | None ->
    let doc = mk_module file in
    doc.file_name


and get_import name =
  Hashtbl.find modules name


  (*
and module_of_type id decls : x_type =
  let rec find = function
    | Type (n, Base _)                  :: _
    | Type (n, Ref (Base_ref _))        :: _
    | Type (n, Ext_ref (_, Base_ref _)) :: _ when n = id ->
      Some (Base_ref n)

    | Enum (n, _)                       :: _
    | Type (n, Ref (Enum_ref _))        :: _
    | Type (n, Ext_ref (_, Enum_ref _)) :: _ when n = id ->
      Some (Enum_ref n)

    | [] ->
      None

    | Import _ :: rest ->
      find rest
  in
  let rec loop = function
    | [] -> failwith id
    | Import name :: rest ->
      let m = get_import name in
      begin try
        match module_of_type id m.declarations with
        | Ref t | Ext_ref (_, t) -> Ext_ref (id, t)
        | _ -> assert false
      with Failure _ -> loop rest end
    | _ :: rest -> loop rest
  in
  match find decls with
  | Some x -> Ref x
  | None -> loop decls
  *)


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


let enum_i name =
  match name.[0] with
  | '0' .. '9' -> "N" ^ name
  | _ -> String.capitalize_ascii name


let ocaml_type = function
  | Byte -> "int"
  | Int8 | Int16 -> "int"
  | Int32 -> "int32"
  | Uint8 | Uint16 -> "int"
  | Uint32 -> "int32"
  | Float32 | Float64 -> "float"


let%test_unit _ =
  if not (Sys.file_exists "out") then
    Unix.mkdir "out" 0o750;
  let files = [ "xproto" ] in
  List.iter files ~f:(fun file ->
    let m = mk_module file in
    let open Printf in
    let print_file out =
      let output_endline out s =
        output_string out s; output_char out '\n'
      in
      output_endline out "type ('a, 'b) bitmask = Flags of 'a list | Val of 'b";
      fprintf out "module %s = struct\n" m.file_name;
      begin match m.query_extension_name with None -> () | Some x ->
        fprintf out "  let query_extension_name = %S\n" x
      end;
      begin match m.version with None -> () | Some (maj, min) ->
        fprintf out "  let version = (%d, %d)\n" maj min
      end;
      List.iter m.declarations ~f:(function
        | Type (n, Alias t) ->
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s = %s\n" n t
        | Enum (n, Alias t) ->
            print_endline "enum alias";
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s_enum = %s_enum\n" n t
        | Bitmask (n, Alias t) ->
            print_endline "bitmask alias";
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s_bitmask = %s_bitmask\n" n t

        | Type (n, Ext_alias (e, t)) ->
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s = %s.%s\n" n e t
        | Enum (n, Ext_alias (e, t)) ->
            print_endline "enum ext alias";
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s_enum = %s.%s_enum\n" n e t
        | Bitmask (n, Ext_alias (e, t)) ->
            print_endline "bitmask ext alias";
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s_bitmask = %s.%s_bitmask\n" n e t

        | Type (n, Base t) ->
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s = %s\n" n (ocaml_type t)
        | Enum (n, Base items) ->
          let n = snake_cased n ^ "_enum" in
          output_char out '\n';
          fprintf out "  type %s = [\n" n;
          List.iter (fun (n, _) -> output_endline out ("    | `" ^ enum_i n)) items;
          output_endline out "  ]";
          fprintf out "  let int32_of_%s : %s -> int32 = function\n" n n;
          List.iter (fun (n, i) -> fprintf out "    | `%s -> %dl\n" (enum_i n) i) items;
          fprintf out "  let %s_of_int32 : int32 -> %s = function\n" n n;
          List.iter (fun (n, i) -> fprintf out "    | %dl -> `%s\n" i (enum_i n)) items;
          fprintf out "    | _ -> failwith \"not recognized\"\n"
        | Bitmask (n, Base { flags; vals }) ->
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s_flag = [\n" n;
          List.iter (fun (n, _) -> output_endline out ("    | `" ^ enum_i n)) flags;
          output_endline out "  ]";
          begin match vals with None -> () | Some vals ->
            fprintf out "  type %s_val = [\n" n;
            List.iter (fun (n, _) -> output_endline out ("    | `" ^ enum_i n)) vals;
            output_endline out "  ]";
            fprintf out "  type %s_bitmask = (%s_flag, %s_val) bitmask\n" n n n
          end;
          fprintf out "  let int32_of_%s_flags : %s_flag list -> int32 =\n" n n;
          output_endline out "    List.fold_left (fun acc -> function";
          List.iter (fun (n, i) ->
            fprintf out "      | `%s -> Int32.(logor (shift_left 1l %d) acc)\n" (enum_i n) i
          ) flags;
          output_endline out "    ) 0l";
          begin match vals with None -> () | Some vals ->
            fprintf out "  let int32_of_%s_val : %s_val -> int32 = function\n" n n;
            List.iter (fun (n, i) -> fprintf out "    | `%s -> %dl\n" (enum_i n) i) vals
          end

        | Import _ -> ()
      );
      output_endline out "end"
    in
    let out = open_out @@ Filename.concat "out" (file ^ ".ml") in
    try
      print_file out;
      close_out out
    with exn ->
      close_out out;
      raise exn
  )

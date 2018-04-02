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

let size_of_base_type = function
  | Byte | Int8 | Uint8      -> 1
  | Int16 | Uint16           -> 2
  | Int32 | Uint32 | Float32 -> 4
  | Float64                  -> 8


type enum_items = (string * int) list

type bitmask =
  { vals  : enum_items option
  ; flags : enum_items }


(* FIXME: sometimes enums have the same value for 0; decide what to keep where.
 * this only seems to happen in xproto though. *)

type ref_type =
  | Base_ref of string
  | Enum_ref of string
  | Bitmask_ref of string


type ref_t =
  [ `Ref of string
  | `Ext of string * string ]

type x_t =
  | Base of base_type
  | Ref of ref_type
  | Ext_ref of string * ref_type

type struct_item =
  | Pad of int
  | Field of string * base_type

type declaration =
  | Import of string
  | Type of string * x_t
  | Enum of string * enum_items
  | Bitmask of string * bitmask
  | Error of string * int * struct_item list
  | Error_alias of string * int * ref_t

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
      Enum (name, items) :: acc

    | X.Enum (name, `Bitmask { vals; bits }, _) ->
      let vals = match vals with
        | [] -> None | l -> Some l
      in
      Bitmask (name, { flags = bits; vals }) :: acc

    | X.Type_alias (name, old) ->
      begin try (* @Hack DELET THIS *)
      let t =
        match base_type_of_string old with
        | Some t ->
          Type (name, Base t)
        | None ->
          Type (name, alias old acc)
      in
      t :: acc
      with _ -> acc end

    | X.Union { switch = Some _; _ } ->
      assert false

    | X.Error { name; code; fields; _ } ->
      let fields = List.map fields ~f:(function
        | `Pad { X.typ = `Bytes; amount; _ } ->
          Pad amount
        | `Field { X.name; typ; allowed = None; _ } ->
          let typ = match base_type_of_string typ with
            | Some t -> t
            | None -> failwith name
          in
          Field (name, typ)
        | _ ->
          failwith "not implemented"
      ) in
      Error (name, code, fields) :: acc

    | X.Error_alias ((name, code), old) ->
      let old = error_alias old acc in
      Error_alias (name, code, old) :: acc

    | _ ->
      acc
  in
  List.rev @@ List.fold_left d ~init:[] ~f:loop


and alias id decls =
  let rec find = function
    | Type (n, Ref (Base_ref _)) :: _
    | Type (n, Ext_ref (_, Base_ref _)) :: _
    | Type (n, Base _) :: _ when n = id ->
      Some (Base_ref id)

    | Type (n, Ref (Enum_ref _)) :: _
    | Type (n, Ext_ref (_, Enum_ref _)) :: _
    | Enum (n, _) :: _ when n = id ->
      Some (Enum_ref id)

    | Type (n, Ref (Bitmask_ref _)) :: _
    | Type (n, Ext_ref (_, Bitmask_ref _)) :: _
    | Bitmask (n, _) :: _ when n = id ->
      Some (Bitmask_ref id)

    | [] ->
      None

    | Type _ :: rest | Enum _ :: rest | Bitmask _ :: rest
    | Error _ :: rest | Error_alias _ :: rest
    | Import _ :: rest ->
      find rest
  in
  let rec find_in_imports = function
    | [] ->
      failwith ("Identifier not found: " ^ id)
    | Import ext :: rest ->
      let m = get_import ext in
      begin match find m.declarations with
      | Some r ->
        Ext_ref (ext, r)
      | None ->
        find_in_imports rest
      end
    | _ :: rest ->
      find_in_imports rest
  in
  match find decls with
  | Some x -> Ref x
  | None -> find_in_imports decls


and error_alias id decls : ref_t =
  let rec find = function
    | Error (n, _, _) :: _
    | Error_alias (n, _, _) :: _ when n = id ->
      Some n
    | _ :: rest ->
      find rest
    | [] ->
      None
  in
  let rec find_in_imports = function
    | [] -> failwith ("Error identifier not found: " ^ id)
    | Import ext :: rest ->
        let m = get_import ext in
        begin match find m.declarations with
        | Some r ->
          `Ext (ext, r)
        | None ->
          find_in_imports rest
        end
    | _ :: rest ->
      find_in_imports rest
  in
  match find decls with
  | Some x -> `Ref x
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


let decode_base_type = function
  | Byte | Int8 | Uint8 -> "get_byte"
  | Int16 | Uint16 -> "get_uint16"
  | Int32 | Uint32 -> "get_uint32"
  | Float32 | Float64 -> failwith "not implemented"


let%test_unit _ =
  if not (Sys.file_exists "out") then
    Unix.mkdir "out" 0o750;
  (* let files = [ "xproto" ] in *)
  List.iter files ~f:(fun file ->
    let m = mk_module file in
    let open Printf in
    let print_file out =
      let output_endline out s =
        output_string out s; output_char out '\n'
      in
      output_endline out "open X11_base\n";
      fprintf out "(* module %s = struct *)\n" m.file_name;
      begin match m.query_extension_name with None -> () | Some x ->
        fprintf out "  let query_extension_name = %S\n" x
      end;
      begin match m.version with None -> () | Some (maj, min) ->
        fprintf out "  let version = (%d, %d)\n" maj min
      end;
      List.iter m.declarations ~f:(function
        | Type (n, Base t) ->
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s = %s\n" n (ocaml_type t)

        | Type (n, t) ->
          let n = snake_cased n in
          output_char out '\n';
          let m, t = match t with
            | Ref r -> "", r
            | Ext_ref (e, r) -> (e ^ "."), r
            | Base _ -> assert false
          in
          let t, suffix = match t with
            | Base_ref t -> t, ""
            | Enum_ref t -> t, "_enum"
            | Bitmask_ref t -> t, "_bitmask"
          in
          fprintf out "  type %s%s = %s%s%s\n" n suffix m t suffix

        | Enum (n, items) ->
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

        | Bitmask (n, { flags; vals }) ->
          let n = snake_cased n in
          output_char out '\n';
          fprintf out "  type %s_flag = [\n" n;
          List.iter (fun (n, _) -> output_endline out ("    | `" ^ enum_i n)) flags;
          output_endline out "  ]";
          begin match vals with
          | None ->
            fprintf out "  type %s_bitmask = %s_flag list\n" n n
          | Some vals ->
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
          end;
          fprintf out "  let int32_of_%s_bitmask : %s_bitmask -> int32 = " n n;
          begin match vals with
          | None ->
            fprintf out "int32_of_%s_flags\n" n
          | Some _ ->
            output_endline out "function";
            fprintf out "    | Flags f -> int32_of_%s_flags f\n" n;
            fprintf out "    | Val v -> int32_of_%s_val v\n" n
          end

        | Error (name, code, fields) ->
          output_char out '\n';
          let n = snake_cased name in
          if fields <> [] then begin
            fprintf out "  type %s_error_content =\n" n;
            List.iteri fields ~f:(fun i -> function
              | Field (n, t) ->
                let n = snake_cased n in
                if i = 0 then
                  fprintf out "    { %s : %s" n (ocaml_type t)
                else
                  fprintf out "\n    ; %s : %s" n (ocaml_type t)
              | _ -> ()
            );
            output_endline out " }"
          end;
          begin if fields <> [] then
            fprintf out "  let %s_error : %s_error_content error =\n" n n
          else
            fprintf out "  let %s_error : unit error =\n" n
          end;
          if fields <> [] then begin
            output_endline out "    let read_content buf offs =";
            let _ = List.fold_left fields ~init:0 ~f:(fun offset -> function
              | Pad n -> offset + n
              | Field (n, t) ->
                let n = snake_cased n in
                fprintf out    "      let %s = %s buf (offs + %d) in\n" n
                  (decode_base_type t) offset;
                offset + size_of_base_type t
            ) in
            List.iteri fields ~f:(fun i -> function
              | Field (n, _) ->
                let n = snake_cased n in
                if i = 0 then
                  fprintf out  "      { %s" n
                else (
                  output_string out "; ";
                  output_string out n
                )
              | _ -> ()
            );
            output_endline out " }";
            output_endline out "    in";
          end;
          fprintf out        "    { name = %S\n" name;
          fprintf out        "    ; code = %d\n" code;
          if fields <> [] then
            output_endline out "    ; content = read_content }"
          else
            output_endline out "    ; content = fun _ _ -> () }"

        | Error_alias (name, code, old) ->
          output_char out '\n';
          let n = snake_cased name in
          fprintf out "  let %s_error =\n" n;
          fprintf out "    { name = %S\n    ; code = %d\n" name code;
          begin match old with
          | `Ref old ->
            fprintf out "    ; content = %s_error.content }\n" (snake_cased old)
          | `Ext (m, old) ->
            fprintf out "    ; content = %s.%s_error.content }\n" m (snake_cased old)
          end

        | Import _ -> ()
      );
      output_endline out "(* end *)"
    in
    let out = open_out @@ Filename.concat "out" (file ^ ".ml") in
    try
      print_file out;
      close_out out
    with exn ->
      close_out out;
      raise exn
  )

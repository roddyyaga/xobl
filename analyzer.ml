module X = Parser

open StdLabels
open MoreLabels


let option_get_exn = function
  | Some x -> x
  | None -> raise Not_found


let string_split chr str =
  let len = String.length str in
  let rec pos i =
    if i >= len then
      None
    else if str.[i] = chr then
      let left = String.sub str ~pos:0 ~len:(if i = 0 then 0 else i - 1) in
      let right = String.sub str ~pos:(i + 1) ~len:(len - i - 1) in
      Some (left, right)
    else
      pos (i + 1)
  in pos 0


(* Primitive types *)
type prim_t =
  | Bool
  | Byte
  | Int8
  | Int16
  | Int32
  | Uint8
  | Uint16
  | Uint32
  | Float32
  | Float64

let prim_t_of_string = function
  | "char"   -> Some Byte
  | "BYTE"   -> Some Byte
  | "BOOL"   -> Some Bool
  | "INT16"  -> Some Int16
  | "INT32"  -> Some Int32
  | "CARD8"  -> Some Uint8
  | "CARD16" -> Some Uint16
  | "CARD32" -> Some Uint32
  | "float"  -> Some Float32
  | "double" -> Some Float64
  | _        -> None

let size_of_prim_t = function
  | Bool | Byte  | Int8   | Uint8 -> 1
  | Int16 | Uint16           -> 2
  | Int32 | Uint32 | Float32 -> 4
  | Float64                  -> 8


type enum_items =
  (string * int) list

type mask =
  { vals  : enum_items option
  ; flags : enum_items }


type ref_t =
  [ `Ref of string
  | `Ext of string * string ]

type basic_t =
  [ ref_t
  | `Prim of prim_t
  | `Union of ref_t ]


type binop = [ `Add | `Sub | `Mul | `Div | `Bit_and | `Bit_left_shift ]
type unop = [ `Bit_not ]

type expression =
  | Binop of binop * expression * expression
  | Unop of unop * expression
  | EField of basic_t
  | EParam of basic_t * ref_t
  | EEnum of basic_t * ref_t
  | Pop_count of expression
  | Sum_of of ref_t * expression option
  | Current_ref
  | EValue of int
  | EBit of int


type struct_item =
  | Pad of int
  | Field of string * basic_t
  | FEnum of string * basic_t * ref_t
  | FMask of string * basic_t * ref_t
  | FAlt_enum of string * basic_t * ref_t
  | FAlt_mask of string * basic_t * ref_t
  | List of string * basic_t * expression option


type event_opts =
  { seq_num : bool }


type declaration =
  | Import of string

  | Type of string * prim_t
  | Type_alias of string * ref_t

  | Enum of string * enum_items
  | Enum_alias of string * ref_t

  | Mask of string * mask
  | Mask_alias of string * ref_t

  | ID_union of string * ref_t list

  | Error of string * int * struct_item list option
  | Event of string * int * struct_item list option * event_opts

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


and alias (find : declaration -> bool) id decls =
  match string_split ':' id with
  | Some (m, _) ->
    Some (`Ext (get_import_name m, id))
  | None ->
    let rec find_in_imports = function
      | Import ext :: rest ->
        let m = get_import ext in
        if List.exists ~f:find m.declarations then
          Some (`Ext (ext, id))
        else
          find_in_imports rest
      | _ :: rest ->
        find_in_imports rest
      | [] ->
        None
    in
    if List.exists decls ~f:find then
      Some (`Ref id)
    else
      find_in_imports decls


and basic_alias id =
  alias (function
    | Type (n, _) | Type_alias (n, _) when n = id -> true
    | _ -> false
  ) id

and basic_alias_exn id decls =
  option_get_exn (basic_alias id decls)


and enum_alias id =
  alias (function
    | Enum (n, _) | Enum_alias (n, _) when n = id -> true
    | _ -> false
  ) id

and enum_alias_exn id decls =
  option_get_exn (enum_alias id decls)


and mask_alias id =
  alias (function
    | Mask (n, _) | Mask_alias (n, _) when n = id -> true
    | _ -> false
  ) id

and mask_alias_exn id decls =
  option_get_exn (mask_alias id decls)


and error_alias id =
  alias (function
    | Error (n, _, _) | Error_alias (n, _, _) when n = id -> true
    | _ -> false
  ) id

and error_alias_exn id decls =
  option_get_exn (error_alias id decls)


and id_union_alias id =
  alias (function
    | ID_union (n, _) when n = id -> true
    | _ -> false
  ) id

and id_union_alias_exn id decls =
  option_get_exn (id_union_alias id decls)


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


and event_field decls = function
  | `Pad { X.typ = `Bytes; amount; _ } ->
    Pad amount

  | `Field { X.name; typ; allowed = None; _ } ->
    let t =
      match prim_t_of_string typ
      with Some p -> `Prim p | None ->
      match basic_alias typ decls
      with Some t -> (t :> basic_t) | None ->
      `Union (id_union_alias_exn typ decls)
    in
    Field (name, t)

  | `Field { X.name; typ; allowed = Some (a, at); _ } ->
    let typ =
      match prim_t_of_string typ
      with Some p -> `Prim p | None ->
      match basic_alias typ decls
      with Some t -> (t :> basic_t) | None ->
      `Union (id_union_alias_exn typ decls)
    in
    begin match a with
      | `Enum -> FEnum (name, typ, enum_alias_exn at decls)
      | `Mask -> FMask (name, typ, mask_alias_exn at decls)
      | `Alt_enum -> FAlt_enum (name, typ, enum_alias_exn at decls)
      | `Alt_mask -> FAlt_mask (name, typ, mask_alias_exn at decls)
    end

    (*
  | `List { X.name; typ; allowed = None; value = None } ->
    let typ =
      match prim_t_of_string typ
      with Some p -> `Prim p | None ->
      match basic_alias typ decls
      with Some t -> (t :> basic_t) | None ->
      `Union (id_union_alias_exn typ decls)
    in
    *)


  | `File_descriptor str ->
    failwith ("unsupported field: file descriptor " ^ str)
  | _ ->
    failwith "unsupported field in event/error struct"


and declaration d =
  List.rev @@ List.fold_left d ~init:[] ~f:(fun acc -> function
    | X.Import file ->
      let name = get_import_name file in
      Import name :: acc

    | X.X_id name ->
      Type (name, Uint32) :: acc

    | X.Enum (name, `Enum items, _) ->
      Enum (name, items) :: acc

    | X.Enum (name, `Bitmask { vals; bits }, _) ->
      let vals = match vals with
        | [] -> None | l -> Some l
      in
      Mask (name, { flags = bits; vals}) :: acc

    | X.X_id_union (name, ts) ->
      let ts = List.map ts ~f:(fun x -> basic_alias_exn x acc) in
      ID_union (name, ts) :: acc

    | X.Type_alias (name, old) ->
      begin
        match prim_t_of_string old
        with Some t -> Type (name, t) :: acc | None ->
        match basic_alias old acc
        with Some r -> Type_alias (name, r) :: acc | None ->
        match enum_alias old acc
        with Some r -> Enum_alias (name, r) :: acc | None ->
        match mask_alias old acc
        with Some r -> Mask_alias (name, r) :: acc | None ->
        acc (** FIXME type_alias also marks stuct aliases *)
      end

    | X.Error { name; code; fields; _ } ->
      let fields = match fields with [] -> None
        | fs -> Some (List.map fs ~f:(event_field acc))
      in
      Error (name, code, fields) :: acc

    | X.Error_alias ((name, code), old) ->
      let old = error_alias_exn old acc in
      Error_alias (name, code, old) :: acc

    | X.Event { name; code; no_sequence_number; fields; align = None; _ } ->
      begin try
      let fields = match fields with [] -> None
        | fs -> Some (List.map fs ~f:(event_field acc))
      in
      Event (name, code, fields, { seq_num = not no_sequence_number }) :: acc
      with Failure x -> failwith ("event " ^ name ^ ": " ^ x)
      | Not_found -> failwith ("event " ^ name) end

    | _ ->
      acc
  )



(* ***************************************************** *)

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
         * GLXContext -> glx_context instead?
         * ^ no, because DECnet would become de_cnet.*)
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
  | Bool -> "bool"
  | Byte -> "int"
  | Int8 | Int16 -> "int"
  | Int32 -> "int32"
  | Uint8 | Uint16 -> "int"
  | Uint32 -> "int32"
  | Float32 | Float64 -> "float"


let decode_base_type = function
  | Bool -> "get_bool"
  | Byte | Int8 | Uint8 -> "get_byte"
  | Int16 | Uint16 -> "get_uint16"
  | Int32 | Uint32 -> "get_uint32"
  | Float32 | Float64 -> failwith "not implemented"


let print_struct out ~sep ?delim ~indent string_of_item items =
  let indent = String.make indent ' ' in
  let beg, end_ = match delim with
    | Some (b, e) -> b, Some e | None -> sep, None
  in
  let rec prn = function
    | [] -> ()
    | hd :: tl ->
      match string_of_item hd with
      | None -> prn tl
      | Some x ->
        Printf.fprintf out "%s%c %s" indent beg x;
        List.iter tl ~f:(fun item -> match string_of_item item with
          | None -> ()
          | Some x ->
            Printf.fprintf out "\n%s%c %s" indent sep x
        );
        match end_ with None -> () | Some e ->
          Printf.fprintf out " %c\n" e
  in prn items


let string_of_ref_t = function
  | `Ref n -> snake_cased n
  | `Ext (e, n) -> e ^ "." ^ snake_cased n


let string_of_basic_t = function
  | `Prim t -> ocaml_type t
  | `Union r -> string_of_ref_t r ^ "_union"
  | #ref_t as r -> string_of_ref_t r


let gen_ocaml_module out m =
  let pf = Printf.fprintf in
  let ps = output_string out in
  let pn () = output_char out '\n' in
  let pe s = ps s; pn () in
  pe "open X11_base\n";
  begin match m.query_extension_name with None -> () | Some x ->
    pf out "let query_extension_name = %S\n" x
  end;
  begin match m.version with None -> () | Some (maj, min) ->
    pf out "let version = (%d, %d)\n" maj min
  end;
  List.iter m.declarations ~f:(
    output_char out '\n';
    function
    | Import _ -> ()

    | Type (n, t) ->
      let n = snake_cased n in
      pf out "type %s = %s\n" n (ocaml_type t)

    | Enum (n, items) ->
      let n = snake_cased n ^ "_enum" in
      pf out "type %s =\n" n;
      print_struct out (fun (n, _) -> Some ("`" ^ enum_i n)) items
        ~sep:'|' ~delim:('[',']') ~indent:2

    | Mask (n, { flags; vals }) ->
      let n = snake_cased n in
      pf out "type %s_flag =\n" n;
      print_struct out (fun (n, _) -> Some ("`" ^ enum_i n)) flags
        ~sep:'|' ~delim:('[',']') ~indent:2;
      begin match vals with
        | None ->
          pf out "type %s_mask = %s_flag list\n" n n
        | Some vals ->
          pf out "type %s_val =\n" n;
          print_struct out (fun (n, _) -> Some ("`" ^ enum_i n)) vals
            ~sep:'|' ~delim:('[',']') ~indent:2;
          pf out "type %s_mask = (%s_flag, %s_val) mask\n" n n n
      end

    | ID_union (n, ts) ->
      let n = snake_cased n in
      pf out "type %s_union =\n" n;
      print_struct out (fun n -> Some ("`" ^ string_of_ref_t n ^ " of int32")) ts
        ~sep:'|' ~delim:('[',']') ~indent:2

    | Type_alias (n, t) ->
      let n = snake_cased n in
      pf out "type %s = %s\n" n (string_of_ref_t t)

    | Enum_alias (n, t) ->
      let n = snake_cased n in
      pf out "type %s_enum = %s_enum\n" n (string_of_ref_t t)

    | Mask_alias (n, t) ->
      let n = snake_cased n in
      pf out "type %s_mask = %s_mask\n" n (string_of_ref_t t)

    | Error_alias (name, code, old) ->
      let n = snake_cased name in
      pf out "let %s_error =\n" n;
      pf out "  { name = %S\n  ; code = %d\n" name code;
      pf out "  ; content = %s_error.content }\n" (string_of_ref_t old)

    | Error (name, code, fields) ->
      let n = snake_cased name in
      begin match fields with Some fields ->
        pf out "type %s_error_content =\n" n;
        print_struct out (function
          | Pad _ -> None
          | Field (n, t) ->
            Some (snake_cased n ^ " : " ^ string_of_basic_t t)
          | FEnum (n, t, r) ->
            Some (snake_cased n ^ " : " ^ string_of_ref_t r ^ "_enum")
          | FMask (n, t, r) ->
            Some (snake_cased n ^ " : " ^ string_of_ref_t r ^ "_mask")
          | FAlt_enum (n, t, r) ->
            Some (snake_cased n ^ " : " ^ string_of_ref_t r ^ "_alt_enum")
          | FAlt_mask (n, t, r) ->
            Some (snake_cased n ^ " : " ^ string_of_ref_t r ^ "_alt_mask")
        ) fields ~sep:';' ~delim:('{','}') ~indent:2;
        pf out "let %s_error =\n" n;
        pe     "  let read_content buf offs =";
        pe     "    () in"; (** FIXME we need type size info *)
        pf out "  { name = %S\n  ; code = %d\n" name code;
        pf out "  ; content = read_content }\n"
      | None ->
        pf out "let %s_error : unit error =\n" n;
        pf out "  { name = %S\n  ; code = %d\n" name code;
        pf out "  ; content = fun _ _ -> () }\n"
      end

    | Event _ -> ()
  )



let%test_unit _ =
  if not (Sys.file_exists "out") then
    Unix.mkdir "out" 0o750;
  List.iter files ~f:begin fun file ->
    let out = open_out @@ Filename.concat "out" @@ file ^ ".ml" in
    try
      gen_ocaml_module out (mk_module file);
      close_out out
    with exn ->
      close_out out;
      raise exn
  end

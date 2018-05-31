open Util


let resolve_extension_path name =
  Filename.concat "xproto/src" (name ^ ".xml")


let load_extension file_name : Parser.protocol_file =
  resolve_extension_path file_name
  |> Parser.parse_file


let snake_cased name =
  if String.uppercase_ascii name = name then
    (* The string is already snake_cased, just make sure it's lowercase. *)
    String.lowercase_ascii name
  else
    (* The string is CamelCased. *)
    let buf = Buffer.create 16 in
    StringLabels.iteri name ~f:(fun i -> function
      | c when i = 0 ->
        Buffer.add_char buf (Char.lowercase_ascii c)
      | 'A' .. 'Z' as c ->
        (* We want to make sure something like GLXContext is turned into
           glxcontext and not g_l_x_context.
           We can't turn it into glx_context because then DECnet would become
           de_cnet. *)
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


let caml_cased name =
  String.capitalize_ascii (snake_cased name)


let id_str = function
  | Analyzer.Id n -> snake_cased n
  | Analyzer.Ext_id (e, n) ->
    String.capitalize_ascii (snake_cased e) ^ "." ^ snake_cased n


let prim_to_string =
  let open Analyzer.Prim in function
    | Void   -> "unit"
    | Char   -> "char"
    | Byte   -> "char"
    | Bool   -> "bool"
    | Int8   -> "int"
    | Int16  -> "int"
    | Int32  -> "int32"
    | Fd     -> "int"
    | Card8  -> "int"
    | Card16 -> "int"
    | Card32 -> "int32"
    | Card64 -> "int64"
    | Float  -> "float"
    | Double -> "float"
    | Xid    -> "xid"


let x_type_str = function
  | Analyzer.Prim t -> prim_to_string t
  | Analyzer.Ref id -> id_str id


let field_type_str =
  let open Analyzer.Pass_2 in function
  | Prim t ->
    x_type_str t
  | Enum (e, t) ->
    (x_type_str t) ^ " " ^ (id_str e) ^ "_enum"
  | Mask (e, t) ->
    (x_type_str t) ^ " " ^ (id_str e) ^ "_mask"
  | Enum_or (e, t) ->
    Printf.sprintf "(%s, %s %s_enum) either"
      (x_type_str t) (x_type_str t) (id_str e)
  | Mask_or (e, t) ->
    Printf.sprintf "(%s, %s %s_mask) either"
      (x_type_str t) (x_type_str t) (id_str e)


let padding_str : Parser.padding -> string = fun { Parser.pad; serialize } ->
  match pad with
  | `Bytes n -> string_of_int n ^ " bytes"
  | `Align n -> "align to " ^ string_of_int n


let print_struct out ~indent of_item items =
  let indent = String.make indent ' ' in
  let rec prn = function
    | [] -> ()
    | hd :: tl ->
      Printf.fprintf out "%s{ %s" indent (of_item hd);
      tl |> List.iter (fun item ->
        Printf.fprintf out "\n%s; %s" indent (of_item item)
      )
  in prn items


let binop_str : Parser.binop -> string = function
  | `Add -> "( + )"
  | `Sub -> "( - )"
  | `Mul -> "( * )"
  | `Div -> "( / )"
  | `Bit_and -> "( land )"
  | `Bit_left_shift -> "( lsl )"

let unop_str : Parser.unop -> string = function
  | `Bit_not -> "lnot"


let rec expression_str : Analyzer.Pass_2.expression -> string =
  let p fmt = Printf.sprintf fmt in
  function
  | `Binop (op, e1, e2) ->
    p "%s (%s) (%s)" (binop_str op) (expression_str e1) (expression_str e2)
  | `Unop (op, e) ->
    p "%s (%s)" (unop_str op) (expression_str e)
  | `Field_ref n ->
    snake_cased n
  | `Param_ref (n, _) ->
    snake_cased n
  | `Enum_ref (en, i) ->
    "`" ^ (caml_cased i)
  | `Sum_of (f, e) ->
    begin match e with
    | None ->
      p "List.fold_left ( + ) 0 %s" f
    | Some e ->
      p "List.fold_left (fun acc curr' -> acc + (%s)) 0 %s" (expression_str e) (snake_cased f)
    end
  | `Current_ref ->
    "curr'"
  | `Pop_count expr ->
    p "pop_count (%s)" (expression_str expr)
  | `Value n ->
    string_of_int n
  | `Bit n ->
    string_of_int (1 lsl n)


let static_field_str = function
  | `Pad p ->
    Printf.sprintf "(* pad: %s *)" (padding_str p)
  | `Field (n, t) ->
    Printf.sprintf "%s : %s;" (snake_cased n) (field_type_str t)
  | `List (n, t, l) ->
    Printf.sprintf "%s : %s array; (* length: %s *)"
      (snake_cased n) (field_type_str t) (expression_str l)
  | `File_descriptor n ->
    Printf.sprintf "%s : file_descriptor;" (snake_cased n)


let print_static_field out ~indent =
  print_struct out ~indent (function
    | `Pad p ->
      Printf.sprintf "(* pad: %s *)" (padding_str p)
    | `Field (n, t) ->
      Printf.sprintf "%s : %s;" (snake_cased n) (field_type_str t)
    | `List (n, t, l) ->
      Printf.sprintf "%s : %s array; (* length: %s *)"
        (snake_cased n) (field_type_str t) (expression_str l)
    | `File_descriptor n ->
      Printf.sprintf "%s : file_descriptor;" (snake_cased n)
  )


(* To be usable these extensions certainly need some more work; maybe we should
   define a thin wrapper and not do any complex usage tracking stuff here e.g.
   deciding whether types should be opaque and so on. *)

let generate out (ext : Analyzer.Pass_2.extension_p2) =
  (* let fo fmt = Printf.fprintf out fmt in *)
  let fe fmt = Printf.fprintf out (fmt ^^ "\n") in
  let ps = output_string out in
  let pe s = output_string out s; output_char out '\n' in
  let pn () = output_char out '\n' in
  pe "(*****************************************************************************)";
  fe "module %s = struct" (String.capitalize_ascii ext.name);
  pe "(*****************************************************************************)";
  ext.version |> Option.iter (fun (maj, min) ->
    fe "let version = (%d, %d)" maj min);
  ext.query_name |> Option.iter (fun n ->
    fe "let query_extension_name = %S" n);
  pn ();
  ext.declarations |> List.iter begin function
    | `Alias (n, t) ->
      (* Should the types be opaque?
        If yes, what are the implications of that?
        Do we have to track usage to know whether they should ever be supplied
        by the users or can we get by just using the enums?
      *)
      fe "type %s = %s" (snake_cased n) (x_type_str t)

    | `X_id_union (n, _) ->
      (* XID unions are hardly used in the codebase, and outputting a variant
         type rather than simply aliasing them to XIDs would be way more
         trouble than it's worth. This might change in the future, but for now
         it's good enough. *)
      fe "type %s = xid" (snake_cased n)

    | `Enum (name, items) ->
      (* We need to know a few things here:
        - output an enumeration? a bitmask? both? (solved, sort of)
        - which types do we need conversion functions to and from for?
        - for bitmasks, do we need to output conversion functions to the "val"
          items too? (probably not, but maybe we could provide some compare
          functions)
      *)
      ()

    | `Struct (name, s) ->
      fe "type %s = {" (snake_cased name);
      let s : Analyzer.Pass_2.struct_fields = s in
      s.fields |> List.iter (fun x ->
        fe "  %s" (static_field_str x)
      );
      begin match s.switch with
      | None ->
        fe "}"
      | Some (name, sw) ->
        fe "  %s : 'a\n}" name;
        fe "SWITCH %s ->" name;
        begin match sw.cond with
        | `Bit_and e ->
          let e = expression_str e in
          fe "  let cond x' = (%s) land x' != 0 in" e
        | `Eq e ->
          let e = expression_str e in
          fe "  let cond x' = (%s) = x' in" e
        end;
        ps "  ";
        sw.cases |> List.iter (
          fun Analyzer.Pass_2.{ exprs; name; fields; switch; _ } ->
            fe "if %s then" (String.concat " || " (List.map (fun x -> "cond " ^ expression_str x) exprs));
            Option.iter (fun n -> fe "    (* name: %s *)" n) name;
            print_static_field out ~indent:4 fields;
            pe " }";
            switch |> Option.iter (fun (name, _) ->
              fe "(* WARNING NESTED SWITCH: %s *)" name);
            ps "  else "
        );
        pe "()";
      end

    | _ ->
      ()
  end;
  pe "end";
  pn ()



let%test_unit "analyzer test" =
  let files =
    [ "bigreq"; "composite"; "damage"; "dpms"; "dri2"; "dri3"; "ge"; "glx"
    ; "present"; "randr"; "record"; "render"; "res"; "screensaver"; "shape"
    ; "shm"; "sync"; "xc_misc"; "xevie"; "xf86dri"; "xf86vidmode"; "xfixes"
    ; "xinerama"; "xinput"; "xkb"; "xprint"; "xproto"; "xselinux"; "xtest"
    ; "xvmc"; "xv" ]
  in
  let exts = List.map load_extension files in
  let exts = Analyzer.analyze_extensions exts in
  let out = open_out "stuffs.ml" in
  List.iter (generate out) exts

module Last_pass = P1_resolve


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


let variant name =
  if name.[0] >= '0' && name.[0] <= '9' then
    "V" ^ snake_cased name
  else
    caml_cased name

let ocaml_reserved =
  [ "and"; "as"; "asr"; "assert"; "begin"; "class"; "constraint"; "do"; "done"
  ; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"
  ; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"
  ; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"
  ; "mod"; "module"; "open"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"
  ; "open!"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"
  ; "try"; "type"; "val"; "virtual"; "when"; "while"; "with" ]


let identifier name =
  let name = snake_cased name in
  if List.mem name ocaml_reserved then
    name ^ "_"
  else
    name


let ident_str = function
  | Types.Id n -> identifier n
  | Types.Ext_id (e, n) ->
    caml_cased e ^ "." ^ identifier n


let prim_str =
  let open Prim in function
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
  | Types.Prim t -> prim_str t
  | Types.Ref id -> ident_str id


let field_type_str =
  let open Last_pass in function
    | Prim t ->
      x_type_str t
    | Enum (e, t) ->
      (ident_str e) ^ "_enum (* " ^ (x_type_str t) ^ " *)"
    | Mask (e, t) ->
      (ident_str e) ^ "_mask (* " ^ (x_type_str t) ^ " *)"
    | Enum_or (e, t) ->
      Printf.sprintf "(%s, %s_enum) either"
        (x_type_str t) (ident_str e)
    | Mask_or (e, t) ->
      Printf.sprintf "(%s, %s_mask) either"
        (x_type_str t) (ident_str e)


let padding_str : Parser.padding -> string = function
  | Parser.{ pd_pad = `Bytes n; _ } ->
    string_of_int n ^ " bytes"
  | Parser.{ pd_pad = `Align n; _ } ->
    "align to " ^ string_of_int n


    (*
let print_struct out ~indent of_items items =
  let indent = String.make indent ' ' in
  let prn = function
    | [] -> ()
    | hd :: tl ->
      Printf.fprintf out "%s{ %s" indent (of_items hd);
      tl |> List.iter (fun item ->
        Printf.fprintf out "\n%s; %s" indent (of_items item)
      );
      output_string out " }\n"
  in prn items
  *)


let binop_str : Parser.binop -> string = function
  | `Add -> "( + )"
  | `Sub -> "( - )"
  | `Mul -> "( * )"
  | `Div -> "( / )"
  | `Bit_and -> "( land )"
  | `Bit_left_shift -> "( lsl )"

let unop_str : Parser.unop -> string = function
  | `Bit_not -> "lnot"


let rec expression_str : Last_pass.expression -> string =
  let p fmt = Printf.sprintf fmt in
  function
  | `Binop (op, e1, e2) ->
    p "%s (%s) (%s)" (binop_str op) (expression_str e1) (expression_str e2)
  | `Unop (op, e) ->
    p "%s (%s)" (unop_str op) (expression_str e)
  | `Field_ref n ->
    identifier n
  | `Param_ref (n, _) ->
    identifier n
  | `Enum_ref (_en, i) ->
    "`" ^ (caml_cased i)
  | `Sum_of (f, e) ->
    begin match e with
    | None ->
      p "List.fold_left ( + ) 0 %s" f
    | Some e ->
      p "List.fold_left (fun acc curr' -> acc + (%s)) 0 %s" (expression_str e) (identifier f)
    end
  | `Current_ref ->
    "curr'"
  | `Pop_count expr ->
    p "pop_count (%s)" (expression_str expr)
  | `Value n ->
    string_of_int n
  | `Bit n ->
    string_of_int (1 lsl n)


let static_field_str : Last_pass.static_field -> string = function
  | `Pad p ->
    Printf.sprintf "(* pad: %s *)" (padding_str p)
  | `Field (n, t) ->
    Printf.sprintf "%s : %s;" (identifier n) (field_type_str t)
  | `List (n, t, l) ->
    Printf.sprintf "%s : %s array; (* length: %s *)"
      (identifier n) (field_type_str t) (expression_str l)
  | `File_descriptor n ->
    Printf.sprintf "%s : file_descriptor;" (identifier n)


(*
let switch_cond_str : Last_pass.cond -> string = function
  | `Bit_and e ->
    "fun cond' -> (" ^ expression_str e ^ ") land cond' <> 0"
  | `Eq e ->
    "fun cond' -> (" ^ exoression_str e ^ ") = cond'"
*)


(* BUGS:
- handle empty (only padding) structs
*)

let generate out (ext : Last_pass.extension) =
  let fe fmt = Printf.fprintf out (fmt ^^ "\n") in
  let pe s = output_string out s; output_char out '\n' in
  let pn () = output_char out '\n' in

  pe "(*****************************************************************************)";
  fe "module %s = struct" (String.capitalize_ascii ext.name);
  pe "(*****************************************************************************)";
  pe "open X11_base";

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
      fe "type %s = %s" (identifier n) (x_type_str t)
    | `X_id_union (name, _) ->
      (* XID unions are hardly used in the codebase, and outputting a variant
         type rather than simply aliasing them to XIDs would be way more
         trouble than it's worth. It's not strictly correct, but it's usable.
      *)
      fe "type %s = xid" (identifier name)

    | `Enum (name, Parser.{ en_vals; en_bits }) ->
      (* We need to know a few things here:
        - output an enumeration? a bitmask? both? (solved, sort of)
        - which types do we need conversion functions to and from for?
        - for bitmasks, do we need to output conversion functions to the "val"
          items too? (probably not, but maybe we could provide some compare
          functions)
      *)
      fe "type %s_enum = [" (identifier name);
      en_vals |> List.iter (fun (name, _n) ->
        fe "  | `%s" (variant name)
      );
      en_bits |> List.iter (fun (name, _n) ->
        fe "  | `%s" (variant name)
      );
      pe "]"

    | `Union (name, fields) ->
      fe "type %s_union = {" (identifier name);
      List.iter (fun x -> pe ("  " ^ static_field_str x)) fields;
      pe "}"

    | `Struct (struct_name, s) ->
      Option.iter (fun (field_name, sw) ->
        let open Last_pass in
        let cases =
          sw.sw_cases |> List.map (fun case ->
            let expr = match case.cs_exprs with
              | [e] -> e
              | _ -> assert false
            in
            let _enum, item = match expr with
              | `Enum_ref (enum, item) -> enum, item
              | _ -> assert false
            in
            let name = Option.with_default item case.cs_name in
            assert (case.cs_switch = None);
            let t_name = Printf.sprintf "%s_%s_%s_case"
              (identifier struct_name) (identifier field_name) (identifier name)
            in
            fe "type %s = {" t_name;
            List.iter (fun x -> pe ("  " ^ (static_field_str x))) case.cs_fields;
            pe "}";
            (name, t_name)
          )
        in
        fe "type %s_%s_switch = [" (identifier struct_name) (identifier field_name);
        List.iter (fun (name, t_name) ->
          fe "  | `%s of %s" (variant name) t_name
        ) cases;
        pe "]"
      ) s.Last_pass.sf_switch;
      fe "type %s = {" (identifier struct_name);
      s.Last_pass.sf_fields |> List.iter (fun x ->
        fe "  %s" (static_field_str x));
      s.Last_pass.sf_switch |> Option.iter (fun (field_name, _sw) ->
        fe "  %s : %s_%s_switch;"
          (identifier field_name) (identifier struct_name) (identifier field_name)
      );
      pe "}"

    | _ ->
      ()
  end;
  pe "end";
  pn ()

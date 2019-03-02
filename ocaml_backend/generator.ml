open Xobl_parser
open Xobl_elaborate

module String_map = Types.String_map


let prim_get =
  let open Prim in function
  | Void -> "(fun _ _ -> ())"
  | Bool -> "X11_base.Get.bool"
  | Char | Byte | Card8 -> "X11_base.Get.byte"
  | Int8 -> "X11_base.Get.int8"
  | Int16 | Fd -> "X11_base.Get.int16"
  | Card16 -> "X11_base.Get.uint16"
  | Int32 -> "X11_base.Get.int32"
  | Card32 -> "X11_base.Get.uint32"
  | Xid -> "X11_base.Get.xid"
  | p -> Printf.kprintf invalid_arg "not implemented: %s" (Ident.of_prim p)


let prim_put_type =
  let open Prim in function
  | Void -> "unit"
  | Bool -> "bool"
  | Char | Byte -> "char"
  | Int8 | Card8 -> "int8"
  | Int16 | Card16 | Fd -> "int16"
  | Int32 | Card32 | Xid -> "int32"
  | Card64 -> "int64"
  | Float -> "float"
  | Double -> "double"

let prim_put =
  let open Prim in function
  | Void -> "(fun _ -> ())"
  | p -> "X11_base.Put." ^ prim_put_type p

let prim_put_int =
  let open Prim in function
  | Void -> "(fun _ -> ())"
  | p -> "X11_base.Put.int_as_" ^ prim_put_type p


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


let rec expression_str : P1_resolve.expression -> string =
  let p fmt = Printf.sprintf fmt in
  function
  | `Binop (op, e1, e2) ->
    p "%s (%s) (%s)" (binop_str op) (expression_str e1) (expression_str e2)
  | `Unop (op, e) ->
    p "%s (%s)" (unop_str op) (expression_str e)
  | `Field_ref n ->
    Ident.record_field n
  | `Param_ref (n, _) ->
    Ident.record_field n ^ " (* param *)"
  | `Enum_ref (_en, i) ->
    Ident.variant i
  | `Sum_of (f, None) ->
    p "List.fold_left ( + ) 0 %s" (Ident.record_field f)
  | `Sum_of (f, Some e) ->
    p "List.fold_left (fun acc curr' -> acc + (%s)) 0 %s" (expression_str e) (Ident.record_field f)
  | `Current_ref ->
    "curr'"
  | `Pop_count expr ->
    p "X11_base.pop_count (%s)" (expression_str expr)
  | `Value n ->
    string_of_int n
  | `Bit n ->
    string_of_int (1 lsl n)


let static_field_str : P2_fields.static_field -> string = function
  | `Pad p ->
    Printf.sprintf "(* pad: %s *)" (padding_str p)
  | `List_length (name, t, ls) ->
    Printf.sprintf "(* length field of %s: %s : %s *)"
      (Ident.record_field ls) (Ident.record_field name) (Ident.of_field_type t)
  | `Field (n, t) ->
    Printf.sprintf "%s : %s;" (Ident.record_field n) (Ident.of_field_type t)
  | `List (n, t, l) ->
    Printf.sprintf "%s : %s list; (* length: %s *)"
      (Ident.record_field n) (Ident.of_field_type t) (expression_str l)


let dynamic_field_str : P2_fields.dynamic_field -> string = function
  | `List_var (n, t) ->
    Printf.sprintf "%s : %s list; (* variable length *)"
      (Ident.record_field n) (Ident.of_field_type t)
  | #P2_fields.static_field as f ->
    static_field_str f


let request_field_str : P2_fields.request_field -> string = function
  | `Expr (n, typ, expr) ->
    Printf.sprintf "(* expr field: %s : %s = %s *)"
      (Ident.record_field n) (Ident.of_field_type typ) (expression_str expr)
  | #P2_fields.dynamic_field as f ->
    dynamic_field_str f


let is_hidden_request_field : P2_fields.request_field -> bool = function
  | `List_length _ | `Pad _ | `Expr _ -> false
  | _ -> true

let is_hidden_field : [> P2_fields.static_field ] -> bool = function
  | `List_length _ | `Pad _ -> false
  | _ -> true

let is_request_struct_empty (rq : P2_fields.request_field list) =
  List.length (List.filter is_hidden_request_field rq) < 1

let is_struct_empty (fields : [> P2_fields.static_field ] list) =
  List.length (List.filter is_hidden_field fields) < 1

let is_event : P2_fields.declaration -> bool = function
  | `Event _ | `Event_alias _ -> true
  | _ -> false

let is_error : P2_fields.declaration -> bool = function
  | `Error _ | `Error_alias _ -> true
  | _ -> false



(*
let switch_cond_str : Last_pass.cond -> string = function
  | `Bit_and e ->
    "fun cond' -> (" ^ expression_str e ^ ") land cond' <> 0"
  | `Eq e ->
    "fun cond' -> (" ^ expression_str e ^ ") = cond'"
*)


(* BUGS:
- handle empty (only padding) structs
*)

let generate (exts : P2_fields.extension String_map.t) out (ext : P2_fields.extension) =
  ignore exts;
  let fe fmt = Printf.fprintf out (fmt ^^ "\n") in
  let ps s = output_string out s in
  let pe s = output_string out s; output_char out '\n' in
  let pn () = output_char out '\n' in

  (* pe "(*****************************************************************************)";
  fe "module %s = struct" (Casing.caml ext.file_name);
  pe "(*****************************************************************************)"; *)
  (* pe "open X11_base";*)

  ext.version |> CCOpt.iter (fun (maj, min) ->
    fe "let version = (%d, %d)" maj min);
  ext.query_name |> CCOpt.iter (fun n ->
    fe "let query_extension_name = %S" n);
  pn ();

  ext.declarations |> List.iter begin function
    | `Alias (n, t) ->
      (* Should the types be opaque?
        If yes, what are the implications of that?
        Do we have to track usage to know whether they should ever be supplied
        by the users or can we get by just using the enums?
      *)
      fe "type %s = %s" (Ident.type_ n) (Ident.of_x_type t)


    | `X_id_union (name, _) ->
      (* XID unions are hardly used in the codebase, and outputting a variant
         type rather than simply aliasing them to XIDs would be way more
         trouble than it's worth. It's not strictly correct, but it's usable.
      *)
      fe "type %s = X11_base.xid" (Ident.type_ name)


    | `Enum (name, Parser.{ en_vals; en_bits }) ->
      (* We need to know a few things here:
        - output an enumeration? a bitmask? both? (solved, sort of)
        - which types do we need conversion functions to and from for?
        - for bitmasks, do we need to output conversion functions to the "val"
          items too? (probably not, but maybe we could provide some compare
          functions)
      *)
      let refs = Cache.enum_refs ext.file_name name in

      if List.length en_vals > 0 then begin
        if List.length en_bits > 0 then
          fe "type %s = [" (Ident.enum_vals name)
        else
          fe "type %s = [" (Ident.enum name);
        en_vals |> List.iter begin fun (name, _n) ->
          fe "  | %s" (Ident.variant name)
        end;
        pe "]"
      end;
      if List.length en_bits > 0 then begin
        fe "type %s = [" (Ident.mask_bits name);
        en_bits |> List.iter begin fun (name, _n) ->
          fe "  | %s" (Ident.variant name)
        end;
        pe "]"
      end;

      if refs.enums > 0 then begin
        match List.length en_vals, List.length en_bits with
        | 0, 0 -> assert false
        | 0, _ ->
          fe "type %s = %s" (Ident.enum name) (Ident.mask_bits name)
        | _, 0 ->
          ()
        | _, _ ->
          fe "type %s = [ %s | %s ]"
            (Ident.enum name) (Ident.enum_vals name) (Ident.mask_bits name)
      end;
      if refs.masks > 0 then
        if List.length en_vals > 0 then
          fe "type %s = (%s, %s) X11_base.mask"
            (Ident.mask name) (Ident.mask_bits name) (Ident.enum_vals name)
        else
          fe "type %s = %s list" (Ident.mask name) (Ident.mask_bits name);


    | `Union (name, fields) ->
      pe "(* union *)";
      fe "type %s = {" (Ident.type_ name);
      List.iter (fun x -> pe ("  " ^ static_field_str x)) fields;
      pe "}"


    | `Struct (struct_name, s) ->
      CCOpt.iter (fun (field_name, sw) ->
        let open P2_fields in
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
            let name = CCOpt.get_or ~default:item case.cs_name in
            assert (case.cs_switch = None);
            let t_name = Printf.sprintf "%s_%s_%s_case"
              (Casing.snake struct_name) (Casing.snake field_name) (Casing.snake name)
            in
            fe "type %s = {" t_name;
            List.iter (fun x -> pe ("  " ^ (static_field_str x))) case.cs_fields;
            pe "}";
            (name, t_name)
          )
        in
        fe "type %s_%s_switch = [" (Casing.snake struct_name) (Casing.snake field_name);
        List.iter (fun (name, t_name) ->
          fe "  | %s of %s" (Ident.variant name) t_name
        ) cases;
        pe "]"
      ) s.P2_fields.sf_switch;
      if List.length (List.filter is_hidden_field s.P2_fields.sf_fields) > 0 then begin
        fe "type %s = {" (Ident.type_ struct_name);
        s.P2_fields.sf_fields |> List.iter (fun x ->
          fe "  %s" (static_field_str x));
        s.P2_fields.sf_switch |> CCOpt.iter (fun (field_name, _sw) ->
          fe "  %s : %s_%s_switch;"
            (Ident.record_field field_name) (Casing.snake struct_name) (Casing.snake field_name)
        );
        pe "}"
      end else
        fe "type %s = unit" (Ident.type_ struct_name)


    | `Error (err_name, number, error) ->
      if List.length (List.filter is_hidden_field error.P2_fields.er_fields) > 0 then (
        fe "type %s_error = {" (Casing.snake err_name);
        error.P2_fields.er_fields |> List.iter (fun x ->
          fe "  %s" (static_field_str x));
        pe "}";
        fe "let parse_%s_error buf at : %s_error =" (Casing.snake err_name) (Casing.snake err_name);
        let offset = ref 4 in
        error.P2_fields.er_fields |> List.iter (function
          | `Pad Parser.{ pd_pad = `Bytes b; _ } ->
            fe "  (* padding: %d bytes *)" b;
            offset := !offset + b
          (* | `Field (name, ident) ->
            let len = Size.of_ident ~exts ext.file_name ident |> Size.get_bounded_exn in *)
          | `Field (name, P1_resolve.Prim (Types.Prim p)) ->
            let len = Size.of_prim p |> Size.to_int in
            fe "  let %s = %s buf (at + %d) in" (Ident.value name) (prim_get p) !offset;
            offset := !offset + len
          | _ ->
            fe "(* UNSUPPORTED FIELD *)"
        );
        ps "  { ";
        error.P2_fields.er_fields |> List.iter (function
          | `Field (name, _) | `List (name, _, _) ->
            ps (Ident.record_field name ^ "; ")
          | `List_length _ | `Pad _ -> ()
        );
        pe "}"
      ) else (
        fe "type %s_error = unit" (Casing.snake err_name);
        fe "let parse_%s_error _ _ : %s_error = ()" (Casing.snake err_name) (Casing.snake err_name)
      );
      fe "(* error alias n.%d *)" number


    | `Error_alias (err_name, number, old) ->
      begin match old with
      | Types.Id n ->
        fe "type %s_error = %s_error" (Casing.snake err_name) (Casing.snake n);
        fe "let parse_%s_error : string -> int -> %s_error =\n  parse_%s_error"
          (Casing.snake err_name) (Casing.snake err_name) (Casing.snake n)
      | Types.Ext_id (e, n) ->
        fe "type %s_error = %s.%s_error" (Casing.snake err_name)
          (Casing.caml e) (Casing.snake n);
        fe "let parse_%s_error : string -> int -> %s_error =\n  %s.parse_%s_error"
          (Casing.snake err_name) (Casing.snake err_name) (Casing.caml e) (Casing.snake n)
      end;
      fe "(* error n.%d *)" number


    | `Event (ev_name, number, P2_fields.{ ev_fields; _ }) ->
      if not (is_struct_empty ev_fields) then (
        fe "type %s_event = {" (Casing.snake ev_name);
        ev_fields |> List.iter (fun x ->
          fe "  %s" (static_field_str x)
        );
        pe "}"
      ) else (
        fe "type %s_event = unit" (Casing.snake ev_name)
      );
      fe "(* event n.%d *)" number


    | `Event_alias (ev_name, number, old) ->
      begin match old with
      | Types.Id n ->
        fe "type %s_event = %s_event" (Casing.snake ev_name)
          (Casing.snake n)
      | Types.Ext_id (e, n) ->
        fe "type %s_event = %s.%s_event" (Casing.snake ev_name)
          (Casing.caml e) (Casing.snake n)
      end;
      fe "(* event n.%d *)" number


    | `Generic_event (ev_name, number, P2_fields.{ gev_fields; _ }) ->
      if not (is_struct_empty gev_fields) then (
        fe "type %s_event = {" (Casing.snake ev_name);
        gev_fields |> List.iter (fun x ->
          fe "  %s" (dynamic_field_str x)
        );
        pe "}"
      ) else (
        fe "type %s_event = unit" (Casing.snake ev_name)
      );
      fe "(* generic event n.%d *)" number


    | `Event_struct (name, evs) ->
      fe "type %s = [" (Casing.snake name);
      evs |> List.iter (function
        | Types.Id n ->
          fe "  | %s of %s_event" (Ident.variant n) (Casing.snake n)
        | Types.Ext_id (e, n) ->
          fe "  | %s of %s.%s_event" (Ident.variant n) (Casing.caml e)
            (Casing.snake n)
      );
      pe "]\n(* event struct *)"


    | `Request (req_name, _number, P2_fields.{ rq_params; rq_reply; _ }) ->
      let args, params_prefix =
        match List.filter is_hidden_field rq_params.P2_fields.rf_fields with
        | [] ->
          "()", ""
        | f :: [] ->
          let arg = request_field_str f in
          Format.sprintf "(%s)" @@ String.sub arg 0 (String.index arg ';'), ""
        | _ ->
          fe "type %s_params = {" (Casing.snake req_name);
          rq_params.P2_fields.rf_fields |> List.iter (fun x ->
            fe "  %s" (request_field_str x)
          );
          pe "}";
          Format.sprintf "(params : %s_params)" (Casing.snake req_name), "params."
      in
      begin match rq_reply with
      | Some(P2_fields.{ re_fields; _ }) when not (is_struct_empty re_fields) ->
        fe "type %s_reply = {" (Casing.snake req_name);
        re_fields |> List.iter (fun x ->
          fe "  %s" (dynamic_field_str x)
        );
        pe "}"
      | Some _ | None -> ()
      end;
      fe "let[@warning \"-27-26\"] put_%s (buf : Buffer.t) %s ="
        (Casing.snake req_name) args;
      let offset = ref 0 in
      rq_params.P2_fields.rf_fields |> List.iter (function
        | `Pad Parser.{ pd_pad = `Bytes b; _ } ->
          fe "  X11_base.Put.padding buf %d;" b;
          offset := !offset + b
        | `Pad _ ->
          fe "  (* unsupported pad field *)"
        | `List_length (_, P1_resolve.Prim (Types.Prim p), ls_name) ->
          fe "  %s buf (List.length %s%s);" (prim_put_int p)
            params_prefix (Ident.func ls_name);
          let size = Size.of_prim p |> Size.get_bounded_exn in
          offset := !offset + size
        | `Field (field_name, P1_resolve.Prim (Types.Prim p)) ->
          fe "  %s buf %s%s;" (prim_put p)
            params_prefix (Ident.record_field field_name);
          let size = Size.of_prim p |> Size.get_bounded_exn in
          offset := !offset + size
        | _ ->
          pe "  (* not implemented *)"
      );
      pe "  ()"

  end;
  let events = List.filter is_event ext.declarations in
  if events <> [] then begin
    pe "type events = [";
    events |> List.iter begin function
      | `Event (ev_name, _, _)
      | `Event_alias (ev_name, _, _) ->
        fe "  | `%s of %s_event" (Casing.caml ev_name) (Casing.snake ev_name)
      | _ ->
        assert false
    end;
    pe "]"
  end;

  let errors = List.filter is_error ext.declarations in
  if errors <> [] then begin
    pe "type errors = [";
    errors |> List.iter begin function
      | `Error (err_name, _, _)
      | `Error_alias (err_name, _, _) ->
        fe "  | `%s of %s_error" (Casing.caml err_name) (Casing.snake err_name)
      | _ ->
        assert false
    end;
    pe "]";
    pe "let parse_errors buf at : int -> errors option = function";
    errors |> List.iter begin function
      | `Error (err_name, number, _)
      | `Error_alias (err_name, number, _) ->
        fe "  | %d ->" number;
        fe "    let err = parse_%s_error buf at in" (Casing.snake err_name);
        fe "    Some (`%s err)" (Casing.caml err_name);
      | _ ->
        assert false
    end;
    pe "  | _ -> None"
  end;

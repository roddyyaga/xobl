open Elaboratetree

module Casing = struct
  let is_last_of_acronym name pos len =
    if pos < len - 1 then
      let next = name.[pos + 1] in
      (next < '0' || next > '9') && Char.lowercase_ascii next = next
    else false

  let%test "is_last_of_acronym example" = is_last_of_acronym "DRI2Buffer" 5 10

  let%test "is_last_of_acronym example 2" =
    not (is_last_of_acronym "DRI2Buffer" 3 10)

  let snek name =
    let buf = Buffer.create 16 in
    let len = String.length name in
    StringLabels.iteri name ~f:(fun i -> function
      | c when i = 0 -> Buffer.add_char buf (Char.lowercase_ascii c)
      | 'A' .. 'Z' as c ->
          let prev = name.[i - 1] in
          if
            prev <> '_'
            && (Char.lowercase_ascii prev = prev
               || is_last_of_acronym name i len)
          then Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c)
      | c -> Buffer.add_char buf c);
    Buffer.contents buf

  let snake name =
    if name = "DECnet" then "decnet"
    else if String.lowercase_ascii name = name then name
    else if String.uppercase_ascii name = name then String.lowercase_ascii name
    else snek name

  let%test "C case" = snake "bigreq" = "bigreq"

  let%test "UPPERCASE" = snake "CHAR2B" = "char2b"

  let%test "snake_case" = snake "bits_per_rgb_value" = "bits_per_rgb_value"

  let%test "CamelCase" = snake "StaticGray" = "static_gray"

  let%test "weird case 1" = snake "GLXContext" = "glx_context"

  let%test "weird case 2" = snake "DECnet" = "decnet"

  let%test "weird case 3" = snake "Positive_HSync" = "positive_h_sync"

  let%test "weird case 4" = snake "DRI2Buffer" = "dri2_buffer"

  let%test "weird case 5" = snake "TestStriGS" = "test_stri_gs"

  let caml name = String.capitalize_ascii (snake name)
end

module Ident = struct
  let ocaml_reserved =
    [
      "and";
      "as";
      "asr";
      "assert";
      "begin";
      "class";
      "constraint";
      "do";
      "done";
      "downto";
      "else";
      "end";
      "exception";
      "external";
      "false";
      "for";
      "fun";
      "function";
      "functor";
      "if";
      "in";
      "include";
      "inherit";
      "initializer";
      "land";
      "lazy";
      "let";
      "lor";
      "lsl";
      "lsr";
      "lxor";
      "match";
      "method";
      "mod";
      "module";
      "open";
      "mutable";
      "new";
      "nonrec";
      "object";
      "of";
      "open";
      "open!";
      "or";
      "private";
      "rec";
      "sig";
      "struct";
      "then";
      "to";
      "true";
      "try";
      "type";
      "val";
      "virtual";
      "when";
      "while";
      "with";
    ]

  let sanitize_numbers name =
    if name.[0] >= '0' && name.[0] <= '9' then "D" ^ name else name

  let snake ?prefix ?suffix name =
    (match (prefix, suffix) with
    | Some prefix, Some suffix ->
        prefix ^ "_" ^ Casing.snake name ^ "_" ^ suffix
    | Some prefix, None -> prefix ^ "_" ^ Casing.snake name
    | None, Some suffix -> Casing.snake name ^ "_" ^ suffix
    | None, None ->
        let name = Casing.snake name in
        if List.mem name ocaml_reserved then name ^ "_" else name)
    |> sanitize_numbers

  let caml name = Casing.caml name |> sanitize_numbers
end

let list_sep sep f out = function
  | [] -> ()
  | head :: tail ->
      f out head;
      List.iter
        (fun item ->
          output_string out sep;
          f out item)
        tail

let list f out = List.iter (f out)

let gen_prim = function
  | Void -> "char"
  | Char -> "char"
  | Byte -> "char"
  | Bool -> "bool"
  | Int8 -> "int"
  | Int16 -> "int"
  | Int32 -> "int32"
  | Fd -> "file_descr"
  | Card8 -> "int"
  | Card16 -> "int"
  | Card32 -> "int32"
  | Card64 -> "int64"
  | Float -> "float"
  | Double -> "float"
  | Xid -> "xid"

let gen_to_int = function
  | Char | Byte -> Some "Char.code"
  | Bool -> Some "Bool.to_int"
  | Int8 | Int16 | Card8 | Card16 | Xid -> None
  | Fd -> Some "Obj.magic"
  | Int32 | Card32 -> Some "Int32.to_int"
  | Card64 -> Some "Int64.to_int"
  | (Void | Float | Double) as prim ->
      failwith
        (Format.asprintf "Can't generate to_int function for %a" pp_prim prim)

let gen_of_int = function
  | Char | Byte -> Some "Char.unsafe_chr"
  | Bool -> Some "(fun x -> x = 0)"
  | Int8 | Int16 | Card8 | Card16 | Xid -> None
  | Fd -> Some "Obj.magic"
  | Int32 | Card32 -> Some "Int32.of_int"
  | Card64 -> Some "Int64.of_int"
  | (Void | Float | Double) as prim ->
      failwith
        (Format.asprintf "Can't generate of_int function for %a" pp_prim prim)

let find_module_by_name xcbs name =
  List.find_map
    (function
      | Core decls when name = "xproto" -> Some (decls, [])
      | Extension { file_name; declarations; imports; _ } when file_name = name
        ->
          Some (declarations, imports)
      | _ -> None)
    xcbs
  |> Option.get

let find_prim name_to_find = function
  | Type_alias { name; type_ = Type_primitive prim } when name = name_to_find ->
      Some (`Prim prim)
  | Type_alias { name; type_ = Type_union _ } when name = name_to_find ->
      Some (`Prim Xid)
  | Type_alias { name; type_ = Type_ref ident } when name = name_to_find ->
      Some (`Ref ident)
  | _ -> None

let rec resolve_type_to_prim (current_module, xcbs) name =
  let decls, _ = find_module_by_name xcbs current_module in
  match List.find_map (find_prim name) decls with
  | Some (`Prim p) -> Some p
  | Some (`Ref { id_module; id_name }) ->
      resolve_type_to_prim (id_module, xcbs) id_name
  | None -> None

let resolve_as_prim (_, xcbs) = function
  | Type_primitive prim -> Some prim
  | Type_ref ident -> resolve_type_to_prim (ident.id_module, xcbs) ident.id_name
  | Type_union _ -> Some Xid

let gen_decode_prim = function
  | Void -> "failwith \"Can't decode void\""
  | Char -> "decode_char"
  | Byte -> "decode_char"
  | Bool -> "decode_bool"
  | Int8 -> "decode_int8"
  | Int16 -> "decode_int16"
  | Int32 -> "decode_int32"
  | Fd -> "decode_file_descr"
  | Card8 -> "decode_uint8"
  | Card16 -> "decode_uint16"
  | Card32 -> "decode_int32"
  | Card64 -> "decode_int64"
  | Float -> "decode_float"
  | Double -> "decode_float"
  | Xid -> "decode_xid"

let gen_encode_prim = function
  | Void -> "failwith \"Can't encode void\""
  | Char -> "encode_char"
  | Byte -> "encode_char"
  | Bool -> "encode_bool"
  | Int8 -> "encode_int8"
  | Int16 -> "encode_int16"
  | Int32 -> "encode_int32"
  | Fd -> "encode_file_descr"
  | Card8 -> "encode_uint8"
  | Card16 -> "encode_uint16"
  | Card32 -> "encode_int32"
  | Card64 -> "encode_int64"
  | Float -> "encode_float"
  | Double -> "encode_float"
  | Xid -> "encode_xid"

let gen_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Div -> "/"
  | Mul -> "*"
  | Bit_and -> "land"
  | Bit_left_shift -> "lsl"

module Env = Map.Make (String)
(** Map from variables in scope to their types.

   Needed because sometimes we have expressions like [length / 4]
   where we need to know [length] is an int32 so we can add a type
   conversion when generating code. *)

let rec gen_expr ~env out = function
  | Binop (op, e1, e2) ->
      Printf.fprintf out "(%a) %s (%a)" (gen_expr ~env) e1 (gen_binop op)
        (gen_expr ~env) e2
  | Unop (Bit_not, e) -> Printf.fprintf out "lnot (%a)" (gen_expr ~env) e
  | Field_ref name -> (
      let to_int =
        match Env.find_opt name env with
        | Some type_ -> gen_to_int type_
        | None -> None
      in
      match to_int with
      | Some to_int -> Printf.fprintf out "(%s %s)" to_int (Ident.snake name)
      | None -> output_string out (Ident.snake name))
  | Expr_value v -> Printf.fprintf out "%Ld" v
  | Expr_bit b -> Printf.fprintf out "1 lsl %d" b
  | Param_ref { param; type_ = _ } -> Printf.fprintf out "%s" param
  | Enum_ref _ -> failwith "Enum_ref"
  | Pop_count _ -> failwith "Pop_count"
  | Sum_of _ -> failwith "Sum_of"
  | List_element_ref -> failwith "List_element_ref"

let gen_ident ?(process_name = fun x -> Ident.snake x) (curr_module, _) out
    { id_module; id_name } =
  if curr_module = id_module then output_string out (process_name id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (process_name id_name)

let gen_decode_ident (curr_module, _) out { id_module; id_name } =
  if curr_module = id_module then
    output_string out (Ident.snake ~prefix:"decode" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"decode" id_name)

let gen_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_prim prim
  | Type_ref ident -> gen_ident ctx out ident
  | Type_union _ -> output_string out "xid"

let gen_decode_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_decode_prim prim
  | Type_ref ident -> gen_decode_ident ctx out ident
  | Type_union _ -> output_string out @@ gen_decode_prim Xid

let gen_enum_item out (name, _) = Printf.fprintf out "`%s" (Ident.caml name)

let gen_decode_enum_item out (name, v) =
  Printf.fprintf out "%Ld -> Some `%s" v (Ident.caml name)

let gen_encode_enum_item out (name, v) =
  Printf.fprintf out "`%s -> %Ld" (Ident.caml name) v

let gen_field_type ctx out = function
  | { ft_type; ft_allowed = None } -> gen_type ctx out ft_type
  | { ft_type = _; ft_allowed = Some (Allowed_enum enum) } ->
      gen_ident ctx out
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum" }
  | { ft_type = _; ft_allowed = Some (Allowed_mask mask) } ->
      gen_ident ctx out
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask" }
  | { ft_type; ft_allowed = Some (Allowed_alt_enum enum) } ->
      Printf.fprintf out "(%a, %a) alt" (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum" }
        (gen_type ctx) ft_type
  | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
      Printf.fprintf out "(%a, %a) alt" (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask" }
        (gen_type ctx) ft_type

let gen_field ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "%s : %a; " (Ident.snake name) (gen_field_type ctx)
        type_
  | Field_file_descriptor name ->
      Printf.fprintf out "%s : file_descr; " (Ident.snake name)
  | Field_optional { name; type_; _ } ->
      Printf.fprintf out "%s : %a option; " (Ident.snake name)
        (gen_field_type ctx) type_
  | Field_list { name; type_; _ } | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "%s : %a list; " (Ident.snake name)
        (gen_field_type ctx) type_
  | Field_variant { name; variant } ->
      Printf.fprintf out "%s : %a; " (Ident.snake name) (gen_ident ctx)
        { variant with id_name = Ident.snake variant.id_name ~suffix:"variant" }
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      ()

let gen_decode_field_type ctx out = function
  | { ft_type; ft_allowed = Some (Allowed_enum enum) } ->
      let p = resolve_as_prim ctx ft_type |> Option.get in
      Printf.fprintf out "decode_enum %a %s %a" (gen_decode_type ctx) ft_type
        (Option.value ~default:"(fun x -> x)" (gen_to_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum_of_int" }
  | { ft_type; ft_allowed = None } | { ft_type; ft_allowed = _ } ->
      gen_decode_type ctx out ft_type

let gen_convert_to_mask_or_enum_expr ~prefix ~suffix ~ctx ~name ~type_ out =
  let gen_decode_mask_expr ~type_ out ident =
    let to_int = Option.bind (resolve_as_prim ctx type_.ft_type) gen_to_int in
    let arg_to_decode =
      match to_int with
      | Some to_int -> Printf.sprintf "(%s %s)" to_int (Ident.snake name)
      | None -> Ident.snake name
    in
    Printf.fprintf out "(%a %s)"
      (gen_ident
         ~process_name:(Ident.snake ~prefix:"decode" ~suffix:"mask")
         ctx)
      ident arg_to_decode
  in
  let should_generate_prefix_and_suffix =
    match type_.ft_allowed with
    | Some (Allowed_mask _)
    | Some (Allowed_alt_enum _)
    | Some (Allowed_alt_mask _) ->
        true
    | _ -> false
  in
  if should_generate_prefix_and_suffix then Printf.fprintf out "%s" prefix;
  let () =
    match type_.ft_allowed with
    | Some (Allowed_mask ident) ->
        assert should_generate_prefix_and_suffix;
        gen_decode_mask_expr ~type_ out ident
    | Some (Allowed_alt_enum ident) ->
        assert should_generate_prefix_and_suffix;
        Printf.fprintf out "(match %a %s with | Some e -> E e | None -> T %s) "
          (gen_ident
             ~process_name:(fun x -> Ident.snake ~suffix:"enum_of_int" x)
             ctx)
          ident (Ident.snake name) (Ident.snake name)
    | Some (Allowed_alt_mask ident) ->
        assert should_generate_prefix_and_suffix;
        Printf.fprintf out "(match %a with | [] -> T %s | es -> E es) "
          (gen_decode_mask_expr ~type_)
          ident (Ident.snake name)
    | Some _ | None -> assert (not should_generate_prefix_and_suffix)
  in
  if should_generate_prefix_and_suffix then Printf.fprintf out "%s" suffix

let gen_decode_field ~ctx ~env out = function
  | Field { name; type_ } -> (
      Printf.fprintf out "let* %s, at = %a buf ~at in " (Ident.snake name)
        (gen_decode_field_type ctx)
        type_;
      gen_convert_to_mask_or_enum_expr
        ~prefix:(Printf.sprintf "let %s = " (Ident.snake name))
        ~suffix:" in " ~ctx ~name ~type_ out;
      match resolve_as_prim ctx type_.ft_type with
      | Some prim -> Env.add name prim env
      | None -> env)
  | Field_file_descriptor name ->
      Printf.fprintf out "let* %s, at = decode_file_descr buf ~at in "
        (Ident.snake name);
      Env.add name Fd env
  | Field_pad { pad = Pad_bytes n; _ } ->
      Printf.fprintf out "let at = at + %d in " n;
      env
  | Field_pad { pad = Pad_align n; _ } ->
      Printf.fprintf out "let at = at + ((at - orig) mod %d) in " n;
      env
  | Field_list_length { name; type_; expr } ->
      Printf.fprintf out "let* %s, at = %a buf ~at in " (Ident.snake name)
        (gen_decode_type ctx) type_;
      let env =
        let resolved = resolve_as_prim ctx type_ in
        match resolved with
        | None -> env
        | Some prim -> (
            match gen_to_int prim with
            | None -> Env.add name prim env
            | Some to_int ->
                Printf.fprintf out "let %s = %s %s in " (Ident.snake name)
                  to_int (Ident.snake name);
                (* OCaml int *)
                Env.add name Int16 env)
      in
      (* Probably this is just [let x = x] *)
      Printf.fprintf out "let %s = %a in" (Ident.snake name) (gen_expr ~env)
        expr;
      env
  | Field_list_simple { name; type_; length } ->
      Printf.fprintf out "let* %s, at = decode_list %a %s buf ~at in "
        (Ident.snake name)
        (gen_decode_field_type ctx)
        type_ (Ident.snake length);
      gen_convert_to_mask_or_enum_expr
        ~prefix:
          (Printf.sprintf "let %s = List.map (fun %s -> " (Ident.snake name)
             (Ident.snake name))
        ~suffix:(Printf.sprintf " ) %s in " (Ident.snake name))
        ~ctx ~name ~type_ out;
      (* For the list cases, we deliberately don't change the environment.
         This is because it is only needed for generating code for expressions,
         which only contain int-like variables and never lists.

         This saves us from having to add a [List] primitive type constructor
         which would never be used. *)
      env
  | Field_list { name; type_; length = Some expr } ->
      Printf.fprintf out "let* %s, at = decode_list %a (%a) buf ~at in "
        (Ident.snake name)
        (gen_decode_field_type ctx)
        type_ (gen_expr ~env) expr;
      env
  | Field_list { length = None; _ } ->
      failwith "Field_list with implicit length not supported yet"
  | Field_variant_tag { variant; type_ } ->
      Printf.fprintf out "let* %s, at = %a buf ~at in " (Ident.snake variant)
        (gen_decode_type ctx) type_;
      (* Probably don't need this in the env for expressions... *)
      env
  | Field_variant { name; variant } ->
      Printf.fprintf out "let* %s, at = %a buf ~at %s in " (Ident.snake name)
        (gen_ident
           ~process_name:(Ident.snake ~prefix:"decode" ~suffix:"variant")
           ctx)
        variant (Ident.snake name);
      (* See list case comment *)
      env
  | Field_expr _ | Field_optional _ | Field_optional_mask _ -> env

let name_of_field = function
  | Field { name; _ }
  | Field_file_descriptor name
  | Field_optional { name; _ }
  | Field_list { name; _ }
  | Field_list_simple { name; _ }
  | Field_variant { name; _ }
  | Field_expr { name; _ } ->
      Some name
  | _ -> None

let gen_decode_fields ?wrap_constructor ctx out fields =
  output_string out "let orig = at in ";
  let env = Env.empty in
  let rec decode_fields env fields_remaining =
    match fields_remaining with
    | [] -> ()
    | field :: fields_remaining ->
        let env = gen_decode_field ~ctx ~env out field in
        output_string out " ";
        decode_fields env fields_remaining
  in
  decode_fields env fields;
  Printf.fprintf out " ignore orig; Some (%s{ %s }, at)"
    (Option.value ~default:"" wrap_constructor)
    (List.filter_map name_of_field fields
    |> List.map Ident.snake |> String.concat "; ")

let gen_encode_ident (curr_module, _) out { id_module; id_name } =
  if curr_module = id_module then
    output_string out (Ident.snake ~prefix:"encode" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"encode" id_name)

let gen_encode_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_encode_prim prim
  | Type_ref ident -> gen_encode_ident ctx out ident
  | Type_union _ -> output_string out @@ gen_encode_prim Xid

let gen_encode_field_type ctx out = function
  | { ft_type; ft_allowed = Some (Allowed_enum ident) } -> (
      let p = resolve_as_prim ctx ft_type |> Option.get in
      Printf.fprintf out "encode_to_int %a " (gen_encode_type ctx) ft_type;
      match gen_of_int p with
      | Some of_int ->
          Printf.fprintf out "(fun x -> %s (%a x))" of_int
            (gen_ident
               ~process_name:(Ident.snake ~prefix:"int_of" ~suffix:"enum")
               ctx)
            ident
      | None ->
          Printf.fprintf out "(%a)"
            (gen_ident
               ~process_name:(Ident.snake ~prefix:"int_of" ~suffix:"enum")
               ctx)
            ident)
  | { ft_type; ft_allowed = Some (Allowed_mask ident) } -> (
      let p = resolve_as_prim ctx ft_type |> Option.get in
      Printf.fprintf out "encode_to_int %a " (gen_encode_type ctx) ft_type;
      match gen_of_int p with
      | Some of_int ->
          Printf.fprintf out "(fun x -> %s (%a x))" of_int
            (gen_ident
               ~process_name:(Ident.snake ~prefix:"int_of" ~suffix:"mask")
               ctx)
            ident
      | None ->
          Printf.fprintf out "(%a)"
            (gen_ident
               ~process_name:(Ident.snake ~prefix:"int_of" ~suffix:"mask")
               ctx)
            ident)
  | { ft_type; ft_allowed = Some (Allowed_alt_enum ident) } ->
      let p = resolve_as_prim ctx ft_type |> Option.get in
      (* TODO - not sure this is actually right: we want to encode the enum
         here not the other type, it is maybe just coincidence if they are the same *)
      Printf.fprintf out "encode_alt %a " (gen_encode_type ctx) ft_type;
      let () =
        match gen_of_int p with
        | Some to_int ->
            Printf.fprintf out "(fun x -> %s (%a x)) " to_int
              (gen_ident
                 ~process_name:(Ident.snake ~prefix:"int_of" ~suffix:"enum")
                 ctx)
              ident
        | None ->
            Printf.fprintf out "(%a) "
              (gen_ident
                 ~process_name:(Ident.snake ~prefix:"int_of" ~suffix:"enum")
                 ctx)
              ident
      in
      Printf.fprintf out "%a" (gen_encode_type ctx) ft_type
  | { ft_type; ft_allowed = Some (Allowed_alt_mask ident) } ->
      let p = resolve_as_prim ctx ft_type |> Option.get in
      (* TODO - same as enum case? *)
      Printf.fprintf out "encode_alt %a " (gen_encode_type ctx) ft_type;
      let () =
        match gen_of_int p with
        | Some to_int ->
            Printf.fprintf out "(fun x -> %s (%a x)) " to_int
              (gen_ident
                 ~process_name:(Ident.snake ~prefix:"int_of" ~suffix:"mask")
                 ctx)
              ident
        | None ->
            Printf.fprintf out "(%a) "
              (gen_ident
                 ~process_name:(Ident.snake ~prefix:"int_of" ~suffix:"mask")
                 ctx)
              ident
      in
      Printf.fprintf out "%a" (gen_encode_type ctx) ft_type
  | { ft_type; ft_allowed = None } -> gen_encode_type ctx out ft_type

let gen_encode_field ~ctx ~fields out = function
  | Field { name; type_ } ->
      Printf.fprintf out "%a buf %s; "
        (gen_encode_field_type ctx)
        type_ (Ident.snake name)
  | Field_optional { name; mask = _; bit = _; type_ } ->
      Printf.fprintf out {| Option.iter (fun %s -> %a buf %s) %s; |}
        (Ident.snake name)
        (gen_encode_field_type ctx)
        type_ (Ident.snake name) (Ident.snake name)
  | Field_optional_mask { name; type_ } ->
      let optional_fields_and_bits =
        List.filter_map
          (function
            | Field_optional { name; bit; _ } -> Some (name, bit) | _ -> None)
          fields
      in
      Printf.fprintf out {| let %s = 0 in |} (Ident.snake name);
      List.iter
        (fun (optional_field_name, bit) ->
          Printf.fprintf out
            {| let %s = if Option.is_some %s then %s lor (1 lsl %d) else %s in |}
            (Ident.snake name)
            (Ident.snake optional_field_name)
            (Ident.snake name) bit (Ident.snake name))
        optional_fields_and_bits;
      let mask_expression =
        match Option.bind (resolve_as_prim ctx type_) gen_of_int with
        | None -> Printf.sprintf "(%s)" (Ident.snake name)
        | Some of_int -> Printf.sprintf "(%s %s)" of_int (Ident.snake name)
      in
      Printf.fprintf out "%a buf %s; " (gen_encode_type ctx) type_
        mask_expression
  (* TODO - can expr matter? *)
  | Field_list_length { name; type_; expr = _ } ->
      let field_name =
        match
          List.filter_map
            (function
              | Field_list_simple { name = field_name; type_ = _; length; _ }
                when length = name ->
                  Some field_name
              | _ -> None)
            fields
        with
        | [] ->
            failwith
              (Printf.sprintf "Error: Length field %s apparently not used" name)
        (* TODO - runtime checking that lists are same length in cases where multiple
           fields use this length *)
        | hd :: _ -> hd
      in
      let length_expression =
        match Option.bind (resolve_as_prim ctx type_) gen_of_int with
        | None -> Printf.sprintf "(List.length %s)" (Ident.snake field_name)
        | Some of_int ->
            Printf.sprintf "(%s (List.length %s))" of_int
              (Ident.snake field_name)
      in
      Printf.fprintf out "%a buf %s ; " (gen_encode_type ctx) type_
        length_expression
  | Field_list_simple { name; type_; length = _ } ->
      Printf.fprintf out "encode_list (%a) buf %s; "
        (gen_encode_field_type ctx)
        type_ (Ident.snake name)
  | _ -> ()

let gen_encode_request_fields ~ctx ~fields out =
  output_string out "let buf = Buffer.create 16 in ";
  List.iter (gen_encode_field ~ctx ~fields out) fields;
  output_string out " buf"

let gen_variant_item ctx out { vi_name; vi_tag = _; vi_fields } =
  Printf.fprintf out "%s of { %a}" (Ident.caml vi_name)
    (list (gen_field ctx))
    vi_fields

let gen_named_arg ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "~(%s : %a) " (Ident.snake name) (gen_field_type ctx)
        type_
  | Field_file_descriptor name ->
      Printf.fprintf out "~(%s : file_descr) " (Ident.snake name)
  | Field_optional { name; type_; _ } ->
      Printf.fprintf out "?(%s : %a option) " (Ident.snake name)
        (gen_field_type ctx) type_
  | Field_list { name; type_; _ } | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "~(%s : %a list) " (Ident.snake name)
        (gen_field_type ctx) type_
  | Field_variant { name; variant } ->
      Printf.fprintf out "~(%s : %a) " (Ident.snake name) (gen_ident ctx)
        { variant with id_name = Ident.snake variant.id_name ~suffix:"variant" }
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      ()

let is_field_visible = function
  | Field _ | Field_file_descriptor _ | Field_optional _ | Field_list _
  | Field_list_simple _ | Field_variant _ ->
      true
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      false

let visible_fields fields =
  fields |> List.filter is_field_visible |> List.length

let gen_fields ctx out fields =
  if visible_fields fields = 0 then output_string out "unit"
  else Printf.fprintf out "{ %a}" (list (gen_field ctx)) fields

let gen_fields_record out fields =
  let gen_field_name out field =
    match name_of_field field with
    | Some name -> Printf.fprintf out "%s; " (Ident.snake name)
    | None -> ()
  in
  Printf.fprintf out "{ %a }" (list gen_field_name) fields

let gen_event_struct_field ctx out ident =
  Printf.fprintf out "%s of %a" (Ident.caml ident.id_name) (gen_ident ctx)
    { ident with id_name = Ident.snake ident.id_name ~suffix:"event" }

let mask_item out (name, _) = Printf.fprintf out "`%s" (Ident.caml name)

let mask_item_and_code out (name, code) =
  Printf.fprintf out "(`%s, %d)" (Ident.caml name) code

let mask_value_and_code out (name, code) =
  Printf.fprintf out "(`%s, %d)" (Ident.caml name) (Int64.to_int code)

let combine_enum_items_with_same_value items =
  let sorted = List.sort (fun (_, x) (_, y) -> Int64.compare x y) items in
  assert (items = sorted);
  let items_rev, last_item_opt =
    List.fold_left
      (fun (finished_items, latest_item_accum) item ->
        match latest_item_accum with
        | None -> (finished_items, Some item)
        | Some latest_item_accum ->
            if snd latest_item_accum = snd item then
              ( finished_items,
                Some (fst latest_item_accum ^ "_" ^ fst item, snd item) )
            else (latest_item_accum :: finished_items, Some item))
      ([], None) items
  in
  let items_rev =
    match last_item_opt with
    | Some item -> item :: items_rev
    | None -> items_rev
  in
  List.rev items_rev

let%expect_test "combine_enum_items" =
  let items =
    [
      ("foo", 0L);
      ("bar", 0L);
      ("baz", 7L);
      ("qux", 8L);
      ("quux", 8L);
      ("corge", 8L);
    ]
  in
  let combined = combine_enum_items_with_same_value items in
  print_endline ([%show: (string * int64) list] combined);
  [%expect {| [("foo_bar", 0L); ("baz", 7L); ("qux_quux_corge", 8L)] |}]

let gen_mask_flags ~name ~items out =
  Printf.fprintf out "let %s_flags = [\n" name;
  List.iter (Printf.fprintf out "%a;\n" mask_item_and_code) items;
  Printf.fprintf out "];;\n"

let gen_mask_values ~name ~values out =
  Printf.fprintf out "let %s_values = [\n" name;
  List.iter (Printf.fprintf out "%a;\n" mask_value_and_code) values;
  Printf.fprintf out "];;\n"

let gen_decode_mask ~name ~with_values out =
  if with_values then
    Printf.fprintf out
      {|let decode_%s i =
          match List.find_opt (fun (value, v) -> v = i) %s_values with
          | Some (value, _) -> V value
          | None -> F (List.filter_map (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None)
      %s_flags)|}
      name name name
  else
    Printf.fprintf out
      {|let decode_%s i =
          (List.filter_map (fun (flag, f) -> if f land (1 lsl i) <> 0 then Some flag else None) %s_flags)|}
      name name

let gen_int_of_mask ~name ~items ~values out =
  Printf.fprintf out "let %s =" (Ident.snake ~prefix:"int_of" name);
  let () =
    match values with
    | [] -> output_string out " fun flags ->\n"
    | _non_empty -> output_string out " function | F flags ->\n"
  in
  Printf.fprintf out
    "List.fold_left (fun acc flag -> let code = match flag with\n";
  List.iter
    (fun (flag, code) ->
      Printf.fprintf out "| `%s -> %d\n" (Ident.caml flag) code)
    items;
  Printf.fprintf out " in acc lor (1 lsl code)) 0 flags\n";
  List.iter
    (fun (value, code) ->
      Printf.fprintf out "| V `%s -> %d\n" (Ident.caml value)
        (Int64.to_int code))
    values

(** For structs and variant constructors *)
let gen_decode_fields_list ?wrap_constructor ~ctx ~name ~type_ ~fields ~toplevel
    out =
  Printf.fprintf out "let %s buf ~at : (%s * int) option = %a %s"
    (Ident.snake ~prefix:"decode" name)
    (Ident.snake type_)
    (gen_decode_fields ?wrap_constructor ctx)
    fields
    (if toplevel then ";;" else "in ")

let gen_decode_variant ~ctx ~type_name ~items out =
  Printf.fprintf out {| let %s buf ~at enum : (%s * int) option = |}
    (Ident.snake ~prefix:"decode" ~suffix:"variant" type_name)
    (Ident.snake ~suffix:"variant" type_name);
  list_sep "\n"
    (fun out { vi_name; vi_fields; vi_tag = _ } ->
      let wrap_constructor = Ident.caml vi_name ^ " " in
      gen_decode_fields_list ~wrap_constructor ~ctx ~name:vi_name
        ~type_:(Ident.snake type_name ~suffix:"variant")
        ~fields:vi_fields ~toplevel:false out)
    out items;
  Printf.fprintf out {| match enum with |};
  list_sep " \n | "
    (fun out { vi_name; vi_fields = _; vi_tag } ->
      Printf.fprintf out "%d -> %s buf ~at" (Int64.to_int vi_tag)
        (Ident.snake vi_name ~prefix:"decode"))
    out items;
  Printf.fprintf out
    {| | invalid -> failwith (Printf.sprintf "Invalid enum tag %%d for %s " invalid) |}
    (Ident.snake type_name)

let gen_declaration ctx out = function
  | Type_alias { name; type_ } ->
      Printf.fprintf out "type %s = %a;;\n" (Ident.snake name) (gen_type ctx)
        type_;
      Printf.fprintf out "let %s = %a;;"
        (Ident.snake ~prefix:"decode" name)
        (gen_decode_type ctx) type_;
      Printf.fprintf out "let %s = %a;;"
        (Ident.snake ~prefix:"encode" name)
        (gen_encode_type ctx) type_
  | Struct { name; fields } ->
      Printf.fprintf out "type %s = %a;;\n" (Ident.snake name) (gen_fields ctx)
        fields;
      gen_decode_fields_list ~ctx ~name ~type_:name ~fields ~toplevel:true out;
      Printf.fprintf out "let %s buf %a ="
        (Ident.snake ~prefix:"encode" name)
        gen_fields_record fields;
      List.iter (gen_encode_field ~ctx ~fields out) fields;
      output_string out " ignore buf"
  | Enum { name; items } ->
      let items = combine_enum_items_with_same_value items in
      Printf.fprintf out "type %s = [ %a ];;\n"
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_enum_item)
        items;
      Printf.fprintf out "let %s : int -> %s option = function %a | _ -> None;;"
        (Ident.snake name ~suffix:"enum_of_int")
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_decode_enum_item)
        items;
      Printf.fprintf out "let %s : %s -> int = function %a ;;"
        (Ident.snake name ~prefix:"int_of" ~suffix:"enum")
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_encode_enum_item)
        items
  | Mask { name; items; additional_values = [] } ->
      let name = Ident.snake name ~suffix:"mask" in
      Printf.fprintf out "type %s = [ %a ] list;;" name
        (list_sep " | " mask_item) items;
      gen_mask_flags ~name ~items out;
      gen_decode_mask ~name ~with_values:false out;
      gen_int_of_mask ~name ~items ~values:[] out
  | Mask { name; items; additional_values = values } ->
      let name = Ident.snake name ~suffix:"mask" in
      Printf.fprintf out "type %s = ([ %a ], [ %a ]) mask;;" name
        (list_sep " | " mask_item) items (list_sep " | " mask_item) values;
      gen_mask_flags ~name ~items out;
      gen_mask_values ~name ~values out;
      gen_decode_mask ~name ~with_values:true out;
      gen_int_of_mask ~name ~items ~values out
  | Variant { name; items } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"variant")
        (list_sep " | " (gen_variant_item ctx))
        items;
      gen_decode_variant ~ctx ~type_name:name ~items out
  | Event { name; fields; _ } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"event")
        (gen_fields ctx) fields
  | Error { name; fields; _ } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"error")
        (gen_fields ctx) fields
  | Request { name; fields; reply; _ } ->
      Option.iter
        (Printf.fprintf out "type %s = %a;;\n"
           (Ident.snake name ~suffix:"reply")
           (gen_fields ctx))
        reply;
      (*Printf.fprintf out "let %s %a() : %s Lwt.t =" (Ident.snake name)*)
      Printf.fprintf out "let %s %a() : Buffer.t =" (Ident.snake name)
        (list (gen_named_arg ctx))
        fields
      (*(if Option.is_some reply then Ident.snake name ~suffix:"reply"*)
      (*else "unit");*);
      gen_encode_request_fields ~ctx ~fields out
  | Event_copy { name; event; _ } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"event")
        (gen_ident ctx)
        { event with id_name = Ident.snake event.id_name ~suffix:"event" }
  | Error_copy { name; error; _ } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"error")
        (gen_ident ctx)
        { error with id_name = Ident.snake error.id_name ~suffix:"error" }
  | Event_struct { name; events } ->
      Printf.fprintf out "type %s = %a;;" (Ident.snake name)
        (list_sep " | " (gen_event_struct_field ctx))
        events

let gen_xcb xcbs out = function
  | Core decls ->
      output_string out "module[@warning \"-27\"] Xproto = struct\n";
      let ctx = ("xproto", xcbs) in
      (list_sep "\n" (gen_declaration ctx)) out decls;
      output_string out "\nend\n"
  | Extension { declarations; name = _; file_name; _ } ->
      Printf.fprintf out "module[@warning \"-27\"] %s = struct\n"
        (String.capitalize_ascii file_name);
      let ctx = (file_name, xcbs) in
      (list_sep "\n" (gen_declaration ctx)) out declarations;
      output_string out "\nend\n"

let gen out xcbs = List.iter (gen_xcb xcbs out) xcbs

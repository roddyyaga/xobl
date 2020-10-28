let mk_xidunion (name, types) = `Xidunion (name, types)

let mk_xidtype name = `Xidtype name

let mk_extension_info name file_name query_name multiword major minor =
  (name, file_name, query_name, multiword, major, minor)

let mk_core decls = `A decls

let mk_extension x = `B x

let try_parse_int s =
  int_of_string_opt s |> Option.to_result ~none:"failed to parse int"

let try_parse_int64 s =
  Int64.of_string_opt s |> Option.to_result ~none:"failed to parse int64"

(* ayy *)

open Parsetree

let mk_required_start_align al_align al_offset = { al_align; al_offset }

let mk_item_value value = Item_value value

let mk_item_bit bit = Item_bit bit

let mk_enum (name, (items, doc)) = Enum { name; items; doc }

let mk_allowed_events ae_extension ae_is_xge min max =
  { ae_extension; ae_is_xge; ae_opcode_range = { min; max } }

let mk_event_struct (name, allowed_events) =
  Event_struct { name; allowed_events }

let mk_import import = Import import

let mk_xid name = Xid name

let mk_xid_union (name, types) = Xid_union { name; types }

let mk_typedef name type_ = Typedef { name; type_ }

let mk_event_copy name event ev_number = Event_copy { name; event; ev_number }

let mk_error_copy name error er_number = Error_copy { name; error; er_number }

let mk_binop (op, (e1, e2)) = Binop (op, e1, e2)

let mk_unop (op, expr) = Unop (op, expr)

let mk_field_ref field = Field_ref field

let mk_param_ref (type_, param) = Param_ref { param; type_ }

let mk_enum_ref (enum, item) = Enum_ref { enum; item }

let mk_pop_count expr = Pop_count expr

let mk_sum_of (field, by_expr) = Sum_of { field; by_expr }

let mk_expr_value v = Expr_value v

let mk_expr_bit b = Expr_bit b

let mk_pad_bytes b = Pad_bytes b

let mk_pad_align n = Pad_align n

let mk_allowed_enum name = Allowed_enum name

let mk_allowed_mask name = Allowed_mask name

let mk_allowed_alt_enum name = Allowed_enum name

let mk_allowed_alt_mask name = Allowed_mask name

let mk_field_type ft_type ft_allowed = { ft_type; ft_allowed }

let mk_field_expr ((name, type_), expr) = Field_expr { name; type_; expr }

let mk_field_list ((name, type_), length) = Field_list { name; type_; length }

let mk_field_file_descriptor name = Field_file_descriptor name

let mk_field_pad (serialize, pad) = Field_pad { pad; serialize }

let mk_field (name, type_) = Field { name; type_ }

let mk_union (name, members) = Union { name; members }

let mk_event ((name, number, is_generic, no_sequence_number), (fields, doc)) =
  Event { name; number; is_generic; no_sequence_number; fields; doc }

let mk_error ((name, number), fields) = Error { name; number; fields }

let mk_switch (sw_name, (sw_cond, sw_cases)) = { sw_name; sw_cond; sw_cases }

let mk_case (cs_name, (cs_cond, cs_fields, cs_switch)) =
  { cs_name; cs_cond; cs_fields; cs_switch }

open Patche
open Patche.Xml
open Patche.Infix

type 'a parser = 'a Patche.Xml.t

let doc : doc parser = el_discard "doc" |> discard_with Doc

let required_start_align : required_start_align parser =
  el_a "required_start_align"
    (map2 mk_required_start_align (Attr.int "align") (Attr.int_o "offset"))

let import = el_b "import" data ->> mk_import

let xid = el_a "xidtype" (Attr.str "name") ->> mk_xid

let xid_union =
  el_ab "xidunion" Attr.(str "name") (many (el_b "type" data)) ->> mk_xid_union

let typedef =
  el_a "typedef" Attr.(map2 mk_typedef (str "newname") (str "oldname"))

let event_struct =
  el_ab "eventstruct" (Attr.str "name")
    (many
       (el_a "allowed"
          (map4 mk_allowed_events (Attr.str "extension") (Attr.bool "xge")
             (Attr.int "opcode-min") (Attr.int "opcode-max"))))
  ->> mk_event_struct

let enum_item : enum_item parser =
  el_b "value" data ->= try_parse_int64 ->> mk_item_value
  <|> el_b "bit" data ->= try_parse_int ->> mk_item_bit

let enum : declaration parser =
  el_ab "enum" (Attr.str "name")
    (many (el_ab "item" (Attr.str "name") enum_item) *<> opt doc)
  ->> mk_enum

let copy name mk =
  el_a name Attr.(map3 mk (str "name") (str "ref") (int "number"))

let event_copy = copy "eventcopy" mk_event_copy

let error_copy = copy "errorcopy" mk_error_copy

let binop = function
  | "+" ->
      Ok Add
  | "-" ->
      Ok Sub
  | "*" ->
      Ok Mul
  | "/" ->
      Ok Div
  | "&" ->
      Ok Bit_and
  | "<<" ->
      Ok Bit_left_shift
  | o ->
      Error ("invalid binop: " ^ o)

let unop = function "~" -> Ok Bit_not | o -> Error ("invalid unop: " ^ o)

let expression expression =
  let binop =
    el_ab "op" (Attr.str "op" ->= binop) (tuple2 expression expression)
  in
  let unop = el_ab "unop" (Attr.str "op" ->= unop) expression in
  let field_ref = el_b "fieldref" data in
  let param_ref = el_ab "paramref" (Attr.str "type") data in
  let enum_ref = el_ab "enumref" (Attr.str "ref") data in
  let pop_count = el_b "popcount" expression in
  let sum_of = el_ab "sumof" (Attr.str "ref") (opt expression) in
  let list_element_ref = el "listelement-ref" in
  let expr_value = el_b "value" data ->= try_parse_int64 in
  let expr_bit = el_b "bit" data ->= try_parse_int in
  choice
    [ binop ->> mk_binop
    ; unop ->> mk_unop
    ; field_ref ->> mk_field_ref
    ; param_ref ->> mk_param_ref
    ; enum_ref ->> mk_enum_ref
    ; pop_count ->> mk_pop_count
    ; sum_of ->> mk_sum_of
    ; list_element_ref |> discard_with List_element_ref
    ; expr_value ->> mk_expr_value
    ; expr_bit ->> mk_expr_bit ]

let expression : expression parser = fix expression

let field_attr =
  let open Attr in
  tuple2 (str "name")
    (map2 mk_field_type (str "type")
       (opt
          (choice
             [ str "enum" ->> mk_allowed_enum
             ; str "mask" ->> mk_allowed_mask
             ; str "altenum" ->> mk_allowed_alt_enum
             ; str "altmask" ->> mk_allowed_alt_mask ])))

let pad_attr =
  let open Attr in
  bool ~default:false "serialize"
  *<> (int "bytes" ->> mk_pad_bytes <|> int "align" ->> mk_pad_align)

let field =
  let exprfield = el_ab "exprfield" field_attr expression in
  let list = el_ab "list" field_attr (opt expression) in
  let fd = el_a "fd" (Attr.str "name") in
  let pad = el_a "pad" pad_attr in
  let field = el_a "field" field_attr in
  choice
    [ exprfield ->> mk_field_expr
    ; list ->> mk_field_list
    ; fd ->> mk_field_file_descriptor
    ; pad ->> mk_field_pad
    ; field ->> mk_field ]

let switch switch =
  let switch_case =
    el_ab "case"
      (* WRONG *) (Attr.str_o "name")
      (tuple3
         (many expression *< opt required_start_align)
         (many field) (opt switch))
    ->> mk_case
  in
  el_ab "switch" (Attr.str "name")
    (expression (* WRONG *) *< opt required_start_align *<> many switch_case)
  ->> mk_switch

let switch = fix switch

let event =
  let attrs =
    let open Attr in
    tuple4 (str "name") (int "number")
      (bool ~default:false "xge")
      (bool ~default:false "no-sequence-number")
  in
  el_ab "event" attrs (opt required_start_align *> many field *<> opt doc)
  ->> mk_event

let error =
  el_ab "error"
    (tuple2 (Attr.str "name") (Attr.int "number"))
    (opt required_start_align *> many field)
  ->> mk_error

let union = el_ab "union" (Attr.str "name") (many field) ->> mk_union

let struct_ = el_ab "struct" (Attr.str "name") (many field *<> opt switch)

let request_reply =
  opt required_start_align *> many field *<> opt switch *<> opt doc

let request =
  el_ab "request"
    (tuple3 (Attr.str "name") (Attr.int "opcode")
       (Attr.bool ~default:true "combine-adjacent"))
    (map5
       (fun _ fields switch reply doc -> (fields, switch, reply, doc))
       (opt required_start_align) (many field) (opt switch) request_reply
       (opt doc))

let declaration =
  choice
    [ import
    ; enum
    ; event_struct
    ; xid
    ; xid_union
    ; typedef
    ; event_copy
    ; error_copy
    ; union
    ; event ]

let core =
  let attrs = Attr.str "header" |> satisfies (( = ) "xproto") in
  el_ab "xcb" attrs (many declaration) ->> snd

let extension =
  let attrs =
    let open Attr in
    map6 mk_extension_info (str "extension-name") (str "header")
      (str "extension-xname")
      (bool "extension_multiword" ~default:false)
      (int "major-version") (int "minor-version")
  in
  el_ab "xcb" attrs (many declaration)

let xcb = dtd *> (core ->> mk_core <|> extension ->> mk_extension) *< eoi

let try_parse_int s =
  int_of_string_opt s |> Option.to_result ~none:"failed to parse int"

let try_parse_int64 s =
  Int64.of_string_opt s |> Option.to_result ~none:"failed to parse int64"

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

let mk_struct (name, (fields, switch)) = Struct { name; fields; switch }

let mk_request_reply ((fields, switch), doc) = { fields; switch; doc }

let mk_request ((name, opcode, combine_adjacent), (fields, switch, reply, doc))
    =
  Request { name; opcode; combine_adjacent; fields; switch; reply; doc }

let mk_core decls = Core decls

let mk_extension
    ((name, file_name, query_name, multiword, major, minor), declarations) =
  Extension
    { name
    ; file_name
    ; query_name
    ; multiword
    ; version = (major, minor)
    ; declarations
    }

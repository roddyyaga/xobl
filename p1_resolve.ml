(**
Pass 1, in which we resolve all that can be resolved.
*)

module X = Parser
module T = Types

(* Some types *)
type expression =
  [ `Binop of X.binop * expression * expression
  | `Unop of X.unop * expression
  | `Field_ref of string
  | `Param_ref of string * T.x_type
  | `Enum_ref of T.ident * string
  | `Sum_of of string * expression option
  | `Current_ref
  | `Pop_count of expression
  | `Value of int
  | `Bit of int ]

type field_type =
  | Prim of T.x_type
  | Enum of T.ident * T.x_type
  | Mask of T.ident * T.x_type
  | Enum_or of T.ident * T.x_type
  | Mask_or of T.ident * T.x_type

type static_field =
  [ `Pad of X.padding
  | `Field of string * field_type
  | `List of string * field_type * expression
  | `File_descriptor of string ]

type dynamic_field =
  [ static_field
  | `List_var of string * field_type ]

type request_field =
  [ dynamic_field
  | `Expr of string * field_type * expression ]


type cond =
  [ `Bit_and of expression
  | `Eq of expression ]

type switch =
  { sw_align : X.required_start_align option
  ; sw_cond  : cond
  ; sw_cases : case list }

and case =
  { cs_exprs  : expression list
  ; cs_name   : string option
  ; cs_align  : X.required_start_align option
  ; cs_fields : static_field list
  ; cs_switch : (string * switch) option }


(* Structs *)
type event =
  { ev_no_sequence_number : bool
  ; ev_align  : X.required_start_align option
  ; ev_fields : static_field list }

type generic_event =
  { gev_no_sequence_number : bool
  ; gev_align  : X.required_start_align option
  ; gev_fields : dynamic_field list }

type error =
  { er_align  : X.required_start_align option
  ; er_fields : static_field list }

type struct_fields =
  { sf_fields : static_field list
  ; sf_switch : (string * switch) option }

type request_fields =
  { rf_align  : X.required_start_align option
  ; rf_fields : request_field list
  ; rf_switch : (string * switch) option }

type reply =
  { re_align  : X.required_start_align option
  ; re_fields : dynamic_field list
  ; re_switch : (string * switch) option }

type request =
  { rq_combine_adjacent : bool
  ; rq_params : request_fields
  ; rq_reply  : reply option }


include Pass.Make(struct
  module Prev = P0_to_extension


  type common_p0_p1 =
    [ `Enum of string * X.enum ]

  type declaration =
    [ common_p0_p1

    | `Event_struct of string * T.ident list

    | `Event_alias of string * int * T.ident
    | `Error_alias of string * int * T.ident

    | `Alias of string * T.x_type
    | `X_id_union of string * T.ident list

    | `Struct of string * struct_fields
    | `Union of string * static_field list

    | `Event of string * int * event
    | `Generic_event of string * int * generic_event
    | `Error of string * int * error

    | `Request of string * int * request ]


  (* Boilerplate *)
  let rec resolve_in_expr ext : X.expression -> expression = function
    | `Param_ref (n, t) ->
      let t = Cache.lookup_type ext t in
      `Param_ref (n, t)

    | `Binop (op, e1, e2) ->
      let e1 = resolve_in_expr ext e1 in
      let e2 = resolve_in_expr ext e2 in
      `Binop (op, e1, e2)

    | `Unop (op, e) ->
      let e = resolve_in_expr ext e in
      `Unop (op, e)

    | `Sum_of (x, e) ->
      let e = Option.map (resolve_in_expr ext) e in
      `Sum_of (x, e)

    | `Pop_count e ->
      let e = resolve_in_expr ext e in
      `Pop_count e

    | `Enum_ref (enum, item) ->
      let enum = Cache.lookup_enum ext `Enum enum in
      `Enum_ref (enum, item)

    | `Field_ref _ | `Current_ref | `Value _ | `Bit _ as e ->
      e


  let resolve_field_type ext X.{ ft_allowed; ft_type } =
    let typ = Cache.lookup_type ext ft_type in
    match ft_allowed with
    | Some (`Enum name) ->
      let enum = Cache.lookup_enum ext `Enum name in
      Enum (enum, typ)

    | Some (`Mask name) ->
      let enum = Cache.lookup_enum ext `Mask name in
      Mask (enum, typ)

    | Some (`Alt_enum name) ->
      let enum = Cache.lookup_enum ext `Enum name in
      Enum_or (enum, typ)

    | Some (`Alt_mask name) ->
      let enum = Cache.lookup_enum ext `Mask name in
      Mask_or (enum, typ)

    | None ->
      Prim typ


  (* A bit of boilerplate for the static/dynamic/request fields *)
  let resolve_in_static_field ext : X.static_field -> static_field = function
    | `Field (name, t) ->
      let t = resolve_field_type ext t in
      `Field (name, t)
    | `List (name, t, expr) ->
      let t = resolve_field_type ext t in
      let expr = resolve_in_expr ext expr in
      `List (name, t, expr)
    | `Pad _ | (`File_descriptor _) as f ->
      f

  let resolve_in_dynamic_field ext : X.dynamic_field -> dynamic_field = function
    | `List_var (name, t) ->
      let t = resolve_field_type ext t in
      `List_var (name, t)
    | #X.static_field as f ->
      (resolve_in_static_field ext f :> dynamic_field)

  let resolve_in_request_field ext : X.request_field -> request_field = function
    | `Expr (name, t, expr) ->
      let t = resolve_field_type ext t in
      let expr = resolve_in_expr ext expr in
      `Expr (name, t, expr)
    | #X.dynamic_field as f ->
      (resolve_in_dynamic_field ext f :> request_field)


  (* Boilerplate for switch/case *)
  let resolve_in_cond ext : X.cond -> cond = function
    | `Bit_and expr ->
      let expr = resolve_in_expr ext expr in
      `Bit_and expr
    | `Eq expr ->
      let expr = resolve_in_expr ext expr in
      `Eq expr

  let rec resolve_in_switch ext : X.switch -> switch =
    fun { sw_align; sw_cond; sw_cases } ->
      let sw_cases = List.map (resolve_in_case ext) sw_cases in
      let sw_cond = resolve_in_cond ext sw_cond in
      { sw_align; sw_cond; sw_cases }

  and resolve_in_case ext : X.case -> case =
    fun { cs_exprs; cs_name; cs_align; cs_fields; cs_switch } ->
      let cs_fields = List.map (resolve_in_static_field ext) cs_fields in
      let cs_switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) cs_switch in
      let cs_exprs = List.map (resolve_in_expr ext) cs_exprs in
      { cs_exprs; cs_name; cs_align; cs_fields; cs_switch }


  (* Boilerplate for events/errors/requests. This would be so much cleaner
     with row types... *)
  let resolve_in_event ext : X.event -> event =
    fun { ev_no_sequence_number; ev_align; ev_fields } ->
      let ev_fields = List.map (resolve_in_static_field ext) ev_fields in
      { ev_no_sequence_number; ev_align; ev_fields }

  let resolve_in_generic_event ext : X.generic_event -> generic_event =
    fun { gev_no_sequence_number; gev_align; gev_fields } ->
      let gev_fields = List.map (resolve_in_dynamic_field ext) gev_fields in
      { gev_no_sequence_number; gev_align; gev_fields }

  let resolve_in_error ext : X.error -> error =
    fun { er_align; er_fields } ->
      let er_fields = List.map (resolve_in_static_field ext) er_fields in
      { er_align; er_fields }

  let resolve_in_struct_fields ext : X.struct_fields -> struct_fields =
    fun { sf_fields; sf_switch } ->
      let sf_fields = List.map (resolve_in_static_field ext) sf_fields in
      let sf_switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) sf_switch in
      { sf_fields; sf_switch }

  let resolve_in_request_fields ext : X.request_fields -> request_fields =
    fun ({ rf_align; rf_fields; rf_switch } : X.request_fields) ->
      let rf_fields = List.map (resolve_in_request_field ext) rf_fields in
      let rf_switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) rf_switch in
      { rf_align; rf_fields; rf_switch }

  let resolve_in_reply ext : X.reply -> reply =
    fun { re_align; re_fields; re_switch } ->
      let re_fields = List.map (resolve_in_dynamic_field ext) re_fields in
      let re_switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) re_switch in
      { re_align; re_fields; re_switch }


  let map _exts ext : Prev.declaration -> declaration = function
    | `X_id id ->
      `Alias (id, T.Prim Prim.Xid)

    | `Type_alias (name, t) ->
      let t = Cache.lookup_type ext t in
      `Alias (name, t)

    | `X_id_union (name, ids) ->
      let ids = List.map (fun t -> Cache.lookup_type_id ext t) ids in
      `X_id_union (name, ids)

    | `Struct (name, f) ->
      let f = resolve_in_struct_fields ext f in
      `Struct (name, f)

    | `Union (name, f) ->
      let f = List.map (resolve_in_static_field ext) f in
      `Union (name, f)

    | `Event (name, n, f) ->
      let f = resolve_in_event ext f in
      `Event (name, n, f)

    | `Generic_event (name, n, f) ->
      let f = resolve_in_generic_event ext f in
      `Generic_event (name, n, f)

    | `Error (name, n, f) ->
      let f = resolve_in_error ext f in
      `Error (name, n, f)

    | `Request (name, n, { rq_combine_adjacent; rq_params; rq_reply }) ->
      let rq_params = resolve_in_request_fields ext rq_params in
      let rq_reply = Option.map (resolve_in_reply ext) rq_reply in
      `Request (name, n, { rq_combine_adjacent; rq_params; rq_reply })

    | `Event_alias (name, no, old) ->
      let old = Cache.lookup_event ext old in
      `Event_alias (name, no, old)

    | `Error_alias (name, no, old) ->
      let old = Cache.lookup_error ext old in
      `Error_alias (name, no, old)

    | `Event_struct (name, evs) ->
      let resolve X.{ aev_extension; aev_opcode_range = (min, max) } =
        List.init (max - min + 1) (fun n -> n + 1)
        |> List.map (fun n -> n + min - 1)
        |> List.map (Cache.event_name_from_no ext aev_extension)
        |> Util.List'.filter_map
        |> List.rev
      in
      let evs = List.map resolve evs |> List.flatten in
      `Event_struct (name, evs)

    | #common_p0_p1 as d ->
      d
end)

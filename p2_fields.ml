module Prev = P1_resolve

type static_field =
  [ `Pad of Parser.padding
  | `Field of string * Prev.field_type
  | `List of string * Prev.field_type * Prev.expression
  | `List_length of string * Prev.field_type ]

type dynamic_field =
  [ static_field
  | `List_var of string * Prev.field_type ]

type request_field =
  [ dynamic_field
  | `Expr of string * Prev.field_type * Prev.expression ]


type switch =
  { sw_align : Parser.required_start_align option
  ; sw_cond  : Prev.cond
  ; sw_cases : case list }

and case =
  { cs_exprs  : Prev.expression list
  ; cs_name   : string option
  ; cs_align  : Parser.required_start_align option
  ; cs_fields : static_field list
  ; cs_switch : (string * switch) option }


type event =
  { ev_no_sequence_number : bool
  ; ev_align  : Parser.required_start_align option
  ; ev_fields : static_field list }

type generic_event =
  { gev_no_sequence_number : bool
  ; gev_align  : Parser.required_start_align option
  ; gev_fields : dynamic_field list }

type error =
  { er_align  : Parser.required_start_align option
  ; er_fields : static_field list }

type struct_fields =
  { sf_fields : static_field list
  ; sf_switch : (string * switch) option }

type request_fields =
  { rf_align  : Parser.required_start_align option
  ; rf_fields : request_field list
  ; rf_switch : (string * switch) option }

type reply =
  { re_align  : Parser.required_start_align option
  ; re_fields : dynamic_field list
  ; re_switch : (string * switch) option }

type request =
  { rq_combine_adjacent : bool
  ; rq_params : request_fields
  ; rq_reply  : reply option }


include Pass.Make(struct
  module Prev = P1_resolve

  type common_p1_p2 =
    [ `Enum of string * Parser.enum

    | `Event_struct of string * Types.ident list

    | `Event_alias of string * int * Types.ident
    | `Error_alias of string * int * Types.ident

    | `Alias of string * Types.x_type
    | `X_id_union of string * Types.ident list ]

  type declaration =
    [ common_p1_p2

    | `Struct of string * struct_fields
    | `Union of string * static_field list

    | `Event of string * int * event
    | `Generic_event of string * int * generic_event
    | `Error of string * int * error

    | `Request of string * int * request ]


  type res = [ `None | `One of string | `Invalid ]

  (* Determine whether the list length expression references a field
     that we can hide in the containing struct.

     FIXME: if the list field is in a switch, we have to search for the field
     in the containing struct as well.

     We could probably do param refs too, but that'd require us searching for
     the struct in which the current struct is contained and eliding it there.

     Popcount and sumof appear as list length fields in xkb and xinput (figures)
     so I guess we'll have to support them, but I'll leave them out for now
     because I don't feel like thinking much.
  *)
  let rec expr_stuff (n : res) : Prev.expression -> res = function
    | `Binop (_, e1, e2) ->
      let n = expr_stuff n e1 in
      expr_stuff n e2

    | `Unop (_, e) ->
      expr_stuff n e

    | `Field_ref field ->
      if n = `None then
        `One field
      else
        `Invalid

    | `Param_ref _
    | `Pop_count _
    | `Sum_of (_, _)
    | `Current_ref ->
      `Invalid

    | `Enum_ref _
    | `Value _ | `Bit _ ->
      n

  let in_fields (fields : [> static_field ] list) : [> static_field ] list =
    List.fold_right (fun field acc -> match field with
      | `List (_, _, expr) ->
        begin match expr_stuff `None expr with
        | `None | `Invalid -> acc
        | `One n ->
          acc |> List.map (function
            | `Field (f, typ) when f = n ->
              `List_length (n, typ)
            | d -> d
          )
        end

      | _ ->
        acc
    ) fields fields


  (* Turn file descriptor fields into normal fields with fd type *)
  let del_fd_static : Prev.static_field -> static_field = function
    | `File_descriptor name ->
      `Field (name, Prev.Prim (Types.Prim Prim.Fd))
    | `Pad _ | `Field _ | `List _ as d -> d

  let del_fd_dynamic : Prev.dynamic_field -> dynamic_field = function
    | `List_var _ as d -> d
    | #Prev.static_field as d -> (del_fd_static d :> dynamic_field)

  let del_fd_request : Prev.request_field -> request_field = function
    | `Expr _ as d -> d
    | #Prev.dynamic_field as d -> (del_fd_dynamic d :> request_field)


  let in_static_fields (fields : Prev.static_field list) : static_field list =
    let fields = List.map del_fd_static fields in
    in_fields fields

  let in_dynamic_fields (fields : Prev.dynamic_field list) : dynamic_field list =
    let fields = List.map del_fd_dynamic fields in
    in_fields fields

  let in_request_fields (fields : Prev.request_field list) : request_field list =
    let fields = List.map del_fd_request fields in
    in_fields fields


  let rec in_switch (sw : Prev.switch) =
    let cases = List.map in_case sw.sw_cases in
    { sw_align = sw.sw_align
    ; sw_cond  = sw.sw_cond
    ; sw_cases = cases }

  and in_case (cs : Prev.case) =
    let switch = Option.map (fun (n, sw) -> (n, in_switch sw)) cs.cs_switch in
    let fields = in_static_fields cs.cs_fields in
    { cs_exprs  = cs.cs_exprs
    ; cs_name   = cs.cs_name
    ; cs_align  = cs.cs_align
    ; cs_fields = fields
    ; cs_switch = switch }


  let map _exts _ext : Prev.declaration -> declaration = function
    | `Union (name, fields) ->
      let fields = List.map del_fd_static fields in
      `Union (name, fields)

    | `Struct (name, { sf_fields; sf_switch }) ->
      let sf_fields = in_static_fields sf_fields in
      let sf_switch = sf_switch |> Option.map (fun (sw_n, sw) -> (sw_n, in_switch sw)) in
      `Struct (name, { sf_fields; sf_switch })

    | `Event (name, no, ev) ->
      let fields = in_static_fields ev.ev_fields in
      let ev =
        { ev_no_sequence_number = ev.ev_no_sequence_number
        ; ev_align  = ev.ev_align
        ; ev_fields = fields }
      in
      `Event (name, no, ev)

    | `Generic_event (name, no, ev) ->
      let fields = in_dynamic_fields ev.gev_fields in
      let ev =
        { gev_no_sequence_number = ev.gev_no_sequence_number
        ; gev_align  = ev.gev_align
        ; gev_fields = fields }
      in
      `Generic_event (name, no, ev)

    | `Error (name, no, er) ->
      let fields = in_static_fields er.er_fields in
      let er =
        { er_align  = er.er_align
        ; er_fields = fields }
      in
      `Error (name, no, er)

    | `Request (name, no, req) ->
      let reply = req.rq_reply |> Option.map (fun (re : Prev.reply) ->
        let fields = in_dynamic_fields re.re_fields in
        let switch = re.re_switch |> Option.map (fun (n, sw) -> (n, in_switch sw)) in
        { re_align = re.re_align
        ; re_fields = fields
        ; re_switch = switch }
      ) in
      let params =
        let fields = in_request_fields req.rq_params.rf_fields in
        let switch = req.rq_params.rf_switch |> Option.map (fun (n, sw) -> (n, in_switch sw)) in
        { rf_align = req.rq_params.rf_align
        ; rf_fields = fields
        ; rf_switch = switch }
      in
      let req =
        { rq_combine_adjacent = req.rq_combine_adjacent
        ; rq_params = params
        ; rq_reply = reply }
      in
      `Request (name, no, req)

    | #common_p1_p2 as d ->
      d
end)

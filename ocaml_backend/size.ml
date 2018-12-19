open Xobl_parser
open Xobl_elaborate

type t =
  [ `Bounded of int
  | `Unbounded of int ]


module M = struct
  let lift (f : int -> int -> int) (n : t) (m : t) : t =
    match (n, m) with
    | `Bounded n, `Bounded m -> `Bounded (f n m)
    | `Bounded n, `Unbounded m
    | `Unbounded n, `Bounded m
    | `Unbounded n, `Unbounded m ->
      `Unbounded (f n m)

  let ( + ) = lift ( + )
  let max = lift max
  let list_max_by f =
    List.fold_left
      (fun acc x -> max acc (f x))
      (`Bounded 0)
  let list_sum_with f =
    List.fold_left (fun acc x -> acc + f x) (`Bounded 0)
end


let to_int : t -> int = function
  | `Bounded x | `Unbounded x -> x

let get_bounded : t -> int option = function
  | `Bounded x -> Some x
  | `Unbounded _ -> None

let get_bounded_exn : t -> int = function
  | `Bounded x -> x
  | `Unbounded _ ->
    invalid_arg "expected bounded size, got unbounded"


let of_prim : Prim.t -> t =
  let open Prim in function
  | Void -> `Bounded 0
  | Char | Byte | Bool | Int8 | Card8 -> `Bounded 1
  | Int16 | Card16 | Fd -> `Bounded 2
  | Int32 | Card32 | Float | Xid ->`Bounded  4
  | Card64 | Double -> `Bounded 8

let rec of_x_type ~exts ext_name : Types.x_type -> t =
  let open Types in function
  | Prim t -> of_prim t
  | Ref id -> of_ident ~exts ext_name id

and of_ident ~exts ext_name ident : t =
  let open Types in
  let ext_name, n = match ident with
    | Id n -> (ext_name, n)
    | Ext_id (ext_name, n) -> (ext_name, n)
  in
  let ext = String_map.find ext_name exts in
  CCList.find_map (of_item ~exts ext_name n) ext.declarations
  |> CCOpt.get_lazy (fun () ->
    Format.ksprintf invalid_arg "not found in %s: %s"
      ext_name n
  )

and of_pad : Parser.pad -> t = function
  | `Bytes n -> `Bounded n
  | `Align _ -> `Bounded 0

and of_padding : Parser.padding -> t =
  fun Parser.{ pd_pad; _ } ->
    of_pad pd_pad

and of_field_type ~exts ext_name : P1_resolve.field_type -> t =
  let open P1_resolve in function
  | Prim t
  | Enum (_, t) | Mask (_, t)
  | Enum_or (_, t) | Mask_or (_, t) ->
    of_x_type ~exts ext_name t

and of_static_field ~exts ext_name : P2_fields.static_field -> t =
  function
  | `Pad p -> of_padding p
  | `Field (_, typ)
  | `List_length (_, typ) -> of_field_type ~exts ext_name typ
  | `List _ ->
    `Unbounded 0

and of_dynamic_field ~exts ext_name : P2_fields.dynamic_field -> t =
  function
  | `List_var _ ->
    `Unbounded 0
  | #P2_fields.static_field as f ->
    of_static_field ~exts ext_name f

and of_request_field ~exts ext_name : P2_fields.request_field -> t =
  function
  | `Expr _ ->
    `Unbounded 0
  | #P2_fields.dynamic_field as f ->
    of_dynamic_field ~exts ext_name f

and of_switch_field ~exts ext_name = function
  | Some (_, s) -> of_switch ~exts ext_name s
  | None -> `Bounded 0

and of_switch ~exts ext_name : P2_fields.switch -> t =
  fun P2_fields.{ sw_cases; _ } ->
    M.list_max_by (of_case ~exts ext_name) sw_cases
    (* List.fold_left
      (fun acc x -> M.max acc (of_case ~exts ext_name x))
      (`Bounded 0)
      sw_cases *)

and of_case ~exts ext_name : P2_fields.case -> t =
  fun P2_fields.{ cs_fields; cs_switch; _ } ->
    M.(
      list_sum_with (of_static_field ~exts ext_name) cs_fields
      + of_switch_field ~exts ext_name cs_switch
    )

and of_struct_fields ~exts ext_name : P2_fields.struct_fields -> t =
  fun P2_fields.{ sf_fields; sf_switch } ->
    M.(
      list_sum_with (of_static_field ~exts ext_name) sf_fields
      + of_switch_field ~exts ext_name sf_switch
    )

and of_event ~exts ext_name : P2_fields.event -> t =
  fun P2_fields.{ ev_fields; _ } ->
    M.list_sum_with (of_static_field ~exts ext_name) ev_fields

and of_error ~exts ext_name : P2_fields.error -> t =
  fun P2_fields.{ er_fields; _ } ->
    M.list_sum_with (of_static_field ~exts ext_name) er_fields

and of_item ~exts ext_name id : P2_fields.declaration -> t option =
  function
  | `Event_alias (name, _, orig)
  | `Error_alias (name, _, orig) when name = id ->
    Some (of_ident ~exts ext_name orig)

  | `Alias (name, typ) when name = id ->
    Some (of_x_type ~exts ext_name typ)

  | `X_id_union (name, _) when name = id ->
    Some (of_prim Xid)

  | `Struct (name, fields) when name = id ->
    Some (of_struct_fields ~exts ext_name fields)

  | `Union (name, _static_fields) when name = id ->
    invalid_arg ("unsupported: trying to get size of union " ^ id)

  | `Event (name, _, ev) when name = id ->
    Some (of_event ~exts ext_name ev)

  | `Event_struct (name, events) when name = id ->
    Some (M.list_max_by (of_ident ~exts ext_name) events)

  | `Generic_event (name, _, _) when name = id ->
    invalid_arg ("unsupported: trying to get size of generic event " ^ id)

  | `Error (name, _, err) when name = id ->
    Some (of_error ~exts ext_name err)

  | `Request (name, _, _req) when name = id ->
    invalid_arg ("unsupported: trying to get size of request " ^ id)
    (* Some (of_request ~exts ext_name req) *)

  | `Enum (name, _) when name = id ->
    Format.ksprintf invalid_arg
      "trying to get size of enum %s.%s" ext_name id

  | `Event_alias _
  | `Error_alias _
  | `Alias _
  | `X_id_union _
  | `Struct _
  | `Union _
  | `Event _
  | `Event_struct _
  | `Generic_event _
  | `Error _
  | `Request _
  | `Enum _ ->
    None

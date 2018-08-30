(* Some types *)
type expression =
  [ `Binop of Parser.binop * expression * expression
  | `Unop of Parser.unop * expression
  | `Field_ref of string
  | `Param_ref of string * Types.x_type
  | `Enum_ref of Types.ident * string
  | `Sum_of of string * expression option
  | `Current_ref
  | `Pop_count of expression
  | `Value of int
  | `Bit of int ]

type field_type =
  | Prim of Types.x_type
  | Enum of Types.ident * Types.x_type
  | Mask of Types.ident * Types.x_type
  | Enum_or of Types.ident * Types.x_type
  | Mask_or of Types.ident * Types.x_type

  (*
type field_type =
  { ft_type : Types.x_type
  ; ft_enum : Parser.allowed_vals option }
  *)

type static_field =
  [ `Pad of Parser.padding
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
  { sw_align : Parser.required_start_align option
  ; sw_cond  : cond
  ; sw_cases : case list }

and case =
  { cs_exprs  : expression list
  ; cs_name   : string option
  ; cs_align  : Parser.required_start_align option
  ; cs_fields : static_field list
  ; cs_switch : (string * switch) option }


(* Structs *)
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


type declaration =
  [ `Enum of string * Parser.enum

  | `Event_struct of string * Parser.allowed_events list

  | `Event_alias of string * int * string
  | `Error_alias of string * int * string

  | `Alias of string * Types.x_type
  | `X_id_union of string * Types.ident list

  | `Struct of string * struct_fields
  | `Union of string * static_field list

  | `Event of string * int * event
  | `Generic_event of string * int * generic_event
  | `Error of string * int * error

  | `Request of string * int * request ]


type extension = declaration Types.extension

type prev_extension = P0_to_extension.declaration Types.extension

val pass : prev_extension Types.String_map.t -> extension Types.String_map.t

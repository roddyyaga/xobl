val variant : string -> string

val value : string -> string
val func : string -> string

val type_ : string -> string

val record : string -> string
val record_field : string -> string

val enum : string -> string
val mask : string -> string

val enum_vals : string -> string
val mask_bits : string -> string


open Xobl_elaborate

val of_ident : Types.ident -> string

val of_prim : Prim.t -> string

val of_x_type : Types.x_type -> string

val of_field_type : P1_resolve.field_type -> string

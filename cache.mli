val init : P0_to_extension.extension list -> unit

val lookup_type : 'a Types.extension -> string -> Types.x_type

val lookup_type_id : 'a Types.extension -> string -> Types.ident
(** Like [lookup_type], but doesn't look at primitive types *)

val lookup_enum : 'a Types.extension -> [ `Enum | `Mask ] -> string -> Types.ident
(** We want to gather usage statistics on the enums so that we can output
   the declarations that are needed, so we also need to know if the caller
   requests it as an enum or as mask. *)

type enum_refs = { enums : int; masks : int }

val enum_refs : unit -> enum_refs list

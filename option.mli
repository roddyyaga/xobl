val get : 'a option -> 'a
(** Unwrap the option.
    @raise Not_found if the argument is [None]. *)

val with_default : 'a -> 'a option -> 'a
(** [with_default def (Some x)] is [x],
 *  [with_default def None] is [def]. *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** [map f o] applies [f] to when [o]  *)

val bind : ('a -> 'b option) -> 'a option -> 'b option
(**  *)

val apply : ('a -> unit) -> 'a option -> unit
(**  *)

val with_default_lazy : 'a Lazy.t -> 'a option -> 'a
(** [with_default_lazy (lazy y) x] unwraps [x] when [x] is [Some _], and
    evaluates [y] otherwise. *)

val or_lazy : 'a option Lazy.t -> 'a option -> 'a option
(** [or_lazy (lazy y) x] returns [x] when [x] is [Some _], and
    forces [y] otherwise. *)

val of_result : ('a, 'b) result -> 'a option
(** Convert a [result] into an [option], dropping the argument of [Error]. *)

val to_result : 'b -> 'a option -> ('a, 'b) result
(**  *)

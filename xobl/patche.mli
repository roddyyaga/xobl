type error = string

type 't input = 't Lazy_list.t Lazy.t

type ('t, 'inp) parser = 'inp input -> ('t * 'inp input, error) Result.t

type ('t, 'inp) t = ('t, 'inp) parser


(** {2 Primitive parsers} *)
val return : 't -> ('t, 'inp) parser

val error : error -> ('t, 'inp) parser

val any : ('inp, 'inp) parser

val eoi : (unit, 'inp) parser


(** {2 Combinators} *)
val bind : ('a, 'inp) parser -> ('a -> ('b, 'inp) parser) -> ('b, 'inp) parser

val ( let& ) : ('a, 'inp) parser -> ('a -> ('b, 'inp) parser) -> ('b, 'inp) parser

val satisfies : ('inp -> bool) -> ('inp, 'inp) parser

val ( <|> ) : ('a, 'inp) parser -> ('a, 'inp) parser -> ('a, 'inp) parser


val apply : ('inp -> ('a, error) result) -> ('a, 'inp) parser

val opt : ('a, 'inp) parser -> ('a option, 'inp) parser

val many : ('a, 'inp) parser -> ('a list, 'inp) parser

val many1 : ('a, 'inp) parser -> ('a list, 'inp) parser


val tuple2 : ('a, 'inp) parser -> ('b, 'inp) parser -> ('a * 'b, 'inp) parser

val pipe : ('a -> 'b) -> ('a, 'inp) parser -> ('b, 'inp) parser

val pipe_result : ('a -> ('b, error) result) -> ('a, 'inp) parser -> ('b, 'inp) parser

val pipe2 :
  ('a, 'inp) parser ->
  ('b, 'inp) parser ->
  ('a -> 'b -> 'c) ->
  ('c, 'inp) parser


val discard_with : ('a, 'inp) parser -> 'b -> ('b, 'inp) parser

val ( &>> ) : ('a, 'inp) parser -> ('b, 'inp) parser -> ('a, 'inp) parser

val ( >>& ) : ('a, 'inp) parser -> ('b, 'inp) parser -> ('b, 'inp) parser


module Xml : sig
  val data : (string, Xmlm.signal) parser

  val dtd : (Xmlm.dtd, Xmlm.signal) parser

  val el_start : string -> 'a Attr.t -> ('a, Xmlm.signal) parser

  val el_start_empty : string -> (unit, Xmlm.signal) parser

  val el_end : (unit, Xmlm.signal) parser
end

type error =
  [`Error of string | `Custom of string | `Test_failed | `Choice_failed]

type ('t, 'inp, 'err) parser = 'inp -> ('t * 'inp, ([> error] as 'err)) Result.t

(* Basic parsers *)
val return : 't -> ('t, 'inp, 'err) parser

val error : 'err -> ('t, 'inp, 'err) parser

val satisfies :
  ('a -> bool) -> ('a, 'inp, 'err) parser -> ('a, 'inp, 'err) parser

val opt : ('a, 'inp, 'err) parser -> ('a option, 'inp, 'err) parser

val discard_with : 'a -> (_, 'inp, 'err) parser -> ('a, 'inp, 'err) parser

val discard : (_, 'inp, 'err) parser -> (unit, 'inp, 'err) parser

val from_result : ('t, 'err) result -> ('t, 'inp, 'err) parser

(* Monad *)
val bind :
     ('a, 'inp, 'err) parser
  -> ('a -> ('b, 'inp, 'err) parser)
  -> ('b, 'inp, 'err) parser

val map : ('a -> 'b) -> ('a, 'inp, 'err) parser -> ('b, 'inp, 'err) parser

val map_result :
     ('a -> ('b, 'err) result)
  -> ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser

(* Choice *)
val or_ :
  ('a, 'inp, 'err) parser -> ('a, 'inp, 'err) parser -> ('a, 'inp, 'err) parser

val choice : ('a, 'inp, 'err) parser list -> ('a, 'inp, 'err) parser

(* Recursion *)
val fix :
     (('a, 'inp, 'err) parser -> ('a, 'inp, 'err) parser)
  -> ('a, 'inp, 'err) parser

(* Sequencing *)
val discard_right :
  ('a, 'inp, 'err) parser -> (_, 'inp, 'err) parser -> ('a, 'inp, 'err) parser

val discard_left :
  (_, 'inp, 'err) parser -> ('a, 'inp, 'err) parser -> ('a, 'inp, 'err) parser

val many : ('a, 'inp, 'err) parser -> ('a list, 'inp, 'err) parser

val many1 : ('a, 'inp, 'err) parser -> ('a list, 'inp, 'err) parser

val tuple2 :
     ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser
  -> ('a * 'b, 'inp, 'err) parser

val tuple3 :
     ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser
  -> ('c, 'inp, 'err) parser
  -> ('a * 'b * 'c, 'inp, 'err) parser

val tuple4 :
     ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser
  -> ('c, 'inp, 'err) parser
  -> ('d, 'inp, 'err) parser
  -> ('a * 'b * 'c * 'd, 'inp, 'err) parser

val tuple6 :
     ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser
  -> ('c, 'inp, 'err) parser
  -> ('d, 'inp, 'err) parser
  -> ('e, 'inp, 'err) parser
  -> ('f, 'inp, 'err) parser
  -> ('a * 'b * 'c * 'd * 'e * 'f, 'inp, 'err) parser

val map2 :
     ('a -> 'b -> 'c)
  -> ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser
  -> ('c, 'inp, 'err) parser

val map3 :
     ('a -> 'b -> 'c -> 'd)
  -> ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser
  -> ('c, 'inp, 'err) parser
  -> ('d, 'inp, 'err) parser

val map4 :
     ('a -> 'b -> 'c -> 'd -> 'e)
  -> ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser
  -> ('c, 'inp, 'err) parser
  -> ('d, 'inp, 'err) parser
  -> ('e, 'inp, 'err) parser

val map5 :
     ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
  -> ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser
  -> ('c, 'inp, 'err) parser
  -> ('d, 'inp, 'err) parser
  -> ('e, 'inp, 'err) parser
  -> ('f, 'inp, 'err) parser

val map6 :
     ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
  -> ('a, 'inp, 'err) parser
  -> ('b, 'inp, 'err) parser
  -> ('c, 'inp, 'err) parser
  -> ('d, 'inp, 'err) parser
  -> ('e, 'inp, 'err) parser
  -> ('f, 'inp, 'err) parser
  -> ('g, 'inp, 'err) parser

module Infix : sig
  val ( let& ) :
       ('a, 'inp, 'err) parser
    -> ('a -> ('b, 'inp, 'err) parser)
    -> ('b, 'inp, 'err) parser

  val ( <|> ) :
       ('a, 'inp, 'err) parser
    -> ('a, 'inp, 'err) parser
    -> ('a, 'inp, 'err) parser

  val ( ->> ) : ('a, 'inp, 'err) parser -> ('a -> 'b) -> ('b, 'inp, 'err) parser

  val ( ->= ) :
       ('a, 'inp, 'err) parser
    -> ('a -> ('b, 'err) result)
    -> ('b, 'inp, 'err) parser

  val ( *> ) :
    (_, 'inp, 'err) parser -> ('a, 'inp, 'err) parser -> ('a, 'inp, 'err) parser

  val ( *< ) :
    ('a, 'inp, 'err) parser -> (_, 'inp, 'err) parser -> ('a, 'inp, 'err) parser

  val ( *<> ) :
       ('a, 'inp, 'err) parser
    -> ('b, 'inp, 'err) parser
    -> ('a * 'b, 'inp, 'err) parser
end

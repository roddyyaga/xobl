type ('t, 'inp) parser = 'inp -> ('t * 'inp, string) result

val return : 't -> ('t, 'inp) parser

val error : string -> ('t, 'inp) parser

val bind : ('a, 'inp) parser -> ('a -> ('b, 'inp) parser) -> ('b, 'inp) parser

val satisfies : ('a -> bool) -> ('a, 'inp) parser -> ('a, 'inp) parser

val or_ : ('a, 'inp) parser -> ('a, 'inp) parser -> ('a, 'inp) parser

val fix : (('a, 'inp) parser -> ('a, 'inp) parser) -> ('a, 'inp) parser

val choice : ('a, 'inp) parser list -> ('a, 'inp) parser

val opt : ('a, 'inp) parser -> ('a option, 'inp) parser

val many : ('a, 'inp) parser -> ('a list, 'inp) parser

val many1 : ('a, 'inp) parser -> ('a list, 'inp) parser

val tuple2 : ('a, 'inp) parser -> ('b, 'inp) parser -> ('a * 'b, 'inp) parser

val tuple3 :
     ('a, 'inp) parser
  -> ('b, 'inp) parser
  -> ('c, 'inp) parser
  -> ('a * 'b * 'c, 'inp) parser

val tuple4 :
     ('a, 'inp) parser
  -> ('b, 'inp) parser
  -> ('c, 'inp) parser
  -> ('d, 'inp) parser
  -> ('a * 'b * 'c * 'd, 'inp) parser

val map : ('a -> 'b) -> ('a, 'inp) parser -> ('b, 'inp) parser

val map2 :
     ('a -> 'b -> 'c)
  -> ('a, 'inp) parser
  -> ('b, 'inp) parser
  -> ('c, 'inp) parser

val map3 :
     ('a -> 'b -> 'c -> 'd)
  -> ('a, 'inp) parser
  -> ('b, 'inp) parser
  -> ('c, 'inp) parser
  -> ('d, 'inp) parser

val map4 :
     ('a -> 'b -> 'c -> 'd -> 'e)
  -> ('a, 'inp) parser
  -> ('b, 'inp) parser
  -> ('c, 'inp) parser
  -> ('d, 'inp) parser
  -> ('e, 'inp) parser

val map5 :
     ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
  -> ('a, 'inp) parser
  -> ('b, 'inp) parser
  -> ('c, 'inp) parser
  -> ('d, 'inp) parser
  -> ('e, 'inp) parser
  -> ('f, 'inp) parser

val map6 :
     ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
  -> ('a, 'inp) parser
  -> ('b, 'inp) parser
  -> ('c, 'inp) parser
  -> ('d, 'inp) parser
  -> ('e, 'inp) parser
  -> ('f, 'inp) parser
  -> ('g, 'inp) parser

val pipe_result :
  ('a -> ('b, string) result) -> ('a, 'inp) parser -> ('b, 'inp) parser

val discard_with : 'a -> (_, 'inp) parser -> ('a, 'inp) parser

val discard : (_, 'inp) parser -> (unit, 'inp) parser

val discard_right : ('a, 'inp) parser -> (_, 'inp) parser -> ('a, 'inp) parser

val discard_left : (_, 'inp) parser -> ('a, 'inp) parser -> ('a, 'inp) parser

val lift : ('a, string) result -> ('a, 'inp) parser

module Infix : sig
  val ( let& ) :
    ('a, 'inp) parser -> ('a -> ('b, 'inp) parser) -> ('b, 'inp) parser

  val ( <|> ) : ('a, 'inp) parser -> ('a, 'inp) parser -> ('a, 'inp) parser

  val ( ->> ) : ('a, 'inp) parser -> ('a -> 'b) -> ('b, 'inp) parser

  val ( ->= ) :
    ('a, 'inp) parser -> ('a -> ('b, string) result) -> ('b, 'inp) parser

  val ( *> ) : (_, 'inp) parser -> ('a, 'inp) parser -> ('a, 'inp) parser

  val ( *< ) : ('a, 'inp) parser -> (_, 'inp) parser -> ('a, 'inp) parser

  val ( *<> ) : ('a, 'inp) parser -> ('b, 'inp) parser -> ('a * 'b, 'inp) parser
end

module Attr : sig
  type input = Xmlm.attribute list

  type 'a t = ('a, input) parser

  val eoi : (unit, input) parser

  val str_o : string -> (string option, input) parser

  val str : string -> (string, input) parser

  val int_o : string -> (int option, input) parser

  val int : string -> (int, input) parser

  val bool_o : string -> (bool option, input) parser

  val bool : ?default:bool -> string -> (bool, input) parser

  val run : ('a, input) parser -> input -> ('a, string) result
end

module Xml : sig
  type input = Xmlm.signal Lazy_list.t Lazy.t

  type 'a t = ('a, input) parser

  val eoi : (unit, input) parser

  val dtd : (Xmlm.dtd, input) parser

  val data : (string, input) parser

  val el_start_a : string -> 'a Attr.t -> ('a, input) parser

  val el_start : string -> (unit, input) parser

  val el_end : (unit, input) parser

  val el : string -> (unit, input) parser

  val el_a : string -> 'a Attr.t -> ('a, input) parser

  val el_b : string -> ('a, input) parser -> ('a, input) parser

  val el_ab :
    string -> 'a Attr.t -> ('b, input) parser -> ('a * 'b, input) parser

  val el_discard : string -> (unit, input) parser

  val run : ('a, input) parser -> input -> ('a, string) result

  val inp : string -> input
end

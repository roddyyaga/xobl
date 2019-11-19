type error = string

type 'a parser

type input = Xmlm.attribute list

type 'a t = input -> ('a, error) result

val str_o : string -> string option parser

val str : string -> string parser

val int_o : string -> int option parser

val int : string -> int parser

val bool_o : string -> bool option parser

val bool_t : string -> bool parser

val bool_f : string -> bool parser

val bool : string -> bool parser

val empty : input -> (unit, error) result

val return : 'a parser
  -> input -> ('a, error) result

val tuple2 : 'a parser -> 'b parser
  -> input -> ('a * 'b, error) result

val tuple3 : 'a parser -> 'b parser -> 'c parser
  -> input -> ('a * 'b * 'c, error) result

val tuple4 : 'a parser -> 'b parser -> 'c parser -> 'd parser
  -> input -> ('a * 'b * 'c * 'd, error) result

val map : ('a -> 'b)
  -> 'a parser
  -> input -> ('b, error) result

val map2 : ('a -> 'b -> 'c)
  -> 'a parser -> 'b parser
  -> input -> ('c, error) result

val map3 : ('a -> 'b -> 'c -> 'd)
  -> 'a parser -> 'b parser -> 'c parser
  -> input -> ('d, error) result

val map4 : ('a -> 'b -> 'c -> 'd -> 'e)
  -> 'a parser -> 'b parser -> 'c parser -> 'd parser
  -> input -> ('e, error) result

val map5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f)
  -> 'a parser -> 'b parser -> 'c parser -> 'd parser -> 'e parser
  -> input -> ('f, error) result

val map6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
  -> 'a parser -> 'b parser -> 'c parser -> 'd parser -> 'e parser -> 'f parser
  -> input -> ('g, error) result

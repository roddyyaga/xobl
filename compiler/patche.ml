module type I = sig
  type 't input
end

module type O = sig
  type 't input

  type error = string

  type ('t, 'inp) parser = 'inp input -> ('t * 'inp input, error) Result.t

  val return : 't -> ('t, 'inp) parser

  val error : error -> ('t, 'inp) parser

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

  val pipe : ('a -> 'b) -> ('a, 'inp) parser -> ('b, 'inp) parser

  val pipe2 :
       ('a, 'inp) parser
    -> ('b, 'inp) parser
    -> ('a -> 'b -> 'c)
    -> ('c, 'inp) parser

  val pipe_result :
    ('a -> ('b, error) result) -> ('a, 'inp) parser -> ('b, 'inp) parser

  val discard_with : 'a -> (_, 'inp) parser -> ('a, 'inp) parser

  val discard_right : ('a, 'inp) parser -> (_, 'inp) parser -> ('a, 'inp) parser

  val discard_left : (_, 'inp) parser -> ('a, 'inp) parser -> ('a, 'inp) parser

  val lift : ('a, error) Result.t -> ('a, error) parser

  module Infix : sig
    val ( let& ) :
      ('a, 'inp) parser -> ('a -> ('b, 'inp) parser) -> ('b, 'inp) parser

    val ( <|> ) : ('a, 'inp) parser -> ('a, 'inp) parser -> ('a, 'inp) parser

    val ( &>> ) : ('a, 'inp) parser -> (_, 'inp) parser -> ('a, 'inp) parser

    val ( >>& ) : (_, 'inp) parser -> ('a, 'inp) parser -> ('a, 'inp) parser

    val ( &>>& ) :
      ('a, 'inp) parser -> ('b, 'inp) parser -> ('a * 'b, 'inp) parser

    val ( => ) : ('a, 'inp) parser -> ('a -> 'b) -> ('b, 'inp) parser
  end
end

module Make (P : I) : O with type 't input = 't P.input = struct
  type 't input = 't P.input

  type error = string

  type ('t, 'inp) parser = 'inp input -> ('t * 'inp input, error) Result.t

  let return v inp = Ok (v, inp)

  let error msg _ = Error msg

  let bind parser f inp =
    match parser inp with
    | Ok (result, rest) ->
        f result rest
    | Error _ as e ->
        e

  let ( let& ) = bind

  let satisfies test p =
    let& res = p in
    if test res then return res else error "test failed"

  let or_ p1 p2 inp = match p1 inp with Ok _ as ok -> ok | Error _ -> p2 inp

  let rec fix p inp = p (fix p) inp

  let choice ps inp =
    let rec loop = function
      | [] ->
          Error "empty"
      | p :: ps -> (
        match p inp with Ok _ as ok -> ok | Error _ -> loop ps )
    in
    loop ps

  let tuple2 p1 p2 =
    let& res1 = p1 in
    let& res2 = p2 in
    return (res1, res2)

  let tuple3 p1 p2 p3 =
    let& res1 = p1 in
    let& res2 = p2 in
    let& res3 = p3 in
    return (res1, res2, res3)

  let tuple4 p1 p2 p3 p4 =
    let& res1 = p1 in
    let& res2 = p2 in
    let& res3 = p3 in
    let& res4 = p4 in
    return (res1, res2, res3, res4)

  let map f p =
    let& res = p in
    return (f res)

  let map2 f p1 p2 =
    let& res1 = p1 in
    let& res2 = p2 in
    return (f res1 res2)

  let map3 f p1 p2 p3 =
    let& res1 = p1 in
    let& res2 = p2 in
    let& res3 = p3 in
    return (f res1 res2 res3)

  let map4 f p1 p2 p3 p4 =
    let& res1 = p1 in
    let& res2 = p2 in
    let& res3 = p3 in
    let& res4 = p4 in
    return (f res1 res2 res3 res4)

  let map5 f p1 p2 p3 p4 p5 =
    let& res1 = p1 in
    let& res2 = p2 in
    let& res3 = p3 in
    let& res4 = p4 in
    let& res5 = p5 in
    return (f res1 res2 res3 res4 res5)

  let map6 f p1 p2 p3 p4 p5 p6 =
    let& res1 = p1 in
    let& res2 = p2 in
    let& res3 = p3 in
    let& res4 = p4 in
    let& res5 = p5 in
    let& res6 = p6 in
    return (f res1 res2 res3 res4 res5 res6)

  let pipe f p =
    let& res = p in
    return (f res)

  let pipe2 p1 p2 f =
    let& res1 = p1 in
    let& res2 = p2 in
    return (f res1 res2)

  let pipe_result f p inp =
    match p inp with
    | Ok (v, rest) ->
        f v |> Result.map (fun v -> (v, rest))
    | Error _ as err ->
        err

  let opt p inp =
    match p inp with
    | Ok (v, rest) ->
        Ok (Some v, rest)
    | Error _ ->
        Ok (None, inp)

  let many p inp =
    let rec loop acc inp =
      match p inp with
      | Ok (v, rest) ->
          if rest == inp then
            invalid_arg
              "infinite loop detected in many: parser did not consume anything"
          else loop (v :: acc) rest
      | Error _ ->
          Ok (List.rev acc, inp)
    in
    loop [] inp

  let many1 p = pipe2 p (many p) (fun hd tl -> hd :: tl)

  let discard_with v p =
    let& _ = p in
    return v

  let discard_right p1 p2 =
    let& res1 = p1 in
    let& _ = p2 in
    return res1

  let discard_left p1 p2 =
    let& _ = p1 in
    p2

  let lift = function Ok v -> return v | Error msg -> error msg

  module Infix = struct
    let ( let& ) = bind

    let ( <|> ) = or_

    let ( >>& ) = discard_left

    let ( &>> ) = discard_right

    let ( &>>& ) = tuple2

    let ( => ) p f = pipe f p
  end
end

module Lazy_list_patche = struct
  include Make (struct
    type 't input = 't Lazy_list.t Lazy.t
  end)

  let any inp =
    match Lazy.force inp with
    | Lazy_list.Cons (v, rest) ->
        Ok (v, rest)
    | Lazy_list.Nil ->
        Error "empty"

  let eoi inp =
    match Lazy.force inp with
    | Lazy_list.Nil ->
        Ok ((), inp)
    | Lazy_list.Cons _ ->
        Error "not eoi"

  let apply f inp =
    match any inp with
    | Ok (v, rest) ->
        f v |> Result.map (fun v -> (v, rest))
    | Error _ as err ->
        err
end

module Attr = struct
  let list_remove_opt f =
    let rec loop prev = function
      | [] ->
          (None, List.rev prev)
      | el :: rest -> (
        match f el with
        | Some el ->
            (Some el, List.rev_append prev rest)
        | None ->
            loop (el :: prev) rest )
    in
    loop []

  let named name = function ("", n), v when n = name -> Some v | _ -> None

  let optional name ls = Ok (list_remove_opt (named name) ls)

  let required name ls =
    match list_remove_opt (named name) ls with
    | Some v, rest ->
        Ok (v, rest)
    | None, _ ->
        Error (Printf.sprintf "couldn't find attribute '%s'" name)

  include Make (struct
    type _ input = Xmlm.attribute list
  end)

  let eoi = function [] -> Ok () | _ -> Error "trailing elements detected"

  type 'a t = Xmlm.attribute list -> ('a, string) result

  let str_o = optional

  let str = required

  let opt_conv v f err =
    match v with
    | None ->
        return None
    | Some v -> (
      match f v with Some i -> return (Some i) | None -> error (err v) )

  open Infix

  let int_o name =
    let& v = optional name in
    opt_conv v int_of_string_opt
      (Printf.sprintf "failed to convert attribute '%s' to int: %S" name)

  let int name =
    let& v = required name in
    int_of_string_opt v
    |> Option.to_result
         ~none:
           (Printf.sprintf "failed to convert attribute '%s' to int: %S" name v)
    |> lift

  let bool_o name =
    let& v = optional name in
    opt_conv v bool_of_string_opt
      (Printf.sprintf "failed to convert attribute '%s' to bool: %S" name)

  (*

  let bool_f name attrs =
      bool_o name attrs
        |> Result.map (fun (v, rest) -> Option.value ~default:false v, rest)


  let bool_t name attrs =
      bool_o name attrs
        |> Result.map (fun (v, rest) -> Option.value ~default:true v, rest)
        *)

  let bool ?default name attrs =
    match (bool_o name attrs, default) with
    | Ok (Some b, rest), _ | Ok (None, rest), Some b ->
        Ok (b, rest)
    | Ok (None, _), None ->
        Error (Printf.sprintf "couldn't find attribute '%s'" name)
    | (Error _ as err), _ ->
        err

  let ( let* ) = Result.bind

  let run p attrs =
    let* res, attrs = p attrs in
    let* () = eoi attrs in
    Ok res
end

module Xml : sig
  val data : (string, Xmlm.signal) Lazy_list_patche.parser

  val dtd : (Xmlm.dtd, Xmlm.signal) Lazy_list_patche.parser

  val el : string -> (unit, Xmlm.signal) Lazy_list_patche.parser

  val el_a : string -> 'a Attr.t -> ('a, Xmlm.signal) Lazy_list_patche.parser

  val el_b :
       string
    -> ('a, Xmlm.signal) Lazy_list_patche.parser
    -> ('a, Xmlm.signal) Lazy_list_patche.parser

  val el_ab :
       string
    -> 'a Attr.t
    -> ('b, Xmlm.signal) Lazy_list_patche.parser
    -> ('a * 'b, Xmlm.signal) Lazy_list_patche.parser
end = struct
  open Lazy_list_patche
  open Lazy_list_patche.Infix

  let data =
    apply @@ function `Data data -> Ok data | _ -> Error "expected `Data"

  let dtd = apply @@ function `Dtd dtd -> Ok dtd | _ -> Error "expected `Dtd"

  let el_start_a name attr =
    apply
    @@ function
    | `El_start ((_, n), attrs) when n = name ->
        attr attrs
    | _ ->
        Error ("expected `El_start: " ^ name)

  let el_start name =
    apply
    @@ function
    | `El_start ((_, n), []) when n = name ->
        Ok ()
    | `El_start ((_, n), _) when n = name ->
        Error "expected `El_start with no attributes"
    | _ ->
        Error ("expected `El_start: " ^ name)

  let el_end =
    apply
    @@ function
    | `El_end ->
        Ok ()
    | `El_start _ ->
        Error "expected `El_end, received `El_start"
    | `Data data ->
        Error ("expected `El_end, received `Data \"" ^ data ^ "\"")
    | `Dtd _ ->
        Error "expect `El_end, received `Dtd"

  let el_ab name attr body = el_start_a name attr &>>& body &>> el_end

  let el_a name attr = el_start_a name attr &>> el_end

  let el_b name body = el_start name >>& body &>> el_end

  let el name = el_start name >>& el_end
end

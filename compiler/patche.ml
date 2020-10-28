type ('t, 'inp) parser = 'inp -> ('t * 'inp, string) Result.t

let return v inp = Ok (v, inp)

let error msg _ = Error msg

let bind parser f inp =
  match parser inp with Ok (result, rest) -> f result rest | Error _ as e -> e

let ( let& ) = bind

let ( let* ) = Result.bind

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

let tuple6 p1 p2 p3 p4 p5 p6 =
  let& res1 = p1 in
  let& res2 = p2 in
  let& res3 = p3 in
  let& res4 = p4 in
  let& res5 = p5 in
  let& res6 = p6 in
  return (res1, res2, res3, res4, res5, res6)

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

let many1 p = map2 (fun hd tl -> hd :: tl) p (many p)

let discard_with v p =
  let& _ = p in
  return v

let discard p = discard_with () p

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

  (* In ascending order of precedence *)

  let ( <|> ) = or_

  let ( ->> ) p f = map f p

  let ( ->= ) p f = pipe_result f p

  let ( *> ) = discard_left

  let ( *< ) = discard_right

  let ( *<> ) = tuple2
end

module Attr = struct
  type input = Xmlm.attribute list

  type 'a t = ('a, input) parser

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

  let eoi = function
    | [] ->
        Ok ((), [])
    | _ ->
        Error "trailing elements detected"

  let str = required

  let str_o = optional

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

  let bool ?default name attrs =
    match (bool_o name attrs, default) with
    | Ok (Some b, rest), _ | Ok (None, rest), Some b ->
        Ok (b, rest)
    | Ok (None, _), None ->
        Error (Printf.sprintf "couldn't find attribute '%s'" name)
    | (Error _ as err), _ ->
        err

  let run p attrs =
    let* res, attrs = p attrs in
    let* (), _ = eoi attrs in
    Ok res
end

module Xml = struct
  type input = Xmlm.signal Lazy_list.t Lazy.t

  type 'a t = ('a, input) parser

  open Infix

  let any inp =
    match Lazy.force inp with
    | Lazy_list.Cons (v, rest) ->
        Ok (v, rest)
    | Lazy_list.Nil ->
        Error "empty"

  let peek inp =
    match Lazy.force inp with
    | Lazy_list.Cons (v, _) ->
        Ok (v, inp)
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

  let data =
    apply @@ function `Data data -> Ok data | _ -> Error "expected `Data"

  let dtd = apply @@ function `Dtd dtd -> Ok dtd | _ -> Error "expected `Dtd"

  let el_start_any =
    apply @@ function `El_start _ -> Ok () | _ -> Error "expected `El_start"

  let el_start_a name attr =
    apply
    @@ function
    | `El_start ((_, n), attrs) when n = name ->
        (Attr.run attr) attrs
    | `El_start ((_, n), _) ->
        Error ("expected <" ^ name ^ "> but got " ^ n)
    | `El_end ->
        Error "expected `El_start, got `El_end"
    | `Data _ ->
        Error "expected `El_start, got `Data"
    | `Dtd _ ->
        Error "expected `El_start, got `Dtd"

  let el_start name =
    apply
    @@ function
    | `El_start ((_, n), []) when n = name ->
        Ok ()
    | `El_start ((_, n), _) when n = name ->
        Error "expected `El_start with no attributes"
    | `El_start ((_, n), _) ->
        Error ("expected <" ^ name ^ "> but got " ^ n)
    | `El_end ->
        Error "expected `El_start, got `El_end"
    | `Data _ ->
        Error "expected `El_start, got `Data"
    | `Dtd _ ->
        Error "expected `El_start, got `Dtd"

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

  let el_ab name attr body = el_start_a name attr *<> body *< el_end

  let el_a name attr = el_start_a name attr *< el_end

  let el_b name body = el_start name *> body *< el_end

  let el name = el_start name *< el_end

  let el_discard name =
    let inner =
      fix
      @@ fun el_discard ->
      many (discard data <|> el_start_any *> el_discard *> el_end) |> discard
    in
    el_start name *> inner *> el_end

  let run p inp =
    let* res, inp = p inp in
    let* (), _ = eoi inp in
    Ok res

  let inp str =
    let i =
      Xmlm.make_input ~strip:true (`String (0, str)) |> Lazy_list.of_xml_input
    in
    match Lazy.force i with
    | Lazy_list.Nil ->
        failwith "what"
    | Lazy_list.Cons (_, rest) ->
        rest

  let%test_module "el_discard" =
    ( module struct
      let%test "empty" = run (el_discard "el") (inp "<el></el>") = Ok ()

      let%test "data" = run (el_discard "el") (inp "<el>a</el>") = Ok ()

      let%test "single element" =
        run (el_discard "el") (inp "<el><a /></el>") = Ok ()

      let%test "multiple elements" =
        run (el_discard "el") (inp "<el><a /><a /></el>") = Ok ()

      let%test "nested elements" =
        run (el_discard "el")
          (inp "<el><a><b>c<c><d />d</c></b><ee>asd</ee></a></el>")
        = Ok ()
    end )
end

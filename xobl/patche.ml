type error = string

type 't input = 't Lazy_list.t Lazy.t

type ('t, 'inp) parser = 'inp input -> ('t * 'inp input, error) Result.t

type ('t, 'inp) t = ('t, 'inp) parser


let return v inp = Ok (v, inp)

let error msg _ = Error msg

let any inp =
  match Lazy.force inp with
  | Lazy_list.Cons (v, rest) -> Ok (v, rest)
  | Lazy_list.Nil -> Error "empty"

let eoi inp  =
  match Lazy.force inp with
  | Lazy_list.Nil -> Ok ((), inp)
  | Lazy_list.Cons _ -> Error "not eoi"


(** {2 Combinators} *)

let bind parser f inp =
  match parser inp with
  | Ok (result, rest) -> f result rest
  | Error _ as e -> e

let ( let& ) = bind

let satisfies test p =
  let& res = p in
  if test res then
    return res
  else
    error "failed test"

let ( <|> ) p1 p2 inp =
  match p1 inp with
  | Ok _ as ok -> ok
  | Error _ -> p2 inp


let tuple2 p1 p2 =
  let& res1 = p1 in
  let& res2 = p2 in
  return (res1, res2)

let pipe f p =
  let& res = p in
  return (f res)

let ( => ) p f = pipe f p

let pipe_result f p inp =
  match p inp with
  | Ok (v, rest) -> 
    f v |> Result.map (fun v -> (v, rest))
  | Error _ as err -> err

let pipe2 p1 p2 f =
  let& res1 = p1 in
  let& res2 = p2 in
  return (f res1 res2)


let apply f inp =
  match any inp with
  | Ok (v, rest) ->
    f v |> Result.map (fun v -> (v, rest))
  | Error _ as err -> err

let opt p inp =
  match p inp with
  | Ok (v, rest) -> Ok (Some v, rest)
  | Error _ -> Ok (None, inp)

let many p inp =
  let rec loop acc inp =
    match p inp with
    | Ok (v, rest) ->
      if rest == inp then
        invalid_arg "infinite loop detected in many: parser did not consume anything"
      else
        loop (v :: acc) rest
    | Error _ ->
      Ok (List.rev acc, inp)
  in
  loop [] inp

let many1 p =
  pipe2 p (many p) (fun hd tl -> hd :: tl)


let discard_with p v =
  let& _ = p in
  return v

let ( &>> ) p1 p2 =
  let& res1 = p1 in
  let& _ = p2 in
  return res1

let ( >>& ) p1 p2 =
  let& _ = p1 in
  p2

let ( &>>& ) = tuple2


module Xml = struct
  let data = apply @@ function
    | `Data data -> Ok data
    | _ -> Error "expected `Data"

  let dtd = apply @@ function
    | `Dtd dtd -> Ok dtd
    | _ -> Error "expected `Dtd"

  let el_start name attr = apply @@ function
    | `El_start ((_, n), attrs) when n = name ->
      attr attrs
    | _ -> Error ("expected `El_start: " ^ name)

  let el_start_empty name = apply @@ function
    | `El_start ((_, n), []) when n = name -> Ok ()
    | _ -> Error ("expected `El_start: " ^ name)

  let el_end = apply @@ function
    | `El_end -> Ok ()
    | `El_start _ -> Error "expected `El_end, received `El_start"
    | `Data data -> Error ("expected `El_end, received `Data \"" ^ data ^ "\"")
    | `Dtd _ -> Error "expect `El_end, received `Dtd"

  let el name body =
    el_start_empty name >>& body &>> el_end

  let el_empty name attr =
    el_start name attr &>> el_end

  let el_attr name attr body =
    el_start name attr &>>& body &>> el_end
end

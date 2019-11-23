let list_pry f =
  let rec loop prev = function
    | [] ->
      None, List.rev prev
    | el :: rest ->
    match f el with
    | Some el ->
      Some el, List.rev_append prev rest
    | None ->
      loop (el :: prev) rest
  in
  loop []


let named name = function
  | ("", n), v when n = name -> Some v
  | _ -> None


let optional name ls =
  Ok (list_pry (named name) ls)


let required name ls =
  match list_pry (named name) ls with
  | Some v, rest -> Ok (v, rest)
  | None, _ -> Error (Printf.sprintf "couldn't find attribute '%s'" name)


type error = string


type input = Xmlm.attribute list


type 'a parser = input -> ('a * Xmlm.attribute list, error) result


type 'a t = input -> ('a, error) result


let return v inp = Ok (v, inp)

let error msg _ = Error msg

let lift = function
  | Ok v -> return v
  | Error msg  -> error msg

let ( let& ) p f inp =
  match p inp with
  | Ok (v, rest) -> f v rest
  | Error _ as e -> e


let opt_conv v f err =
  match v with
  | None -> return None
  | Some v ->
  match f v with
  | Some i -> return (Some i)
  | None -> error (err v)


let str_o = optional

let str = required


let int_o name =
  let& v = optional name in
  opt_conv v int_of_string_opt 
    (Printf.sprintf "failed to convert attribute '%s' to int: %S" name)

let int name =
  let& v = required name in
  int_of_string_opt v
  |> Option.to_result ~none:(
      Printf.sprintf "failed to convert attribute '%s' to int: %S" name v
    )
  |> lift


let bool_o name =
  let& v = optional name in
  opt_conv v bool_of_string_opt
    (Printf.sprintf "failed to convert attribute '%s' to bool: %S" name)


let bool_f name attrs =
  bool_o name attrs
  |> Result.map (fun (v, rest) -> Option.value ~default:false v, rest)


let bool_t name attrs =
  bool_o name attrs
  |> Result.map (fun (v, rest) -> Option.value ~default:true v, rest)


let bool name attrs =
  bool_o name attrs
  |> function
    | Ok (Some b, rest) -> Ok (b, rest)
    | Ok (None, _) -> Error (Printf.sprintf "couldn't find attribute '%s'" name)
    | Error _ as err -> err


let ( <|> ) a1 a2 attrs =
  match a1 attrs with
  | Ok _ as ok -> ok
  | Error _ -> a2 attrs

let ( => ) p f inp =
  match p inp with
  | Ok (result, rest) -> Ok (f result, rest)
  | Error _ as e -> e

let or_ o p inp =
  match p inp with
  | Ok (result, rest) -> Ok (Some result, rest)
  | Error _ -> Ok (o, inp)


let eoi = function
  | [] -> Ok ()
  | _ -> Error "trailing elements detected"


let empty = eoi


let ( let* ) = Result.bind


let return p attrs =
  let* a, attrs = p attrs in
  let* () = eoi attrs in
  Ok a


let tuple2 p1 p2 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* () = eoi attrs in
  Ok (a1, a2)


let tuple3 p1 p2 p3 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* () = eoi attrs in
  Ok (a1, a2, a3)


let tuple4 p1 p2 p3 p4 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* a4, attrs = p4 attrs in
  let* () = eoi attrs in
  Ok (a1, a2, a3, a4)


let map f p attrs =
  let* a, attrs = p attrs in
  let* () = eoi attrs in
  Ok (f a)


let map2 f p1 p2 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* () = eoi attrs in
  Ok (f a1 a2)


let map3 f p1 p2 p3 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* () = eoi attrs in
  Ok (f a1 a2 a3)


let map4 f p1 p2 p3 p4 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* a4, attrs = p4 attrs in
  let* () = eoi attrs in
  Ok (f a1 a2 a3 a4)


let map5 f p1 p2 p3 p4 p5 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* a4, attrs = p4 attrs in
  let* a5, attrs = p5 attrs in
  let* () = eoi attrs in
  Ok (f a1 a2 a3 a4 a5)


let map6 f p1 p2 p3 p4 p5 p6 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* a4, attrs = p4 attrs in
  let* a5, attrs = p5 attrs in
  let* a6, attrs = p6 attrs in
  let* () = eoi attrs in
  Ok (f a1 a2 a3 a4 a5 a6)

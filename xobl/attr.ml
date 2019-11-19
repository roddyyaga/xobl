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


let map_fst f (a, b) =
  f a |> Result.map (fun v -> v, b)


type error = string


type input = Xmlm.attribute list


type 'a parser = input -> ('a * Xmlm.attribute list, error) result


type 'a t = input -> ('a, error) result


let str_o name attrs =
  list_pry (named name) attrs
  |> map_fst Result.ok


let str name attrs =
  list_pry (named name) attrs
  |> map_fst @@ Option.to_result ~none:(
    Printf.sprintf "couldn't find attribute '%s'" name
  )


let int_o name attrs =
  list_pry (named name) attrs
  |> map_fst @@ function
    | None -> Ok None
    | Some v ->
      match int_of_string_opt v with
      | Some i -> Ok (Some i)
      | None -> Error (
        Printf.sprintf "failed to convert attribute '%s' to int: %S" name v
      )


let int name attrs =
  let res, rest = list_pry (named name) attrs in
  res
  |> Option.to_result ~none:(
    Printf.sprintf "couldn't find attribute '%s'" name
  )
  |> fun x -> Result.bind x (fun v ->
    int_of_string_opt v
    |> Option.to_result ~none:(
      Printf.sprintf "failed to convert attribute '%s' to int: %S" name v
    )
  )
  |> Result.map (fun v -> v, rest)


let bool_o name attrs =
  list_pry (named name) attrs
  |> map_fst @@ function
    | None -> Ok None
    | Some v ->
      match bool_of_string_opt v with
      | Some b -> Ok (Some b)
      | None -> Error (
        Printf.sprintf "failed to convert attribute '%s' to bool: %S" name v
      )


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


let ( let* ) = Result.bind


let return p attrs =
  let* a, attrs = p attrs in
  if attrs = [] then
    Ok a
  else
    Error "trailing elements detected"


let tuple2 p1 p2 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  if attrs = [] then
    Ok (a1, a2)
  else
    Error "trailing elements detected"


let tuple3 p1 p2 p3 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  if attrs = [] then
    Ok (a1, a2, a3)
  else
    Error "trailing elements detected"


let tuple4 p1 p2 p3 p4 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* a4, attrs = p4 attrs in
  if attrs = [] then
    Ok (a1, a2, a3, a4)
  else
    Error "trailing elements detected"


let map f p attrs =
  let* a, attrs = p attrs in
  if attrs = [] then
    Ok (f a)
  else
    Error "trailing elements detected"


let map2 f p1 p2 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  if attrs = [] then
    Ok (f a1 a2)
  else
    Error "trailing elements detected"


let map3 f p1 p2 p3 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  if attrs = [] then
    Ok (f a1 a2 a3)
  else
    Error "trailing elements detected"


let map4 f p1 p2 p3 p4 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* a4, attrs = p4 attrs in
  if attrs = [] then
    Ok (f a1 a2 a3 a4)
  else
    Error "trailing elements detected"


let map5 f p1 p2 p3 p4 p5 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* a4, attrs = p4 attrs in
  let* a5, attrs = p5 attrs in
  if attrs = [] then
    Ok (f a1 a2 a3 a4 a5)
  else
    Error "trailing elements detected"


(* let map6 f p1 p2 p3 p4 p5 p6 attrs =
  let* a1, attrs = p1 attrs in
  let* a2, attrs = p2 attrs in
  let* a3, attrs = p3 attrs in
  let* a4, attrs = p4 attrs in
  let* a5, attrs = p5 attrs in
  let* a6, attrs = p6 attrs in
  if attrs = [] then
    Ok (f a1 a2 a3 a4 a5 a6)
  else
    Error "trailing elements detected" *)
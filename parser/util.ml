(** Returns [Some y] for which [f x = Some y] in the list and the list without
    [x], otherwise returns [(None, l)].
    Non-tail recursive. *)
let rec list_pry f = function
  | [] ->
    None, []
  | el :: rest ->
    match f el with
    | Some el ->
      Some el, rest
    | None ->
      let found, rest = list_pry f rest in
      found, el :: rest


exception Unexpected of string

let fail_unexpected str =
  raise (Unexpected str)

let fail_unexpectedf fmt =
  Printf.ksprintf fail_unexpected fmt

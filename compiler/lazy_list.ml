type 'a t = Cons of 'a * 'a t Lazy.t | Nil

let of_xml_input inp =
  let rec next () =
    match Xmlm.input inp with
    | v ->
        Cons (v, Lazy.from_fun next)
    | exception Xmlm.Error _ ->
        Nil
  in
  Lazy.from_fun next

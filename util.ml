module String' = struct
  let split chr str =
    let len = String.length str in
    let rec pos i =
      if i >= len then
        None
      else if str.[i] = chr then
        let left = StringLabels.sub str ~pos:0 ~len:i in
        let right = StringLabels.sub str ~pos:(i + 1) ~len:(len - i - 1) in
        Some (left, right)
      else
        pos (i + 1)
    in pos 0
end


let failwithf fmt = Printf.ksprintf failwith fmt


module List' = struct
  let rec first (test : 'a -> 'b option) : 'a list -> 'b option = function
    | [] ->
      None
    | hd :: tl ->
      match test hd with
      | Some x -> Some x
      | None -> first test tl


  let rec first_exn test = function
    | [] ->
      (match name with
      | Some name -> invalid_arg name
      | None -> raise Not_found)
    | hd :: tl ->
      match test hd with
      | Some x -> x
      | None -> first_exn test tl


  let rec extract f = function
    | [] -> None, []
    | x :: rest ->
      match f x with
      | Some x ->
        Some x, rest
      | None ->
        (* Not tail-recursive, but eh *)
        let found, ls = extract f rest in
        found, x :: ls

  let filter_map ls =
    let f acc = function Some x -> x :: acc | None -> acc in
    List.fold_left f [] ls
    |> List.rev
end

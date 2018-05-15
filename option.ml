let get = function
  | Some x -> x
  | None -> raise Not_found

let with_default def = function
  | Some x -> x
  | None -> def

let map f = function
  | Some x -> Some (f x)
  | None -> None

let bind f = function
  | Some x -> f x
  | None -> None

let iter f = function
  | Some x -> f x
  | None -> ()

let with_default_lazy def = function
  | Some x -> x
  | None -> Lazy.force def

let or_lazy y x = match x with
  | Some _ -> x
  | None -> Lazy.force y

let of_result = function
  | Ok v -> Some v
  | Error _ -> None

let to_result msg = function
  | Some v -> Ok v
  | None -> Error msg

(** Get and parse attributes out of association lists.
    The "_o" suffix represents an attribute that may not be present. *)

let str attrs x =
  List.assoc x attrs

let str_o attrs x =
  List.assoc_opt x attrs

let int attrs x =
  str attrs x |> int_of_string

let int_o attrs x =
  str_o attrs x
  |> CCOpt.map int_of_string

let bool_o attrs x =
  str_o attrs x
  |> CCOpt.map bool_of_string

(** Attributes that are false by default *)
let bool_f attrs x =
  bool_o attrs x
  |> CCOpt.get_or ~default:false

(** Attributes that are true by default *)
let bool_t attrs x =
  bool_o attrs x
  |> CCOpt.get_or ~default:true

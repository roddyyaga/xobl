type hostname =
  | Unix_domain_socket of string
  | Internet_domain of ([`Ipv4 | `Ipv6] * string * int)

type name = { hostname : hostname; display : int; screen : int }

val default : name

val parse_name : string -> name

val try_get_name : string option -> string

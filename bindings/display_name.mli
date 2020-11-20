type hostname =
  | Unix_domain_socket of string
  | Internet_domain of ([`Ipv4 | `Ipv6] * string * int)

type t = { hostname : hostname; display : int; screen : int }

val default : t

val parse : string -> t option

type family =
  | Family_local
  | Family_wild (* wildcard *)
  | Family_netname
  | Family_krb5_principal
  | Family_local_host

type entry =
  { xau_family : family
  ; xau_address : string
  ; xau_dpynum : string
  ; xau_type : string
  ; xau_data : string }

val read_entry : char Stream.t -> entry

val get_best_auth : family:family -> address:string -> dpynum:string ->
  ?types:(string list) -> entry list -> (string * string) option

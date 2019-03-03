let read_short_be inp =
  let b0 = Stream.next inp |> Char.code in
  let b1 = Stream.next inp |> Char.code in
  (b0 lsl 8) lor b1


(** Read length-prefixed string (Pascal string).
    length : short big-endian *)
let read_pstring inp =
  let len = read_short_be inp in
  let str = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set str i (Stream.next inp)
  done;
  Bytes.to_string str


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


let family_of_short = function
  | 256 -> Family_local
  | 254 -> Family_netname
  | 253 -> Family_krb5_principal
  | 252 -> Family_local_host
  | 0xFFFF -> Family_wild
  | _ -> failwith "Unknown family"


  (*
let show_family = function
  | Family_local -> "local"
  | Family_netname -> "netname"
  | Family_krb5_principal -> "krb5_principal"
  | Family_local_host -> "localhost"
  | Family_wild -> "wildcard"
  *)


(* Entry format:
   family       : short
   address_len  : short
   address      : char[address_len]
   dpynum_len   : short
   dpynum       : char[dpynum_len]
   type_len     : short
   type         : char[type_len]
   data_len     : short
   data         : char[data_len] *)
let read_entry inp =
  let xau_family = read_short_be inp |> family_of_short in
  let xau_address = read_pstring inp in
  let xau_dpynum = read_pstring inp in
  let xau_type = read_pstring inp in
  let xau_data = read_pstring inp in
  { xau_family
  ; xau_address
  ; xau_dpynum
  ; xau_type
  ; xau_data }

let%test _ =
  let line =
    {|  megane 0 MIT-MAGIC-COOKIE-1 s&<Á Ã,FÊ9ÓJ|}
  in
  let inp = Stream.of_string line in
  let auth = read_entry inp in
  auth.xau_family = Family_local &&
  auth.xau_address = "megane" &&
  auth.xau_dpynum = "0" &&
  auth.xau_type = "MIT-MAGIC-COOKIE-1"


(** Find an authentication entry matching [family], [address] and [dpynum]. *)
let get_best_auth ~family ~address ~dpynum ?(types=["MIT-MAGIC-COOKIE-1"]) entries =
  assert (List.length types > 0);
  let matches = entries |> List.filter
    (fun { xau_family; xau_address; xau_dpynum; xau_type; _ } ->
      let is_wildcard = family = Family_wild || xau_family = Family_wild in
      (is_wildcard || (family = xau_family && address = xau_address)) &&
      dpynum = xau_dpynum &&
      List.mem xau_type types
    )
  in
  types |> CCList.find_map (fun typ ->
    matches |> CCList.find_map (fun { xau_type; xau_data; _ } ->
      if typ = xau_type then
        Some (xau_type, xau_data)
      else
        None
    )
  )

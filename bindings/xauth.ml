let get_path () =
  match Sys.getenv_opt "XAUTHORITY" with
  | Some path ->
      Some path
  | None ->
      Sys.getenv_opt "HOME"
      |> Option.map (fun home -> Filename.concat home ".Xauthority")

type family =
  | Family_local
  | Family_wild (* wildcard *)
  | Family_netname
  | Family_krb5_principal
  | Family_local_host

type entry =
  { xau_family : family
  ; xau_address : string
  ; xau_dpynum : int
  ; xau_type : string
  ; xau_data : string
  }

let family_of_int = function
  | 256 ->
      Family_local
  | 254 ->
      Family_netname
  | 253 ->
      Family_krb5_principal
  | 252 ->
      Family_local_host
  | 0xFFFF ->
      Family_wild
  | _ ->
      failwith "Unknown family"

let read_uint16_be inp =
  let b0 = Stream.next inp |> Char.code in
  let b1 = Stream.next inp |> Char.code in
  (b0 lsl 8) lor b1

(** Read length-prefixed string (Pascal string).
    length : uint16 big-endian *)
let read_pstring inp =
  let len = read_uint16_be inp in
  let str = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set str i (Stream.next inp)
  done;
  Bytes.to_string str

(* Entry format:
   family       : uint16
   address_len  : uint16
   address      : char[address_len]
   dpynum_len   : uint16
   dpynum       : char[dpynum_len]
   type_len     : uint16
   type         : char[type_len]
   data_len     : uint16
   data         : char[data_len] *)
let read_entry inp =
  let xau_family = read_uint16_be inp |> family_of_int in
  let xau_address = read_pstring inp in
  let xau_dpynum = read_pstring inp |> int_of_string in
  let xau_type = read_pstring inp in
  let xau_data = read_pstring inp in
  { xau_family; xau_address; xau_dpynum; xau_type; xau_data }

let%test _ =
  let line = {|  megane 0 MIT-MAGIC-COOKIE-1 s&<Á Ã,FÊ9ÓJ|} in
  let inp = Stream.of_string line in
  let auth = read_entry inp in
  auth.xau_family = Family_local
  && auth.xau_address = "megane"
  && auth.xau_dpynum = 0
  && auth.xau_type = "MIT-MAGIC-COOKIE-1"

let rec read_all_entries stream acc =
  try
    let acc = read_entry stream :: acc in
    read_all_entries stream acc
  with Stream.Failure -> List.rev acc

let read_all_entries stream = read_all_entries stream []

let entries_from_file path =
  let f = open_in path in
  try Stream.of_channel f |> read_all_entries
  with exn -> close_in f; raise exn

let rec find_some ~f = function
  | [] ->
      None
  | head :: tail -> (
    match f head with Some v -> Some v | None -> find_some ~f tail )

(** Find an authentication entry matching [family], [address] and [display]. *)
let get_best_auth ~family ~address ~display ?(types = ["MIT-MAGIC-COOKIE-1"])
    entries =
  assert (List.length types > 0);
  let matches =
    List.filter
      (fun { xau_family; xau_address; xau_dpynum; xau_type; _ } ->
        let is_wildcard = family = Family_wild || xau_family = Family_wild in
        (is_wildcard || (family = xau_family && address = xau_address))
        && display = xau_dpynum
        && List.mem xau_type types)
      entries
  in
  find_some types ~f:(fun typ ->
      find_some matches ~f:(fun { xau_type; xau_data; _ } ->
          if typ = xau_type then Some (xau_type, xau_data) else None))

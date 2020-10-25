type name = { hostname : string option; display : int; screen : int }

(* The display name has form [hostname]:displaynumber[.screennumber].
   The parts between brackets can be omitted.
   https://cgit.freedesktop.org/xorg/app/xauth/tree/parsedpy.c
   https://www.x.org/releases/X11R7.7/doc/man/man7/X.7.xhtml#heading5 *)
let parse_name name =
  let colon = String.rindex name ':' in
  let hostname = if colon = 0 then None else Some (String.sub name 0 colon) in
  let last_half =
    String.sub name (colon + 1) (String.length name - colon - 1)
  in
  let dot = String.rindex_opt last_half '.' in
  let display, screen =
    match dot with
    | None ->
        (last_half, None)
    | Some dot ->
        let display = String.sub last_half 0 dot in
        let screen =
          String.sub last_half (dot + 1) (String.length last_half - dot - 1)
        in
        (display, Some screen)
  in
  (hostname, display, screen)

let%test _ = parse_name ":0.1" = (None, "0", Some "1")

let%test _ = parse_name "x.org:0" = (Some "x.org", "0", None)

let%test _ = parse_name "[::1]:0" = (Some "[::1]", "0", None)

let%test _ =
  parse_name "198.112.45.11:0.1" = (Some "198.112.45.11", "0", Some "1")

let%test "DECnet addresses terminate in :: but we don't support DECnet" =
  parse_name "hydra::0.1" = (Some "hydra:", "0", Some "1")

let parse_name name =
  let hostname, display, screen = parse_name name in
  { hostname
  ; display = int_of_string display
  ; screen = Option.fold screen ~some:int_of_string ~none:0
  }

let try_get_name = function
  | Some name ->
      name
  | None ->
      Sys.getenv_opt "DISPLAY" |> Option.value ~default:":0"

let open_ { hostname; display; screen } =
  let%lwt localhost = Lwt_unix.gethostname () in
  let domain, address, (xauth_name, xauth_data) =
    match hostname with
    | Some hostname when hostname <> localhost ->
        failwith "remote domain not implemented"
    | Some _ | None ->
        let addr = "/tmp/.X11-unix/X" ^ string_of_int display in
        let auth =
          Option.bind (Xauth.get_path ()) (fun path ->
            Xauth.entries_from_file path
            |> Xauth.get_best_auth ~family:Family_local ~address:localhost
              ~display
          )
    |> Option.value ~default:("", "")
        in
        (Unix.PF_UNIX, Unix.PF_ADDR_UNIX addr, auth)
  in
  let%lwt socket = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket address;%lwt
  let handshake, len = Protocol.make_handshake xauth_name xauth_data in
  let _ = Lwt_unix.write socket handshake 0 len in
  let%lwt in_buf = Protocol.read_handshake_response socket in
  let conn_info, _ = Protocol.read_handshake in_buf
  Lwt.return ()

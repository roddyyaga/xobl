let ( let& ) = Option.bind

let ( let* ) = Lwt.bind

let open_display Display.{ hostname; display; screen = _ } =
  let* domain, address, (xauth_name, xauth_data) =
    match hostname with
    | Display.Unix_domain_socket path ->
        let* localhost = Lwt_unix.gethostname () in
        let auth =
          let& xauth_path = Xauth.get_path () in
          Xauth.entries_from_file xauth_path
          |> Xauth.get_best ~family:Xauth.Family.Local ~address:localhost
               ~display
        in
        let auth = Option.value ~default:("", "") auth in
        Lwt.return (Unix.PF_UNIX, Unix.ADDR_UNIX path, auth)
    | Display.Internet_domain (family, hostname, port) ->
        let family =
          match family with `Ipv4 -> Unix.PF_INET | `Ipv6 -> Unix.PF_INET6
        in
        let* addresses =
          Lwt_unix.getaddrinfo hostname (string_of_int port)
            [Unix.AI_SOCKTYPE Unix.SOCK_STREAM; Unix.AI_FAMILY family]
        in
        let Unix.{ ai_family; ai_addr; _ } = List.hd addresses in
        Lwt.return (ai_family, ai_addr, ("", ""))
  in
  let socket = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket address in
  let handshake, len = Protocol.make_handshake xauth_name xauth_data in
  let* _ = Lwt_unix.write socket handshake 0 len in
  let* in_buf = Protocol.read_handshake_response socket in
  let _conn_info, _ = Protocol.read_handshake in_buf in
  Lwt.return ()

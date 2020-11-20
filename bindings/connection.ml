let ( let& ) = Option.bind

let ( let* ) = Lwt.bind

let open_display ~hostname ~display =
  let* domain, address, (xauth_name, xauth_data) =
    match hostname with
    | Display_name.Unix_domain_socket path ->
        let* localhost = Lwt_unix.gethostname () in
        let auth =
          let& xauth_path = Xauth.get_path () in
          Xauth.entries_from_file xauth_path
          |> Xauth.get_best ~family:Xauth.Family.Local ~address:localhost
               ~display
        in
        let auth = Option.value ~default:("", "") auth in
        Lwt.return (Unix.PF_UNIX, Unix.ADDR_UNIX path, auth)
    | Display_name.Internet_domain (family, hostname, port) ->
        let family =
          match family with `Ipv4 -> Unix.PF_INET | `Ipv6 -> Unix.PF_INET6
        in
        let* addresses =
          Lwt_unix.getaddrinfo hostname (string_of_int port)
            [Unix.AI_SOCKTYPE Unix.SOCK_STREAM; Unix.AI_FAMILY family]
        in
        (* TODO: we should try to connect to all the results in order
           instead of just picking the first. *)
        let Unix.{ ai_family; ai_addr; _ } = List.hd addresses in
        Lwt.return (ai_family, ai_addr, ("", ""))
  in
  let socket = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket address in
  let handshake, len = Protocol.make_handshake xauth_name xauth_data in
  let* _ = Lwt_unix.write socket handshake 0 len in
  let* in_buf = Protocol.read_handshake_response socket in
  let conn_info, _ = Protocol.read_handshake in_buf in
  Lwt.return (socket, conn_info)

module Xid_seed = struct
  type seed = { last : int32; inc : int32; base : int32; max : int32 }

  let make ~base ~mask =
    let inc = Int32.(logand mask (neg mask)) in
    let max = Int32.(add (sub mask inc) 1l) in
    { last = 0l; inc; base; max }

  (* TODO: use xmisc extension to look for unused xids when they run out
     https://gitlab.freedesktop.org/xorg/lib/libxcb/-/blob/master/src/xcb_xid.c *)
  let generate { last; inc; base; max } =
    if last > 0l && last >= max then
      failwith "No more available resource identifiers"
    else
      let last = Int32.add last inc in
      let xid = Int32.logor last base in
      Lwt.return (xid, { last; inc; base; max })
end

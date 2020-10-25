let open_display name =
  let name = Display.try_get_name name in
  let Display.{ hostname; display; screen } = Display.parse_name name in
  let localhost = Unix.gethostname () in
  let domain, address, (xauth_name, xauth_data) =
    match hostname with
    | Some hostname when hostname <> localhost ->
        failwith "remote domain not implemented!"
    | Some _ | None ->
        let addr = "/tmp/.X11-unix/X" ^ string_of_int display in
        let auth =
          Option.bind (Xauth.get_path ()) (fun path ->
              Xauth.entries_from_file path
              |> Xauth.get_best ~family:Xauth.Family.Local ~address:localhost
                   ~display)
          |> Option.value ~default:("", "")
        in
        (Unix.PF_UNIX, Unix.ADDR_UNIX addr, auth)
  in
  let socket = Unix.socket domain Unix.SOCK_STREAM 0 in
  Unix.set_close_on_exec socket;
  Unix.connect socket address;
  let handshake, len = Protocol.make_handshake xauth_name xauth_data in
  let _ = Unix.write socket handshake 0 len in
  let%lwt in_buf =
    Protocol.read_handshake_response (Lwt_unix.of_unix_file_descr socket)
  in
  let conn_info, _ = Protocol.read_handshake in_buf in
  let new_xid =
    let inc =
      let open Int32 in
      logand conn_info.Protocol.resource_id_mask
        (neg conn_info.Protocol.resource_id_mask)
    in
    let max = conn_info.Protocol.resource_id_mask in
    let last = ref 0l in
    fun () ->
      if !last > 0l && !last >= Int32.(add (sub max inc) 1l) then
        failwith "No more available resource identifiers"
      else last := Int32.add !last inc;
      Int32.logor !last conn_info.Protocol.resource_id_base
  in
  let len = 36 in
  let xid = new_xid () in
  let buf = Bytes.create len in
  Bytes.set_int8 buf 0 1;
  Bytes.set_uint16_le buf 2 (len / 4);
  Bytes.set_int32_le buf 4 xid;
  Bytes.set_int32_le buf 8 (List.hd conn_info.Protocol.screens).root;
  Bytes.set_uint16_le buf 12 0;
  Bytes.set_uint16_le buf 14 0;
  Bytes.set_uint16_le buf 16 200;
  Bytes.set_uint16_le buf 18 200;
  Bytes.set_uint16_le buf 20 10;
  Bytes.set_uint16_le buf 22 0x1;
  Bytes.set_int32_le buf 24 (List.hd conn_info.Protocol.screens).root_visual;
  Bytes.set_int32_le buf 28 0x2l;
  Bytes.set_int32_le buf 32 (List.hd conn_info.Protocol.screens).white_pixel;
  let _ = Unix.write socket buf 0 len in
  let len = 8 in
  let buf = Bytes.create len in
  Bytes.set_int8 buf 0 8;
  Bytes.set_uint16_le buf 2 (len / 4);
  Bytes.set_int32_le buf 4 xid;
  let _ = Unix.write socket buf 0 len in
  ignore (read_line ());
  Lwt.return (conn_info, screen)

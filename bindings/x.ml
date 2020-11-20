let ( let& ) = Option.bind

let open_display name =
  let name =
    match name with
    | Some name ->
        name
    | None ->
        Sys.getenv_opt "DISPLAY" |> Option.value ~default:":0"
  in
  let Display_name.{ hostname; display; screen } = Display_name.parse name |> Option.get in
  let%lwt socket, conn_info = Connection.open_display ~hostname ~display in
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
      else (
        last := Int32.add !last inc;
        Int32.logor !last conn_info.Protocol.resource_id_base )
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
  let%lwt _ = Lwt_unix.write socket buf 0 len in
  let len = 8 in
  let buf = Bytes.create len in
  Bytes.set_int8 buf 0 8;
  Bytes.set_uint16_le buf 2 (len / 4);
  Bytes.set_int32_le buf 4 xid;
  let%lwt _ = Lwt_unix.write socket buf 0 len in
  Lwt_unix.sleep 5.;%lwt
  Lwt.return (conn_info, screen)

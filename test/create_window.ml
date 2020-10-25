let () =
  Lwt_main.run
    (let%lwt resp, _ = Xobl_bindings.X.open_display None in
     print_endline (Xobl_bindings.Protocol.show_handshake_resp resp);
     Lwt.return ())

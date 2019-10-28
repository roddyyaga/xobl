let set_background =
  Xproto.grab_server ();%lwt
  let%lwt root_pmap =
    Xproto.get_properties_pixmap win ["_XROOTPMAP_ID"; "ESETROOT_PMAP_ID"]
  in
  Xproto.intern_atom ~name:"_XROOTPMAP_ID" ~only_if_exists:true ();%lwt
  Xproto.intern_atom ~name:"ESETROOT_PMAP_ID" ~only_if_exists:true ();%lwt

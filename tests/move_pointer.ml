let () =
  let dpy = Xproto.open_display "" in
  let scr = dpy.dpy_roots.(0) in
  let root = scr.scr_root in



module String_map = Types.String_map

let resolve_extension_path name =
  Filename.concat "xproto/src" (name ^ ".xml")

let load_extension file_name : Parser.protocol_file =
  resolve_extension_path file_name
  |> Parser.parse_file

let%test_unit "analyzer test" =
  let files =
    [ "bigreq"; "composite"; "damage"; "dpms"; "dri2"; "dri3"; "ge"; "glx"
    ; "present"; "randr"; "record"; "render"; "res"; "screensaver"; "shape"
    ; "shm"; "sync"; "xc_misc"; "xevie"; "xf86dri"; "xf86vidmode"; "xfixes"
    ; "xinerama"; "xinput"; "xkb"; "xprint"; "xproto"; "xselinux"; "xtest"
    ; "xvmc"; "xv" ]
  in
  let exts = List.map load_extension files in
  let exts = List.map P0_to_extension.lift_imports exts in
  Cache.init exts;
  let exts : P0_to_extension.extension String_map.t =
    exts |> List.fold_left (fun acc ext ->
      String_map.add acc ~key:ext.Types.file_name ~data:ext
    ) String_map.empty
  in
  let exts = P1_resolve.pass exts in
  let out = open_out "stuffs.ml" in
  output_string out "open X11_base\n\n";
  String_map.iter exts ~f:(fun ~key:_ ~data:ext ->
    Generator.generate out ext
  )

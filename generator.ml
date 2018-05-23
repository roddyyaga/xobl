open Util


let resolve_extension_path name =
  Filename.concat "xproto/src" (name ^ ".xml")


let load_extension file_name : Parser.protocol_file =
  resolve_extension_path file_name
  |> Parser.parse_file


let%test_unit "generator test" =
  let files =
    [ "bigreq"; "composite"; "damage"; "dpms"; "dri2"; "dri3"; "ge"; "glx"
    ; "present"; "randr"; "record"; "render"; "res"; "screensaver"; "shape"
    ; "shm"; "sync"; "xc_misc"; "xevie"; "xf86dri"; "xf86vidmode"; "xfixes"
    ; "xinerama"; "xinput"; "xkb"; "xprint"; "xproto"; "xselinux"; "xtest"
    ; "xvmc"; "xv" ]
  in
  let exts = List.map load_extension files in
  let _ = Analyzer.analyze_extensions exts in
  ()

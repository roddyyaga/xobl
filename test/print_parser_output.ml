open Xobl_compiler__

let modules =
  [ "bigreq"
  ; "composite"
  ; "damage"
  ; "dpms"
  ; "dri2"
  ; "dri3"
  ; "ge"
  ; "glx"
  ; "present"
  ; "randr"
  ; "record"
  ; "render"
  ; "res"
  ; "screensaver"
  ; "shape"
  ; "shm"
  ; "sync"
  ; "xc_misc"
  ; "xevie"
  ; "xf86dri"
  ; "xf86vidmode"
  ; "xfixes"
  ; "xinerama"
  ; "xinput"
  ; "xkb"
  ; "xprint"
  ; "xproto"
  ; "xselinux"
  ; "xtest"
  ; "xv"
  ; "xvmc" ]

exception Test_failed of Parser_utils.error

let parse_module m =
  let f = open_in (Printf.sprintf "../protocol/%s.xml" m) in
  try
    let xmlm_inp = Xmlm.make_input ~strip:true (`Channel f) in
    let inp = Patche.Lazy_list.of_xml_input xmlm_inp in
    ( match Patche.Xml.run Parser.xcb inp with
    | Error err ->
        raise (Test_failed err)
    | Ok xcb ->
        print_endline (Parsetree.show_xcb xcb) );
    close_in f
  with exn -> close_in f; raise exn

let () = modules |> List.iter parse_module

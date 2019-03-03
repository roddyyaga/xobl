(* The display name has form [hostname]:displaynumber[.screennumber].
   The parts between brackets can be omitted.
   https://cgit.freedesktop.org/xorg/app/xauth/tree/parsedpy.c
   https://www.x.org/releases/X11R7.7/doc/man/man7/X.7.xhtml#heading5 *)
let parse_display_name name =
  let colon = String.rindex name ':' in
  let hostname =
    match String.sub name 0 colon with
    | "" -> None
    | hostname -> Some hostname
  in
  let last_half =
    String.sub name (colon + 1) (String.length name - colon - 1)
  in
  let dot = String.rindex_opt last_half '.' in
  let display, screen = match dot with
    | None -> last_half, None
    | Some dot ->
      let display = String.sub last_half 0 dot in
      let screen =
        String.sub last_half (dot + 1) (String.length last_half - dot - 1)
      in
      display, Some screen
  in
  hostname, display, screen

let%test _ =
  parse_display_name ":0.1" = (None, "0", Some "1")

let%test _ =
  parse_display_name "x.org:0" = (Some "x.org", "0", None)

let%test _ =
  parse_display_name "[::1]:0" = (Some "[::1]", "0", None)

let%test _ =
  parse_display_name "198.112.45.11:0.1" = (Some "198.112.45.11", "0", Some "1")

let%test "fuck DECnet" =
  parse_display_name "hydra::0.1" = (Some "hydra:", "0", Some "1")

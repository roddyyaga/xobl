let is_last_of_acronym name pos len =
  if pos < len - 1 then
    let next = name.[pos + 1] in
    (next < '0' || next > '9')
      && Char.lowercase_ascii next = next
  else
    false

let%test "is_last_of_acronym example" =
  is_last_of_acronym "DRI2Buffer" 5 10

let%test "is_last_of_acronym example 2" =
  not (is_last_of_acronym "DRI2Buffer" 3 10)

let snek name =
  let buf = Buffer.create 16 in
  let len = String.length name in
  StringLabels.iteri name ~f:(fun i -> function
    | c when i = 0 ->
      Buffer.add_char buf (Char.lowercase_ascii c)

    | 'A' .. 'Z' as c ->
      let prev = name.[i - 1] in
      if prev <> '_' &&
        (Char.lowercase_ascii prev = prev || is_last_of_acronym name i len)
      then
        Buffer.add_char buf '_';
      Buffer.add_char buf (Char.lowercase_ascii c)

    | c ->
      Buffer.add_char buf c
  );
  Buffer.contents buf


let snake name =
  if name = "DECnet" then
    "decnet"
  else if String.lowercase_ascii name = name then
    name
  else if String.uppercase_ascii name = name then
    String.lowercase_ascii name
  else
    snek name


let%test "C-case" =
  snake "bigreq" = "bigreq"

let%test "UPPERCASE" =
  snake "CHAR2B" = "char2b"

let%test "snake_case" =
  snake "bits_per_rgb_value" = "bits_per_rgb_value"

let%test "CamelCase" =
  snake "StaticGray" = "static_gray"

let%test "weird case 1" =
  snake "GLXContext" = "glx_context"

let%test "weird case 2" =
  snake "DECnet" = "decnet"

let%test "weird case 3" =
  snake "Positive_HSync" = "positive_h_sync"

let%test "weird case 4" =
  snake "DRI2Buffer" = "dri2_buffer"

let%test "weird case 5" =
  snake "TestStriGS" = "test_stri_gs"


let caml name =
  String.capitalize_ascii (snake name)

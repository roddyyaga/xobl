let modules =
  [
    "bigreq";
    "composite";
    "damage";
    "dpms";
    "dri2";
    "dri3";
    "ge";
    "glx";
    "present";
    "randr";
    "record";
    "render";
    "res";
    "screensaver";
    "shape";
    "shm";
    "sync";
    "xc_misc";
    (* "xevie"; *)
    "xf86dri";
    "xf86vidmode";
    "xfixes";
    "xinerama";
    "xinput";
    (* "xkb"; *)
    "xprint";
    "xproto";
    "xselinux";
    "xtest";
    "xv";
    "xvmc";
  ]

let parse_module m =
  let f = open_in (Printf.sprintf "../xml-xcb/%s.xml" m) in
  try
    let m = Xobl_compiler.Parser.parse (`Channel f) |> Result.get_ok in
    close_in f;
    m
  with exn ->
    close_in f;
    raise exn

let sort_topological (nodes : (string * string list) list) =
  let rec dfs out (node, dependencies) =
    if List.mem node out then out
    else
      node
      :: List.fold_left
           (fun out node ->
             let dependencies = List.assoc node nodes in
             dfs out (node, dependencies))
           out dependencies
  in
  List.fold_left dfs [] nodes |> List.rev

let sort_xcbs xcbs =
  xcbs
  |> List.map (function
       | Xobl_compiler__Elaboratetree.Core _ -> ("xproto", [])
       | Extension { file_name; imports; _ } -> (file_name, imports))
  |> sort_topological
  |> List.map (fun name ->
         xcbs
         |> List.find (function
              | Xobl_compiler__Elaboratetree.Core _ when name = "xproto" -> true
              | Extension { file_name; _ } when file_name = name -> true
              | _ -> false))

let () =
  let xcbs =
    modules |> List.map parse_module
    |> List.map Xobl_compiler.Elaborate.unions_to_switches
    |> Xobl_compiler.Elaborate.resolve_idents
  in
  output_string stdout
    {|type xid = int
type file_descr = int
type ('flags, 'vals) mask = F of 'flags list | V of 'vals
type ('enum, 't) alt = E of 'enum | T of 't
let (let*) = Option.bind

let decode f buf ~at ~size =
  if Bytes.length buf < at + size - 1 then None else Some (f buf at, at + size)

let decode_char buf ~at = decode Bytes.get buf ~at ~size:1

let decode_uint8 buf ~at = decode Bytes.get_uint8 buf ~at ~size:1

let decode_int8 buf ~at = decode Bytes.get_int8 buf ~at ~size:1

let decode_bool buf ~at =
  decode_uint8 buf ~at |> Option.map (fun (n, at) -> (n <> 0, at))

let decode_uint16 buf ~at = decode Bytes.get_uint16_le buf ~at ~size:2

let decode_int16 buf ~at = decode Bytes.get_int16_le buf ~at ~size:2

let decode_int32 buf ~at = decode Bytes.get_int32_le buf ~at ~size:4

let decode_int64 buf ~at = decode Bytes.get_int64_le buf ~at ~size:8

let decode_float buf ~at =
  decode_int64 buf ~at
  |> Option.map (fun (n, at) -> (Int64.float_of_bits n, at))

let decode_file_descr buf ~at =
  decode Bytes.get_int16_le buf ~at ~size:2
  |> Option.map (fun (n, at) -> ((Obj.magic n : Unix.file_descr), at))

let decode_xid = decode_int16

let decode_enum decode to_int of_int buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) ->
    match of_int (to_int n) with
    | None -> None
    | Some e -> Some (e, at)

let decode_list decode_item len buf ~at =
  let rec loop items at len =
    if len = 0 then Some ((List.rev items), at)
    else
      match decode_item buf ~at with
      | None -> None
      | Some (item, at) ->
        loop (item :: items) at (len - 1)
  in
  loop [] at len

let encode_char buf c = Buffer.add_char buf c

let encode_bool buf b = Buffer.add_char buf (if b then Char.chr 1 else Char.chr 0)

let encode_int8 buf i = Buffer.add_int8 buf i

let encode_int16 buf i = Buffer.add_int16_le buf i

let encode_int32 buf i = Buffer.add_int32_le buf i

let encode_int64 buf i = Buffer.add_int64_le buf i

let encode_file_descr buf fd = Buffer.add_int16_le buf (Obj.magic fd)

let encode_uint8 buf i = Buffer.add_uint8 buf i

let encode_uint16 buf i = Buffer.add_uint16_le buf i

let encode_float buf f = Buffer.add_int64_le buf (Int64.bits_of_float f)

let encode_xid = encode_int16

let encode_to_int encode to_int buf x = encode buf (to_int x)

let encode_list encode_item buf items =
    List.iter (encode_item buf) items

let encode_alt encode to_int encode_t buf = function
    | E e -> encode_to_int encode to_int buf e
    | T t -> encode_t buf t
|};
  List.map (Xobl_compiler.Elaborate.do_stuff xcbs) xcbs
  |> sort_xcbs
  |> Xobl_compiler__.Generate_ocaml.gen stdout

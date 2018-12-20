type xid = int32

type fd = int

type buffer = string


type ('l, 'r) either =
  | Left of 'l
  | Right of 'r


type ('flags, 'vals) mask =
  | Flags of 'flags list
  | Val of 'vals


  (*
type 'a event =
  { name    : string
  ; code    : int
  ; content : string -> int -> 'a }
*)


(* Little endian *)
let get_byte buf at =
  Char.code (String.get buf at)

let get_byte_int32 buf at =
  Int32.of_int (get_byte buf at)

let get_uint16 buf at =
  let b0 = get_byte buf at in
  let b1 = get_byte buf (at + 1) in
  (b0 lsl 8) lor b1

let get_uint32 buf at =
  let b0 = get_byte_int32 buf at in
  let b1 = get_byte_int32 buf (at + 1) in
  let b2 = get_byte_int32 buf (at + 2) in
  let b3 = get_byte_int32 buf (at + 3) in
  Int32.(logor (logor (shift_left b0 24) (shift_left b1 16))
               (logor (shift_left b2 8)  b3))

let get_int16 = get_uint16
let get_int32 = get_uint32

let get_bool buf at =
  match get_byte buf at with
  | 0 -> false
  | _ -> true

let get_xid = get_uint32


let put_padding buf bytes =
  for _ = 1 to bytes do
    Buffer.add_char buf '\000'
  done


let put_char buf char =
  Buffer.add_char buf char

let put_int_as_char buf int =
  Buffer.add_char buf (Char.chr int)


let put_bool buf bool =
  Buffer.add_char buf (if bool then '\000' else '\001')

let put_int_as_bool buf int =
  Buffer.add_char buf (if int > 0 then '\000' else '\001')


let put_int8 = put_int_as_char
let put_int_as_int8 = put_int_as_char


let put_int16 _buf _int16 =
  ()
  (* let b0 = (0xFF00 land int16) lsr 2 in
  let b1 = 0x00FF land int16 in
  put_int8 buf b0;
  put_int8 buf b1 *)

let put_int_as_int16 = put_int16


let put_int32 _buf _int32 =
  ()
  (* let b0 = Int32.(to_int (0xFF land (int32 lsr 23)))
  let b0 = Int32.(to_int (0xFF000000l land int32) lsr 6) in
  let b1 = Int32.(to_int (0x00FF0000l land int32) lsr 4) in
  let b2 = Int32.(to_int (0x0000FF00l land int32) lsr 2) in
  let b3 = Int32.(to_int (0x000000FFl land int32)) in
  put_int8 buf b0;
  put_int8 buf b1;
  put_int8 buf b2;
  put_int8 buf b3 *)

let put_int_as_int32 buf int =
  put_int32 buf (Int32.of_int int)


let put_xid = put_int32


let put_int64 _buf _int64 =
  ()
  (* let b0 = Int64.(to_int (0xFF00000000000000L land int32) lsr 14) in
  let b1 = Int64.(to_int (0x00FF000000000000L land int32) lsr 12) in
  let b2 = Int64.(to_int (0x0000FF0000000000L land int32) lsr 10) in
  let b3 = Int64.(to_int (0x000000FF00000000L land int32 in
  let b4 = 0x00000000FF000000L land int32 in
  let b5 = 0x0000000000FF0000L land int32 in
  let b6 = 0x000000000000FF00L land int32 in
  let b7 = 0x00000000000000FFL land int32 in
  put_int8 buf b0;
  put_int8 buf b1;
  put_int8 buf b2;
  put_int8 buf b3;
  put_int8 buf b4;
  put_int8 buf b5;
  put_int8 buf b6;
  put_int8 buf b7 *)

let put_int_as_int64 buf int =
  put_int64 buf (Int64.of_int int)


let parse_error buf at =
  assert (get_byte buf at = 0);
  let code = get_byte buf (at + 1) in
  let seq_num = get_uint16 buf (at + 2) in
  (code, seq_num)


module type Extension = sig
  val version : int * int
  val query_version_name : string
end
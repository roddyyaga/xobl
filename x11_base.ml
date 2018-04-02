type ('a, 'b) bitmask =
  | Flags of 'a list
  | Val of 'b


type 'a error =
  { name    : string
  ; code    : int
  ; content : string -> int -> 'a }


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

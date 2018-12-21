type xid = int32

type fd = int


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
module Get = struct
  let byte buf at =
    Char.code (String.get buf at)

  let byte_int32 buf at =
    Int32.of_int (byte buf at)

  let uint16 buf at =
    let b0 = byte buf at in
    let b1 = byte buf (at + 1) in
    (b0 lsl 8) lor b1

  let uint32 buf at =
    let b0 = byte_int32 buf at in
    let b1 = byte_int32 buf (at + 1) in
    let b2 = byte_int32 buf (at + 2) in
    let b3 = byte_int32 buf (at + 3) in
    Int32.(logor (logor (shift_left b0 24) (shift_left b1 16))
                 (logor (shift_left b2 8)  b3))

  let int16 = uint16
  let int32 = uint32

  let bool buf at =
    match byte buf at with
    | 0 -> false
    | _ -> true

  let xid = uint32
end


module Put = struct
  let padding buf bytes =
    for _ = 1 to bytes do
      Buffer.add_char buf '\000'
    done


  let char buf char =
    Buffer.add_char buf char

  let int_as_char buf int =
    Buffer.add_char buf (Char.chr int)


  let bool buf bool =
    Buffer.add_char buf
      (if bool then '\001' else '\000')

  let int_as_bool buf int =
    Buffer.add_char buf
      (if int > 0 then '\001' else '\000')


  let int8 = int_as_char
  let int_as_int8 = int_as_char


  let int16 buf int16 =
    ignore buf;
    ignore int16;
    ()

  let int_as_int16 = int16


  let int32 buf int32 =
    ignore buf;
    ignore int32;
    ()

  let int_as_int32 buf int =
    int32 buf (Int32.of_int int)


  let xid = int32


  let int64 buf int64 =
    ignore buf;
    ignore int64;
    ()

  let int_as_int64 buf int =
    int64 buf (Int64.of_int int)
end


let parse_error buf at =
  assert (Get.byte buf at = 0);
  let code = Get.byte buf (at + 1) in
  let seq_num = Get.uint16 buf (at + 2) in
  (code, seq_num)


module type Extension = sig
  val version : int * int
  val query_version_name : string
end
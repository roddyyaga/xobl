type xid = int32

type fd = int


type ('v, 't) variant_or =
  | Variant of 'v
  | Type of 'v


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

  let int8 buf at =
    let b = byte buf at in
    b lor (-(b lsr 7) lsl 8)

  let byte_int32 buf at =
    Int32.of_int (byte buf at)

  let uint16 buf at =
    let b0 = byte buf at in
    let b1 = byte buf (at + 1) in
    b0 lor (b1 lsl 8)

  let int16 buf at =
    let i = uint16 buf at in
    i lor (-(i lsr 15) lsl 16)

  let uint32 buf at =
    let b0 = byte_int32 buf at in
    let b1 = byte_int32 buf (at + 1) in
    let b2 = byte_int32 buf (at + 2) in
    let b3 = byte_int32 buf (at + 3) in
    Int32.(logor (logor b0                 (shift_left b1 8))
                 (logor (shift_left b2 16) (shift_left b3 24)))

  let int32 buf at =
    let i = uint32 buf at in
    Int32.(logor i (shift_left (neg (shift_left i 31)) 32))

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
    Buffer.add_char buf (Char.unsafe_chr int)

  let bool buf bool =
    Buffer.add_char buf
      (if bool then '\001' else '\000')

  let int_as_bool buf int =
    Buffer.add_char buf
      (if int > 0 then '\001' else '\000')


  let int8 = int_as_char
  let int_as_int8 = int_as_char

  let int16 buf int =
    Buffer.add_char buf (Char.unsafe_chr int);
    Buffer.add_char buf (Char.unsafe_chr (int asr 8))

  let int_as_int16 = int16

  let int32 buf int =
    Buffer.add_char buf (Char.unsafe_chr Int32.(to_int int));
    Buffer.add_char buf (Char.unsafe_chr Int32.(to_int (shift_right int 8)));
    Buffer.add_char buf (Char.unsafe_chr Int32.(to_int (shift_right int 16)));
    Buffer.add_char buf (Char.unsafe_chr Int32.(to_int (shift_right int 24)))

  let int_as_int32 buf int =
    int32 buf (Int32.of_int int)

  let xid = int32

  let int64 buf int =
    Buffer.add_char buf (Char.unsafe_chr Int64.(to_int int));
    Buffer.add_char buf (Char.unsafe_chr Int64.(to_int (shift_right int 8)));
    Buffer.add_char buf (Char.unsafe_chr Int64.(to_int (shift_right int 16)));
    Buffer.add_char buf (Char.unsafe_chr Int64.(to_int (shift_right int 24)));
    Buffer.add_char buf (Char.unsafe_chr Int64.(to_int (shift_right int 32)));
    Buffer.add_char buf (Char.unsafe_chr Int64.(to_int (shift_right int 40)));
    Buffer.add_char buf (Char.unsafe_chr Int64.(to_int (shift_right int 48)));
    Buffer.add_char buf (Char.unsafe_chr Int64.(to_int (shift_right int 56)))

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

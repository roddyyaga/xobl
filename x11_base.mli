type xid

type fd = int

type buffer = string

type ('l, 'r) either =
  | Left of 'l
  | Right of 'r

type ('flags, 'vals) mask =
  | Flags of 'flags list
  | Val of 'vals


val get_byte : buffer -> int -> int
val get_byte_int32 : buffer -> int -> int32
val get_uint16 : buffer -> int -> int
val get_uint32 : buffer -> int -> int32
val get_int16 : buffer -> int -> int
val get_int32 : buffer -> int -> int32
val get_bool : buffer -> int -> bool
val get_xid : buffer -> int -> xid

val parse_error : buffer -> int * int

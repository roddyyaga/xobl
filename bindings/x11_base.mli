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

val put_padding : Buffer.t -> int -> unit

val put_char : Buffer.t -> char -> unit
val put_bool : Buffer.t -> bool -> unit
val put_int8 : Buffer.t -> int -> unit
val put_int16 : Buffer.t -> int -> unit
val put_int32 : Buffer.t -> int32 -> unit
val put_xid : Buffer.t -> xid -> unit
val put_int64 : Buffer.t -> int64 -> unit

val put_int_as_char : Buffer.t -> int -> unit
val put_int_as_bool : Buffer.t -> int -> unit
val put_int_as_int8 : Buffer.t -> int -> unit
val put_int_as_int16 : Buffer.t -> int -> unit
val put_int_as_int32 : Buffer.t -> int -> unit
val put_int_as_int64 : Buffer.t -> int -> unit

val parse_error : buffer -> int -> int * int

module type Extension = sig
  val version : int * int
  val query_version_name : string
end
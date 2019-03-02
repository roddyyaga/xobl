type xid

type fd = int

type ('v, 't) variant_or =
  | Variant of 'v
  | Type of 'v

type ('flags, 'vals) mask =
  | Flags of 'flags list
  | Val of 'vals


module Get : sig
  val byte : string -> int -> int
  val byte_int32 : string -> int -> int32
  val int8 : string -> int -> int
  val uint16 : string -> int -> int
  val int16 : string -> int -> int
  val uint32 : string -> int -> int32
  val int32 : string -> int -> int32
  val bool : string -> int -> bool
  val xid : string -> int -> xid
end

module Put : sig
  val padding : Buffer.t -> int -> unit

  val char : Buffer.t -> char -> unit
  val bool : Buffer.t -> bool -> unit
  val int8 : Buffer.t -> int -> unit
  val int16 : Buffer.t -> int -> unit
  val int32 : Buffer.t -> int32 -> unit
  val xid : Buffer.t -> xid -> unit
  val int64 : Buffer.t -> int64 -> unit

  val int_as_char : Buffer.t -> int -> unit
  val int_as_bool : Buffer.t -> int -> unit
  val int_as_int8 : Buffer.t -> int -> unit
  val int_as_int16 : Buffer.t -> int -> unit
  val int_as_int32 : Buffer.t -> int -> unit
  val int_as_int64 : Buffer.t -> int -> unit
end

val parse_error : string -> int -> int * int

module type Extension = sig
  val version : int * int
  val query_version_name : string
end

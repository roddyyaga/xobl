type t =
  | Void
  | Char
  | Byte
  | Bool
  | Int8
  | Int16
  | Int32
  | Fd
  | Card8
  | Card16
  | Card32
  | Card64
  | Float
  | Double
  | Xid (** maps to a Card32 *)

let of_string = function
  | "void"   -> Some Void
  | "char"   -> Some Char
  | "BYTE"   -> Some Byte
  | "BOOL"   -> Some Bool
  | "INT8"   -> Some Int8
  | "INT16"  -> Some Int16
  | "INT32"  -> Some Int32
  | "fd"     -> Some Fd
  | "CARD8"  -> Some Card8
  | "CARD16" -> Some Card16
  | "CARD32" -> Some Card32
  | "CARD64" -> Some Card64
  | "float"  -> Some Float
  | "double" -> Some Double
  | _        -> None

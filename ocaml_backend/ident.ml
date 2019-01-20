let variant name =
  if name.[0] >= '0' && name.[0] <= '9' then
    "`_" ^ Casing.snake name
  else
    "`" ^ Casing.caml name


let ocaml_reserved =
  [ "and"; "as"; "asr"; "assert"; "begin"; "class"; "constraint"; "do"; "done"
  ; "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun"
  ; "function"; "functor"; "if"; "in"; "include"; "inherit"; "initializer"
  ; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"
  ; "mod"; "module"; "open"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"
  ; "open!"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to"; "true"
  ; "try"; "type"; "val"; "virtual"; "when"; "while"; "with" ]


let identifier name =
  let name = Casing.snake name in
  if List.mem name ocaml_reserved then
    name ^ "_"
  else
    name


let value = identifier
let func = identifier


let type_ = identifier
let record = identifier
let record_field = identifier

let enum name =
  Casing.snake name ^ "_enum"

let mask name =
  Casing.snake name ^ "_mask"


let enum_vals name =
  Casing.snake name ^ "_vals"

let mask_bits name =
  Casing.snake name ^ "_bits"



open Xobl_elaborate

let of_ident = function
  | Types.Id n -> identifier n
  | Types.Ext_id (e, n) ->
    Casing.caml e ^ "." ^ identifier n

let ident_part = function
  | Types.Id n -> Casing.snake n
  | Types.Ext_id (e, n) ->
    Casing.caml e ^ "." ^ Casing.snake n

let of_prim =
  let open Prim in function
  | Void   -> "unit"
  | Char   -> "char"
  | Byte   -> "char"
  | Bool   -> "bool"
  | Int8   -> "int"
  | Int16  -> "int"
  | Int32  -> "int32"
  | Fd     -> "X11_base.fd"
  | Card8  -> "int"
  | Card16 -> "int"
  | Card32 -> "int32"
  | Card64 -> "int64"
  | Float  -> "float"
  | Double -> "float"
  | Xid    -> "X11_base.xid"

let of_x_type = function
  | Types.Prim t -> of_prim t
  | Types.Ref id -> of_ident id


let enum_id name =
  ident_part name ^ "_enum"

let mask_id name =
  ident_part name ^ "_mask"

let of_field_type =
  let open P1_resolve in function
  | Prim t ->
    of_x_type t

  | Enum (e, t) ->
    Printf.sprintf "%s (* %s *)"
      (enum_id e) (of_x_type t)

  | Mask (m, t) ->
    Printf.sprintf "%s (* %s *)"
      (mask_id m) (of_x_type t)

  | Enum_or (e, t) ->
    Printf.sprintf "(%s, %s) X11_base.variant_or"
      (enum_id e) (of_x_type t)

  | Mask_or (m, t) ->
    Printf.sprintf "(%s, %s) X11_base.variant_or"
      (mask_id m) (of_x_type t)

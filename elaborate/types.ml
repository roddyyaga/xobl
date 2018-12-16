type 'a extension =
  { name         : string
  ; file_name    : string
  ; query_name   : string option
  ; version      : (int * int) option
  ; imports      : string list
  ; declarations : 'a list }


type ident =
  | Id of string
  | Ext_id of string * string


type x_type =
  | Prim of Prim.t
  | Ref of ident


module String_map = MoreLabels.Map.Make(String)

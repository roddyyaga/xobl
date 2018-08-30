module T = Types


module type Prev = sig
  type declaration

  type extension = declaration T.extension
end


module type S = sig
  type declaration

  type extension = declaration T.extension

  type prev_extension

  val pass : prev_extension T.String_map.t -> extension T.String_map.t
end


module type P = sig
  module Prev : Prev

  type declaration

  val map : Prev.extension T.String_map.t -> Prev.extension -> Prev.declaration -> declaration
end


module Make(P : P) :
  S with type declaration = P.declaration
    with type prev_extension = P.Prev.extension
= struct
  type prev_extension = P.Prev.extension

  type declaration = P.declaration

  type extension = declaration T.extension

  let folder exts ~key:ext_id ~data:ext acc =
    let declarations = List.map (P.map exts ext) ext.declarations in
    let ext = { ext with declarations } in
    T.String_map.add acc ~key:ext_id ~data:ext

  let pass (exts : prev_extension T.String_map.t) : extension T.String_map.t =
    T.String_map.fold exts ~init:T.String_map.empty ~f:(folder exts)
end

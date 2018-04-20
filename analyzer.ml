module X = Parser


let string_split chr str =
  let len = String.length str in
  let rec pos i =
    if i >= len then
      None
    else if str.[i] = chr then
      let left = StringLabels.sub str ~pos:0 ~len:i in
      let right = StringLabels.sub str ~pos:(i + 1) ~len:(len - i - 1) in
      Some (left, right)
    else
      pos (i + 1)
  in pos 0

let option_get_exn = function
  | Some x -> x
  | None -> raise Not_found

let option_map f = function
  | Some x -> Some (f x)
  | None -> None


let failwithf fmt = Printf.ksprintf failwith fmt


let ( <||> ) x y =
  match x with
  | Some _ -> x
  | None -> Lazy.force y


let ( <|!|> ) x y =
  match x with
  | Some x -> x
  | None -> Lazy.force y


let rec list_get (test : 'a -> 'b option) : 'a list -> 'b option = function
  | [] ->
    None
  | hd :: tl ->
    match test hd with
    | Some x -> Some x
    | None -> list_get test tl

let rec list_get_exn test = function
  | [] ->
    raise Not_found
  | hd :: tl ->
    match test hd with
    | Some x -> x
    | None -> list_get_exn test tl


let resolve_extension_path name =
  Filename.concat "xproto/src" (name ^ ".xml")


module StrMap = MoreLabels.Map.Make(String)


module Pass_1 = struct
  (* In which we load every extension and abstract imports into the extension
   * information. *)

  type declaration =
    | X_id of string
    | X_id_union of string * string list
    | Enum of string * X.enum
    | Type_alias of string * string

    | Event of string * int * X.event
    | Generic_event of string * int * X.generic_event
    | Event_struct of string * X.allowed_events list
    | Event_alias of string * int * string

    | Error of string * int * X.error
    | Error_alias of string * int * string

    | Struct of string * X.struct_fields
    | Union of string * X.static_field list
    | Request of string * int * X.request


  let lift_imports decls : string list * declaration list =
    let imports, decls =
      ListLabels.fold_left decls ~init:([], []) ~f:(fun (imports, decls) ->
        function
        | X.Import id ->
          (id :: imports), decls

        | X.X_id x                      -> imports, X_id x                     :: decls
        | X.X_id_union (x1, x2)         -> imports, X_id_union (x1, x2)        :: decls
        | X.Enum (x1, x2)               -> imports, Enum (x1, x2)              :: decls
        | X.Type_alias (x1, x2)         -> imports, Type_alias (x1, x2)        :: decls
        | X.Event (x1, x2, x3)          -> imports, Event (x1, x2, x3)         :: decls
        | X.Generic_event (x1, x2, x3)  -> imports, Generic_event (x1, x2, x3) :: decls
        | X.Event_struct (x1, x2)       -> imports, Event_struct (x1, x2)      :: decls
        | X.Event_alias (x1, x2, x3)    -> imports, Event_alias (x1, x2, x3)   :: decls
        | X.Error (x1, x2, x3)          -> imports, Error (x1, x2, x3)         :: decls
        | X.Error_alias (x1, x2, x3)    -> imports, Error_alias (x1, x2, x3)   :: decls
        | X.Struct (x1, x2)             -> imports, Struct (x1, x2)            :: decls
        | X.Union (x1, x2)              -> imports, Union (x1, x2)             :: decls
        | X.Request (x1, x2, x3)        -> imports, Request (x1, x2, x3)       :: decls
      )
    in
    List.rev imports, List.rev decls


  type extension =
    { name         : string
    ; file_name    : string
    ; query_name   : string option
    ; version      : (int * int) option
    ; imports      : string list
    ; declarations : declaration list }


  let load_extension file_name : extension =
    match X.parse_file (resolve_extension_path file_name) with
    | Core decls ->
      let imports, declarations = lift_imports decls in
      { name = "Xproto"
      ; query_name = None; version = None
      ; file_name; imports; declarations }

    | Extension ({ name; query_name; version; _ }, decls) ->
      let imports, declarations = lift_imports decls in
      { query_name = Some query_name; version = Some version
      ; name; file_name; imports; declarations }
end


module Pass_2 = struct
  (* In which we separate Enum declarations into enums and masks.  *)
  module P = Pass_1

  type declaration =
    | X_id of string
    | X_id_union of string * string list
    | Enum of string * X.enum_items
    | Mask of string * X.mask
    | Type_alias of string * string

    | Event of string * int * X.event
    | Generic_event of string * int * X.generic_event
    | Event_struct of string * X.allowed_events list
    | Event_alias of string * int * string

    | Error of string * int * X.error
    | Error_alias of string * int * string

    | Struct of string * X.struct_fields
    | Union of string * X.static_field list
    | Request of string * int * X.request


  let sep_enums (decls : P.declaration list) : declaration list =
    ListLabels.map decls ~f:(function
      | P.Enum (name, `Enum items) ->
        Enum (name, items)

      | P.Enum (name, `Mask mask) ->
        Mask (name, mask)

      | P.X_id x                      -> X_id x
      | P.X_id_union (x1, x2)         -> X_id_union (x1, x2)
      | P.Type_alias (x1, x2)         -> Type_alias (x1, x2)
      | P.Event (x1, x2, x3)          -> Event (x1, x2, x3)
      | P.Generic_event (x1, x2, x3)  -> Generic_event (x1, x2, x3)
      | P.Event_struct (x1, x2)       -> Event_struct (x1, x2)
      | P.Event_alias (x1, x2, x3)    -> Event_alias (x1, x2, x3)
      | P.Error (x1, x2, x3)          -> Error (x1, x2, x3)
      | P.Error_alias (x1, x2, x3)    -> Error_alias (x1, x2, x3)
      | P.Struct (x1, x2)             -> Struct (x1, x2)
      | P.Union (x1, x2)              -> Union (x1, x2)
      | P.Request (x1, x2, x3)        -> Request (x1, x2, x3)
    )


  type extension =
    { name         : string
    ; file_name    : string
    ; query_name   : string option
    ; version      : (int * int) option
    ; imports      : string list
    ; declarations : declaration list }


  let pass (ext : P.extension) : extension =
    let declarations = sep_enums ext.declarations in
    { name = ext.name
    ; file_name = ext.file_name
    ; query_name = ext.query_name
    ; version = ext.version
    ; imports = ext.imports
    ; declarations }
end


module Pass_3 = struct
  (* In which we get rid of XIDs.  *)
  module P = Pass_2

  type declaration =
    | Enum of string * X.enum_items
    | Mask of string * X.mask
    | Type_alias of string * string
    | Card32_union of string * string list

    | Event of string * int * X.event
    | Generic_event of string * int * X.generic_event
    | Event_struct of string * X.allowed_events list
    | Event_alias of string * int * string

    | Error of string * int * X.error
    | Error_alias of string * int * string

    | Struct of string * X.struct_fields
    | Union of string * X.static_field list
    | Request of string * int * X.request


  let del_xids (decls : P.declaration list) : declaration list =
    ListLabels.map decls ~f:(function
      | P.X_id id ->
        Type_alias (id, "CARD32")

      | P.X_id_union (name, ids) ->
        Card32_union (name, ids)

      | P.Enum (x1, x2)               -> Enum (x1, x2)
      | P.Mask (x1, x2)               -> Mask (x1, x2)
      | P.Type_alias (x1, x2)         -> Type_alias (x1, x2)
      | P.Event (x1, x2, x3)          -> Event (x1, x2, x3)
      | P.Generic_event (x1, x2, x3)  -> Generic_event (x1, x2, x3)
      | P.Event_struct (x1, x2)       -> Event_struct (x1, x2)
      | P.Event_alias (x1, x2, x3)    -> Event_alias (x1, x2, x3)
      | P.Error (x1, x2, x3)          -> Error (x1, x2, x3)
      | P.Error_alias (x1, x2, x3)    -> Error_alias (x1, x2, x3)
      | P.Struct (x1, x2)             -> Struct (x1, x2)
      | P.Union (x1, x2)              -> Union (x1, x2)
      | P.Request (x1, x2, x3)        -> Request (x1, x2, x3)
    )


  type extension =
    { name         : string
    ; file_name    : string
    ; query_name   : string option
    ; version      : (int * int) option
    ; imports      : string list
    ; declarations : declaration list }


  let pass (ext : P.extension) : extension =
    let declarations = del_xids ext.declarations in
    { name = ext.name
    ; file_name = ext.file_name
    ; query_name = ext.query_name
    ; version = ext.version
    ; imports = ext.imports
    ; declarations }
end


module Pass_4 = struct
  (* In which we resolve the type aliases to primitive types. *)
  module P = Pass_3

  type prim =
    | Void
    | Bool
    | Byte
    | Int8
    | Int16
    | Int32
    | Uint8
    | Uint16
    | Uint32
    | Uint64
    | Float32
    | Float64

  let prim_of_string = function
    | "void"   -> Some Void
    | "char"   -> Some Byte
    | "BYTE"   -> Some Byte
    | "BOOL"   -> Some Bool
    | "INT8"   -> Some Int8
    | "INT16"  -> Some Int16
    | "INT32"  -> Some Int32
    | "fd"     -> Some Int32
    | "CARD8"  -> Some Uint8
    | "CARD16" -> Some Uint16
    | "CARD32" -> Some Uint32
    | "CARD64" -> Some Uint64
    | "float"  -> Some Float32
    | "double" -> Some Float64
    | _        -> None

  type declaration =
    | Prim of string * prim
    | Enum of string * X.enum_items
    | Mask of string * X.mask
    | Card32_union of string * string list

    | Type_alias of string * string

    | Event of string * int * X.event
    | Generic_event of string * int * X.generic_event
    | Event_struct of string * X.allowed_events list
    | Event_alias of string * int * string

    | Error of string * int * X.error
    | Error_alias of string * int * string

    | Struct of string * X.struct_fields
    | Union of string * X.static_field list
    | Request of string * int * X.request


  let add_prim (decls : P.declaration list) : declaration list =
    ListLabels.map decls ~f:(function
      | P.Type_alias (name, id) ->
        (match prim_of_string id with
        | Some p -> Prim (name, p)
        | None   -> Type_alias (name, id))

      | P.Card32_union (x1, x2)       -> Card32_union (x1, x2)
      | P.Enum (x1, x2)               -> Enum (x1, x2)
      | P.Mask (x1, x2)               -> Mask (x1, x2)
      | P.Event (x1, x2, x3)          -> Event (x1, x2, x3)
      | P.Generic_event (x1, x2, x3)  -> Generic_event (x1, x2, x3)
      | P.Event_struct (x1, x2)       -> Event_struct (x1, x2)
      | P.Event_alias (x1, x2, x3)    -> Event_alias (x1, x2, x3)
      | P.Error (x1, x2, x3)          -> Error (x1, x2, x3)
      | P.Error_alias (x1, x2, x3)    -> Error_alias (x1, x2, x3)
      | P.Struct (x1, x2)             -> Struct (x1, x2)
      | P.Union (x1, x2)              -> Union (x1, x2)
      | P.Request (x1, x2, x3)        -> Request (x1, x2, x3)
    )


  type extension =
    { name         : string
    ; file_name    : string
    ; query_name   : string option
    ; version      : (int * int) option
    ; imports      : string list
    ; declarations : declaration list }


  let pass (ext : P.extension) : extension =
    let declarations = add_prim ext.declarations in
    { name = ext.name
    ; file_name = ext.file_name
    ; query_name = ext.query_name
    ; version = ext.version
    ; imports = ext.imports
    ; declarations }
end


module Pass_5 = struct
  (* In which we resolve all type aliases. *)
  module P = Pass_4

  type prim = P.prim

  type ref_t =
    | Ref of string
    | Ext of string * string

  type x_type =
    [ `Prim of ref_t
    | `Enum of ref_t
    | `Mask of ref_t
    | `Card32_union of ref_t
    | `Struct of ref_t
    | `Union of ref_t
    | `Event_struct of ref_t ]

  type declaration =
    | Prim of string * prim
    | Enum of string * X.enum_items
    | Mask of string * X.mask
    | Card32_union of string * string list

    | Alias of string * x_type

    | Event of string * int * X.event
    | Generic_event of string * int * X.generic_event
    | Event_struct of string * X.allowed_events list
    | Event_alias of string * int * string

    | Error of string * int * X.error
    | Error_alias of string * int * string

    | Struct of string * X.struct_fields
    | Union of string * X.static_field list
    | Request of string * int * X.request


  let rec alias_of_decl exts curr_ext ref_t t : P.declaration -> x_type option = function
    | Prim (name, _) when name = t ->
      Some (`Prim (ref_t name))
    | Enum (name, _) when name = t ->
      Some (`Enum (ref_t name))
    | Mask (name, _) when name = t ->
      Some (`Mask (ref_t name))
    | Card32_union (name, _) when name = t ->
      Some (`Card32_union (ref_t name))
    | Struct (name, _) when name = t ->
      Some (`Struct (ref_t name))
    | Type_alias (name, old) when name = t ->
      (* Recursively resolve the alias to the old type until we find a proper
       * type declaration. This probably gets very inefficient, but
       * I'm pretty sure it only happens once.
       * NOTE: this is not entirely correct, because we're aliasing to the
       * original declaration's name rather than the aliased one. *)
      let t = resolve_alias exts curr_ext old in
      Some t
    | _ ->
      None


  and resolve_alias (exts : P.extension StrMap.t) (ext : P.extension) name =
    match string_split ':' name with
    | Some (ext_id, name) ->
      let ext = StrMap.find ext_id exts in
      list_get_exn (alias_of_decl exts ext (fun x -> Ext (ext_id, x)) name) ext.declarations
    | None ->
      match list_get (alias_of_decl exts ext (fun x -> Ref x) name) ext.declarations with
      | Some x -> x
      | None ->
        ext.imports |> list_get_exn (fun ext_id ->
          let ext = StrMap.find ext_id exts in
          list_get (alias_of_decl exts ext (fun x -> Ext (ext_id, x)) name)
        ext.declarations)


  type extension =
    { name         : string
    ; file_name    : string
    ; query_name   : string option
    ; version      : (int * int) option
    ; imports      : string list
    ; declarations : declaration list }


  let resolve_aliases (exts : P.extension StrMap.t) : extension StrMap.t =
    StrMap.fold exts ~init:StrMap.empty ~f:(fun ~key ~data acc ->
      let decls = ListLabels.map data.declarations ~f:(function
        | P.Type_alias (name, t) ->
          let t = resolve_alias exts data t in
          Alias (name, t)

        | P.Prim (x1, x2)               -> Prim (x1, x2)
        | P.Enum (x1, x2)               -> Enum (x1, x2)
        | P.Mask (x1, x2)               -> Mask (x1, x2)
        | P.Card32_union (x1, x2)       -> Card32_union (x1, x2)
        | P.Event (x1, x2, x3)          -> Event (x1, x2, x3)
        | P.Generic_event (x1, x2, x3)  -> Generic_event (x1, x2, x3)
        | P.Event_struct (x1, x2)       -> Event_struct (x1, x2)
        | P.Event_alias (x1, x2, x3)    -> Event_alias (x1, x2, x3)
        | P.Error (x1, x2, x3)          -> Error (x1, x2, x3)
        | P.Error_alias (x1, x2, x3)    -> Error_alias (x1, x2, x3)
        | P.Struct (x1, x2)             -> Struct (x1, x2)
        | P.Union (x1, x2)              -> Union (x1, x2)
        | P.Request (x1, x2, x3)        -> Request (x1, x2, x3)
      ) in
      let ext =
        { name = data.name; file_name = data.file_name
        ; query_name = data.query_name; version = data.version
        ; imports = data.imports; declarations = decls } in
      StrMap.add acc ~key ~data:ext
    )
end


module Pass_6 = struct
  (* In which we resolve event structs. *)
  module P = Pass_5

  type prim = Pass_4.prim

  type x_type = Pass_5.x_type

  type declaration_p6 =
    | Prim of string * prim
    | Enum of string * X.enum_items
    | Mask of string * X.mask
    | Card32_union of string * string list
    | Struct of string * X.struct_fields
    | Union of string * X.static_field list
    | Event_struct of string * P.ref_t list

    | Alias of string * x_type

    | Event of string * int * X.event
    | Generic_event of string * int * X.generic_event
    | Event_alias of string * int * string

    | Error of string * int * X.error
    | Error_alias of string * int * string

    | Request of string * int * X.request


  let gather_events max min acc : P.declaration -> string list = function
    | P.Event (name, c, _) when c >= min && c <= max -> name :: acc
    | _ -> acc


  let resolve (exts : P.extension StrMap.t) : P.declaration -> declaration_p6 = function
    | P.Event_struct (name, events) ->
      let evs = ListLabels.map events ~f:(fun { X.extension; opcode_range = (r1, r2) } ->
        (* For some reason event struct refers to an extension by its name
         * rather than its filename, so we have no choice but to find the
         * module like this. *)
        let ext_id, ext = option_get_exn @@ StrMap.fold exts ~init:None
          ~f:(fun ~key:id ~data:ext -> function
            | Some x -> Some x
            | None -> if ext.P.name = extension then Some (id, ext) else None
          ) in
        (* The order is supposedly min, max but you can never be too sure. *)
        let max, min = if r1 > r2 then r1, r2 else r2, r1 in
        List.fold_left (gather_events max min) [] ext.P.declarations
        |> List.map (fun x -> P.Ext (ext_id, x))
      ) in
      Event_struct (name, List.flatten evs)

    | P.Prim (x1, x2)               -> Prim (x1, x2)
    | P.Enum (x1, x2)               -> Enum (x1, x2)
    | P.Mask (x1, x2)               -> Mask (x1, x2)
    | P.Card32_union (x1, x2)       -> Card32_union (x1, x2)
    | P.Alias (x1, x2)              -> Alias (x1, x2)
    | P.Event (x1, x2, x3)          -> Event (x1, x2, x3)
    | P.Generic_event (x1, x2, x3)  -> Generic_event (x1, x2, x3)
    | P.Event_alias (x1, x2, x3)    -> Event_alias (x1, x2, x3)
    | P.Error (x1, x2, x3)          -> Error (x1, x2, x3)
    | P.Error_alias (x1, x2, x3)    -> Error_alias (x1, x2, x3)
    | P.Struct (x1, x2)             -> Struct (x1, x2)
    | P.Union (x1, x2)              -> Union (x1, x2)
    | P.Request (x1, x2, x3)        -> Request (x1, x2, x3)


  type extension_p6 =
    { name         : string
    ; file_name    : string
    ; query_name   : string option
    ; version      : (int * int) option
    ; imports      : string list
    ; declarations : declaration_p6 list }


  let folder exts ~key:(ext_id : string) ~data:(ext : P.extension) acc =
    let decls = List.map (resolve exts) ext.declarations in
    let ext =
      { name = ext.name; file_name = ext.file_name
      ; query_name = ext.query_name; version = ext.version
      ; imports = ext.imports
      ; declarations = decls } in
    StrMap.add acc ~key:ext_id ~data:ext


  let resolve_event_structs (exts : P.extension StrMap.t) : extension_p6 StrMap.t =
    StrMap.fold exts ~init:StrMap.empty ~f:(folder exts)
end


module Pass_7 = struct
  (* In which we resolve the event and error aliases. *)
  module P = Pass_6

  type ref_t = Pass_5.ref_t

  type prim = Pass_4.prim

  type x_type = Pass_5.x_type

  type declaration_p7 =
    | Prim of string * prim
    | Enum of string * X.enum_items
    | Mask of string * X.mask
    | Card32_union of string * string list
    | Struct of string * X.struct_fields
    | Union of string * X.static_field list
    | Event_struct of string * ref_t list

    | Alias of string * x_type
    | Event_alias of string * int * ref_t
    | Generic_event_alias of string * int * ref_t
    | Error_alias of string * int * ref_t

    | Event of string * int * X.event
    | Generic_event of string * int * X.generic_event
    | Error of string * int * X.error
    | Request of string * int * X.request


  let find_event name = function
    | P.Event (n, _, _) when n = name -> true
    | _ -> false


  let find_generic_event name = function
    | P.Generic_event (n, _, _) when n = name -> true
    | _ -> false


  let find_error name = function
    | P.Error (n, _, _) when n = name -> true
    | _ -> false


  let resolve exts curr_ext : P.declaration_p6 -> declaration_p7 = function
    | P.Event_alias (name, code, old) ->
      if List.exists (find_event old) curr_ext.P.declarations then
        Event_alias (name, code, Pass_5.Ref old)
      else if List.exists (find_generic_event old) curr_ext.P.declarations then
        Generic_event_alias (name, code, Pass_5.Ref old)
      else
        curr_ext.imports |> list_get_exn (fun ext_id ->
          let ext = StrMap.find ext_id exts in
          if List.exists (find_event old) ext.P.declarations then
            Some (Event_alias (name, code, Pass_5.Ext (ext_id, old)))
          else if List.exists (find_generic_event old) ext.P.declarations then
            Some (Generic_event_alias (name, code, Pass_5.Ext (ext_id, old)))
          else
            None
        )

    | P.Error_alias (name, code, old) ->
      let old =
        if List.exists (find_error old) curr_ext.P.declarations then
          Pass_5.Ref old
        else
          curr_ext.imports |> list_get_exn (fun ext_id ->
            let ext = StrMap.find ext_id exts in
            if List.exists (find_error old) ext.P.declarations
            then Some (Pass_5.Ext (ext_id, old)) else None
          )
      in Error_alias (name, code, old)

    | P.Prim (x1, x2)               -> Prim (x1, x2)
    | P.Enum (x1, x2)               -> Enum (x1, x2)
    | P.Mask (x1, x2)               -> Mask (x1, x2)
    | P.Card32_union (x1, x2)       -> Card32_union (x1, x2)
    | P.Alias (x1, x2)              -> Alias (x1, x2)
    | P.Event (x1, x2, x3)          -> Event (x1, x2, x3)
    | P.Event_struct (x1, x2)       -> Event_struct (x1, x2)
    | P.Generic_event (x1, x2, x3)  -> Generic_event (x1, x2, x3)
    | P.Error (x1, x2, x3)          -> Error (x1, x2, x3)
    | P.Struct (x1, x2)             -> Struct (x1, x2)
    | P.Union (x1, x2)              -> Union (x1, x2)
    | P.Request (x1, x2, x3)        -> Request (x1, x2, x3)


  type extension_p7 =
    { name         : string
    ; file_name    : string
    ; query_name   : string option
    ; version      : (int * int) option
    ; imports      : string list
    ; declarations : declaration_p7 list }


  let resolve_events_errors (exts : P.extension_p6 StrMap.t) : extension_p7 StrMap.t =
    StrMap.fold exts ~init:StrMap.empty ~f:(fun ~key:ext_id ~data:ext acc ->
      let decls = List.map (resolve exts ext) ext.declarations in
      let ext =
        { name = ext.name; file_name = ext.file_name
        ; query_name = ext.query_name; version = ext.version
        ; imports = ext.imports
        ; declarations = decls } in
      StrMap.add acc ~key:ext_id ~data:ext
    )
end


module Pass_8 = struct
  (* In which we resolve Card32 unions. *)
  module P = Pass_7

  type ref_t = Pass_5.ref_t

  type prim = Pass_4.prim

  type x_type = Pass_5.x_type

  type declaration_p8 =
    | Prim of string * prim
    | Enum of string * X.enum_items
    | Mask of string * X.mask
    | Card32_union of string * ref_t list
    | Struct of string * X.struct_fields
    | Union of string * X.static_field list
    | Event_struct of string * ref_t list

    | Alias of string * x_type
    | Event_alias of string * int * ref_t
    | Generic_event_alias of string * int * ref_t
    | Error_alias of string * int * ref_t

    | Event of string * int * X.event
    | Generic_event of string * int * X.generic_event
    | Error of string * int * X.error
    | Request of string * int * X.request


  let find_xid name = function
    | P.Prim (n, Pass_4.Uint32) | P.Alias (n, `Prim _) when n = name -> true
    | _ -> false


  let resolve_card32_union (exts : P.extension_p7 StrMap.t) (ext : P.extension_p7) name =
    match string_split ':' name with
    | Some (ext_id, name) ->
      Pass_5.Ext (ext_id, name)
    | None ->
      if List.exists (find_xid name) ext.declarations
      then Pass_5.Ref name else
        ext.imports |> list_get_exn (fun ext_id ->
          let ext = StrMap.find ext_id exts in
          if List.exists (find_xid name) ext.declarations then
            Some (Pass_5.Ext (ext_id, name))
          else
            None
        )


  let resolve exts curr_ext = function
    | P.Card32_union (name, ids) ->
      let ids = List.map (resolve_card32_union exts curr_ext) ids in
      Card32_union (name, ids)

    | P.Prim (x1, x2)                    -> Prim (x1, x2)
    | P.Enum (x1, x2)                    -> Enum (x1, x2)
    | P.Mask (x1, x2)                    -> Mask (x1, x2)
    | P.Alias (x1, x2)                   -> Alias (x1, x2)
    | P.Event_alias (x1, x2, x3)         -> Event_alias (x1, x2, x3)
    | P.Generic_event_alias (x1, x2, x3) -> Generic_event_alias (x1, x2, x3)
    | P.Error_alias (x1, x2, x3)         -> Error_alias (x1, x2, x3)
    | P.Event (x1, x2, x3)               -> Event (x1, x2, x3)
    | P.Event_struct (x1, x2)            -> Event_struct (x1, x2)
    | P.Generic_event (x1, x2, x3)       -> Generic_event (x1, x2, x3)
    | P.Error (x1, x2, x3)               -> Error (x1, x2, x3)
    | P.Struct (x1, x2)                  -> Struct (x1, x2)
    | P.Union (x1, x2)                   -> Union (x1, x2)
    | P.Request (x1, x2, x3)             -> Request (x1, x2, x3)


  type extension_p8 =
    { name         : string
    ; file_name    : string
    ; query_name   : string option
    ; version      : (int * int) option
    ; imports      : string list
    ; declarations : declaration_p8 list }


  let resolve_card32_unions (exts : P.extension_p7 StrMap.t) : extension_p8 StrMap.t =
    StrMap.fold exts ~init:StrMap.empty ~f:(fun ~key:ext_id ~data:ext acc ->
      let decls = List.map (resolve exts ext) ext.declarations in
      let ext =
        { name = ext.name; file_name = ext.file_name
        ; query_name = ext.query_name; version = ext.version
        ; imports = ext.imports
        ; declarations = decls } in
      StrMap.add acc ~key:ext_id ~data:ext
    )
end


module Pass_9 = struct
  (* In which we resolve the types of all struct fields. *)
  module P = Pass_8

  type ref_t = Pass_5.ref_t

  type prim = Pass_4.prim

  type x_type = Pass_5.x_type

  type basic =
    [ `Prim of prim
    | `Prim_ref of ref_t
    | `Card32_union of ref_t ]

  type complex =
    [ `Struct of ref_t
    | `Union of ref_t
    | `Event_struct of ref_t ]

  type enumeration =
    [ `Enum of ref_t * basic
    | `Mask of ref_t * basic
    | `Alt_enum of ref_t * basic
    | `Alt_mask of ref_t * basic ]

  type field_type = [ basic | complex | enumeration ]

  type static_field =
    [ `Pad of X.padding
    | `Field of string * field_type
    | `List of string * field_type * X.expression
    | `File_descriptor of string ]

  type dynamic_field =
    [ static_field  | `List_var of string * field_type ]

  type request_field =
    [ dynamic_field | `Expr of string * basic * X.expression ]

  type switch =
    { align : X.required_start_align option
    ; cond : X.cond
    ; cases : case list }

  and case =
    { exprs : X.expression list
    ; name  : string option
    ; align_c : X.required_start_align option
    ; fields : static_field list
    ; switch : (string * switch) option }

  type event =
    { no_sequence_number : bool
    ; align : X.required_start_align option
    ; fields : static_field list }

  type generic_event =
    { no_sequence_number : bool
    ; align : X.required_start_align option
    ; fields : dynamic_field list }

  type error =
    { align  : X.required_start_align option
    ; fields : static_field list }

  type struct_fields =
    { fields : static_field list
    ; switch : (string * switch) option }

  type request_fields =
    { align  : X.required_start_align option
    ; fields : request_field list
    ; switch : (string * switch) option }

  type reply =
    { align  : X.required_start_align option
    ; fields : dynamic_field list
    ; switch : (string * switch) option }

  type request =
    { combine_adjacent : bool
    ; params : request_fields
    ; reply : reply option }

  type declaration_p9 =
    | Prim of string * prim
    | Enum of string * X.enum_items
    | Mask of string * X.mask
    | Card32_union of string * ref_t list
    | Struct of string * struct_fields
    | Union of string * static_field list
    | Event_struct of string * ref_t list

    | Alias of string * x_type
    | Event_alias of string * int * ref_t
    | Generic_event_alias of string * int * ref_t
    | Error_alias of string * int * ref_t

    | Event of string * int * event
    | Generic_event of string * int * generic_event
    | Error of string * int * error
    | Request of string * int * request


  let find_basic ref_t name : P.declaration_p8 -> basic option = function
    | P.Prim (n, _) | P.Alias (n, `Prim _) when n = name ->
      Some (`Prim_ref (ref_t n))
    | P.Card32_union (n, _) | P.Alias (n, `Card32_union _) when n = name ->
      Some (`Card32_union (ref_t n))
    | _ -> None

  let find_complex ref_t name : P.declaration_p8 -> complex option = function
    | P.Struct (n, _) | P.Alias (n, `Struct _) when n = name ->
      Some (`Struct (ref_t n))
    | P.Union (n, _) | P.Alias (n, `Union _) when n = name ->
      Some (`Union (ref_t n))
    | P.Event_struct (n, _) | P.Alias (n, `Event_struct _) when n = name ->
      Some (`Event_struct (ref_t n))
    | _ -> None

  let find_field_type ref_t name x : field_type option =
    (find_basic ref_t name x :> field_type option)
      <||> lazy (find_complex ref_t name x :> field_type option)


  let resolve_basic (exts : P.extension_p8 StrMap.t) (curr_ext : P.extension_p8) : X.field_type -> basic = function
    | { X.typ; allowed = Some enumeration } ->
      failwith "invalid basic type"

    | { X.typ; allowed = None } ->
      option_map (fun t -> `Prim t) (Pass_4.prim_of_string typ) <|!|> lazy begin
        match string_split ':' typ with
        | Some (ext_id, name) when ext_id = curr_ext.file_name ->
          list_get_exn (find_basic (fun n -> Pass_5.Ref n) name) curr_ext.declarations
        | Some (ext_id, name) ->
          let ext = StrMap.find ext_id exts in
          list_get_exn (find_basic (fun n -> Pass_5.Ext (ext_id, n)) name) ext.declarations
        | None ->
          list_get (find_basic (fun n -> Pass_5.Ref n) typ) curr_ext.declarations
          <|!|> lazy (curr_ext.imports |> list_get_exn (fun ext_id ->
              let ext = StrMap.find ext_id exts in
              list_get (find_basic (fun n -> Pass_5.Ext (ext_id, n)) typ) ext.declarations
            ))
      end



  let resolve_field_type (exts : P.extension_p8 StrMap.t) (curr_ext : P.extension_p8) = function
    | { X.typ; allowed = Some enumeration } ->
      begin
      let typ =
        option_map (fun t -> `Prim t) (Pass_4.prim_of_string typ) <|!|> lazy begin
          match string_split ':' typ with
          | Some (ext_id, name) when ext_id = curr_ext.file_name ->
            list_get_exn (find_basic (fun n -> Pass_5.Ref n) name) curr_ext.declarations
          | Some (ext_id, name) ->
            let ext = StrMap.find ext_id exts in
            list_get_exn (find_basic (fun n -> Pass_5.Ext (ext_id, n)) name) ext.declarations
          | None ->
            list_get (find_basic (fun n -> Pass_5.Ref n) typ) curr_ext.declarations
            <|!|> lazy (curr_ext.imports |> list_get_exn (fun ext_id ->
                let ext = StrMap.find ext_id exts in
                list_get (find_basic (fun n -> Pass_5.Ext (ext_id, n)) typ) ext.declarations
              ))
        end
      in
      let process_enum test name = match string_split ':' name with
        | Some (ext_id, name) when ext_id = curr_ext.file_name ->
          Pass_5.Ref name
        | Some (ext_id, name) ->
          Pass_5.Ext (ext_id, name)
        | None ->
          if List.exists test curr_ext.declarations
          then Pass_5.Ref name
          else curr_ext.imports |> list_get_exn (fun ext_id ->
            let ext = StrMap.find ext_id exts in
            if List.exists test ext.declarations
            then Some (Pass_5.Ext (ext_id, name))
            else None
          )
      in
      match enumeration with
      | `Enum n ->
        let e = process_enum (function Enum (name, _) when name = n -> true | _ -> false) n in
        `Enum (e, typ)
      | `Alt_enum n ->
          (* ***********************************************************************
           * FIXME FIXME FIXME FIXME FIXME
           * So apparently we can't know ahead of time whether an enum is an enum or
           * a mask, because FUCKING XINPUT decided to use the bit index syntax to
           * represent a number, probably because they were too lazy to look up
           * how to write 1 << 31. These declarations are what ultimately decides
           * what is an enum and what is a mask, unless the spec decides to fuck me
           * over again.
           * *********************************************************************** *)
          begin try
        let e = process_enum (function Enum (name, _) when name = n -> true | _ -> false) n in
        `Alt_enum (e, typ)
          with Not_found -> failwithf "cound't find %s" n end
      | `Mask n ->
        let e = process_enum (function Mask (name, _) when name = n -> true | _ -> false) n in
        `Mask (e, typ)
      | `Alt_mask n ->
        let e = process_enum (function Mask (name, _) when name = n -> true | _ -> false) n in
        `Alt_mask (e, typ)
      end

    | { X.typ; allowed = None } ->
      begin try
      option_map (fun t -> `Prim t) (Pass_4.prim_of_string typ) <|!|> lazy begin
        match string_split ':' typ with
        | Some (ext_id, name) when ext_id = curr_ext.file_name ->
          list_get_exn (find_field_type (fun n -> Pass_5.Ref n) name) curr_ext.declarations
        | Some (ext_id, name) ->
          let ext = StrMap.find ext_id exts in
          list_get_exn (find_field_type (fun n -> Pass_5.Ext (ext_id, n)) name) ext.declarations
        | None ->
          list_get (find_field_type (fun n -> Pass_5.Ref n) typ) curr_ext.declarations
          <|!|> lazy (curr_ext.imports |> list_get_exn (fun ext_id ->
              let ext = StrMap.find ext_id exts in
              list_get (find_field_type (fun n -> Pass_5.Ext (ext_id, n)) typ) ext.declarations
            ))
      end
      with Not_found -> failwithf "couldn't find type %s" typ end


  let mk_static_field exts curr_ext : X.static_field -> static_field = function
    | `Field (name, typ) ->
      let typ = resolve_field_type exts curr_ext typ in
      `Field (name, typ)

    | `List (name, typ, expr) ->
      let typ = resolve_field_type exts curr_ext typ in
      `List (name, typ, expr)

    | `File_descriptor name -> `File_descriptor name
    | `Pad pad              -> `Pad pad


  let mk_dynamic_field exts curr_ext : X.dynamic_field -> dynamic_field = function
    | `List_var (name, typ) ->
      let typ = resolve_field_type exts curr_ext typ in
      `List_var (name, typ)
    | #X.static_field as f ->
      (mk_static_field exts curr_ext f :> dynamic_field)


  let mk_request_field exts curr_ext : X.request_field -> request_field = function
    | `Expr (name, basic, expr) ->
      let typ = resolve_basic exts curr_ext basic in
      `Expr (name, typ, expr)
    | #X.dynamic_field as f ->
      (mk_dynamic_field exts curr_ext f :> request_field)


  let rec mk_switch exts curr_ext { X.align; cond; cases } : switch =
    let cases = List.map (mk_case exts curr_ext) cases in
    { align; cond; cases }

  and mk_case exts curr_ext { X.exprs; name; align_c; fields; switch } : case =
    let fields = List.map (mk_static_field exts curr_ext) fields in
    let switch = option_map (fun (name, switch) -> name, mk_switch exts curr_ext switch) switch in
    { exprs; name; align_c; fields; switch }


  let resolve exts curr_ext = function
    | P.Struct (name, { fields; switch }) ->
      let fields = List.map (mk_static_field exts curr_ext) fields in
      let switch = option_map (fun (name, switch) -> name, mk_switch exts curr_ext switch) switch in
      Struct (name, { fields; switch })

    | P.Union (name, fields) ->
      let fields = List.map (mk_static_field exts curr_ext) fields in
      Union (name, fields)

    | P.Event (name, code, { no_sequence_number; align; fields }) ->
      let fields = List.map (mk_static_field exts curr_ext) fields in
      Event (name, code, { no_sequence_number; align; fields })

    | P.Generic_event (name, code, { no_sequence_number; align; fields }) ->
      let fields = List.map (mk_dynamic_field exts curr_ext) fields in
      Generic_event (name, code, { no_sequence_number; align; fields })

    | P.Error (name, code, { X.align; fields }) ->
      let fields = List.map (mk_static_field exts curr_ext) fields in
      Error (name, code, { align; fields })

    | P.Request (name, code, { X.combine_adjacent
        ; params = { align = req_align; fields = req_fields; switch = req_switch }; reply }) ->
      let req_fields = List.map (mk_request_field exts curr_ext) req_fields in
      let req_switch = option_map (fun (name, switch) -> name, mk_switch exts curr_ext switch) req_switch in
      let params : request_fields = { align = req_align; fields = req_fields; switch = req_switch } in
      let reply = reply |> option_map (fun { X.align; fields; switch } ->
        let fields = List.map (mk_dynamic_field exts curr_ext) fields in
        let switch = option_map (fun (name, switch) -> name, mk_switch exts curr_ext switch) switch in
        { align; fields; switch }
      ) in
      Request (name, code, { combine_adjacent; reply; params })

    | P.Prim (x1, x2)                    -> Prim (x1, x2)
    | P.Card32_union (x1, x2)            -> Card32_union (x1, x2)
    | P.Enum (x1, x2)                    -> Enum (x1, x2)
    | P.Mask (x1, x2)                    -> Mask (x1, x2)
    | P.Event_struct (x1, x2)            -> Event_struct (x1, x2)
    | P.Alias (x1, x2)                   -> Alias (x1, x2)
    | P.Event_alias (x1, x2, x3)         -> Event_alias (x1, x2, x3)
    | P.Generic_event_alias (x1, x2, x3) -> Generic_event_alias (x1, x2, x3)
    | P.Error_alias (x1, x2, x3)         -> Error_alias (x1, x2, x3)


  type extension_p9 =
    { name         : string
    ; file_name    : string
    ; query_name   : string option
    ; version      : (int * int) option
    ; imports      : string list
    ; declarations : declaration_p9 list }


  let resolve_field_types (exts : P.extension_p8 StrMap.t) : extension_p9 StrMap.t =
    StrMap.fold exts ~init:StrMap.empty ~f:(fun ~key:ext_id ~data:ext acc ->
      let decls = List.map (resolve exts ext) ext.declarations in
      let ext =
        { name = ext.name; file_name = ext.file_name
        ; query_name = ext.query_name; version = ext.version
        ; imports = ext.imports
        ; declarations = decls } in
      StrMap.add acc ~key:ext_id ~data:ext
    )
end


let%test_unit _ =
  let files =
    [ "bigreq"; "composite"; "damage"; "dpms"; "dri2"; "dri3"; "ge"; "glx"
    ; "present"; "randr"; "record"; "render"; "res"; "screensaver"; "shape"
    ; "shm"; "sync"; "xc_misc"; "xevie"; "xf86dri"; "xf86vidmode"; "xfixes"
    ; "xinerama"; "xinput"; "xkb"; "xprint"; "xproto"; "xselinux"; "xtest"
    ; "xvmc"; "xv" ] in
  let exts = List.map Pass_1.load_extension files in
  let exts = List.map Pass_2.pass exts in
  let exts = List.map Pass_3.pass exts in
  let exts = List.map Pass_4.pass exts in
  let exts = ListLabels.fold_left exts ~init:StrMap.empty
    ~f:(fun acc ext -> StrMap.add acc ~key:ext.Pass_4.file_name ~data:ext) in
  let exts = Pass_5.resolve_aliases exts in
  let exts = Pass_6.resolve_event_structs exts in
  let exts = Pass_7.resolve_events_errors exts in
  let exts = Pass_8.resolve_card32_unions exts in
  let exts = Pass_9.resolve_field_types exts in
  ()

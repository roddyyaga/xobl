module X = Parser


let string_split chr str =
  let len = String.length str in
  let rec pos i =
    if i >= len then
      None
    else if str.[i] = chr then
      let left = StringLabels.sub str ~pos:0 ~len:(if i = 0 then 0 else i - 1) in
      let right = StringLabels.sub str ~pos:(i + 1) ~len:(len - i - 1) in
      Some (left, right)
    else
      pos (i + 1)
  in pos 0

let option_get_exn = function
  | Some x -> x
  | None -> raise Not_found


let failwithf fmt = Printf.ksprintf failwith fmt


let ( <||> ) x y =
  match x with
  | Some _ -> x
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
  (** In which we load every extension and abstract imports into the extension
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
  (** In which we separate Enum declarations into enums and masks.  *)
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
  (** In which we get rid of the notion of XIDs.  *)
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
  (** In which we resolve the type aliases to primitive types. *)
  module P = Pass_3

  type prim =
    | Bool
    | Byte
    | Int8
    | Int16
    | Int32
    | Uint8
    | Uint16
    | Uint32
    | Float32
    | Float64

  let prim_of_string = function
    | "char"   -> Some Byte
    | "BYTE"   -> Some Byte
    | "BOOL"   -> Some Bool
    | "INT16"  -> Some Int16
    | "INT32"  -> Some Int32
    | "CARD8"  -> Some Uint8
    | "CARD16" -> Some Uint16
    | "CARD32" -> Some Uint32
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
  (* In which we resolve the types of all aliases. *)
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
    | `Struct of ref_t ]

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
  ()


(*
let%test_unit "pass 1" =
  let files =
    [ "bigreq"; "composite"; "damage"; "dpms"; "dri2"; "dri3"; "ge"; "glx"
    ; "present"; "randr"; "record"; "render"; "res"; "screensaver"; "shape"
    ; "shm"; "sync"; "xc_misc"; "xevie"; "xf86dri"; "xf86vidmode"; "xfixes"
    ; "xinerama"; "xinput"; "xkb"; "xprint"; "xproto"; "xselinux"; "xtest"
    ; "xvmc"; "xv" ]
  in
  let _ = Pass_1.load_extensions files in
  ()
  *)

module X = Parser
open Util


let resolve_extension_path name =
  Filename.concat "xproto/src" (name ^ ".xml")


let load_extension file_name : X.protocol_file =
  resolve_extension_path file_name
  |> X.parse_file



type 'a extension =
  { name         : string
  ; file_name    : string
  ; query_name   : string option
  ; version      : (int * int) option
  ; imports      : string list
  ; declarations : 'a list }


let mk_pass (type n o)
    (f : o extension -> o -> n)
    (exts : o extension String_map.t)
    : n extension String_map.t =
  String_map.fold exts ~init:String_map.empty ~f:(fun ~key: ext_id ~data:ext acc ->
    let declarations = List.map (f ext) ext.declarations in
    let ext =
      { declarations
      ; name = ext.name; file_name = ext.file_name
      ; query_name = ext.query_name; version = ext.version
      ; imports = ext.imports }
    in
    String_map.add acc ~key:ext_id ~data:ext
  )



(** Multi-purpose ID type that should be used for all references to things
   declared elsewhere. *)
type id =
  | Id of string              (** ID in the current extension. *)
  | Ext_id of string * string (** ID in a different extension. *)


(** An X primitive type. *)
type x_type =
  | Basic of string
  | Ref of id


let primitive_types =
  [ "void"
  ; "char"
  ; "BYTE"
  ; "BOOL"
  ; "INT8"
  ; "INT16"
  ; "INT32"
  ; "fd"
  ; "CARD8"
  ; "CARD16"
  ; "CARD32"
  ; "CARD64"
  ; "float"
  ; "double"
  ; "XID" (** maps to a card32 *)
  ]


let is_primitive x = List.mem x primitive_types


module Pass_0 = struct
  type declaration_p0 =
    [ `X_id of string
    | `X_id_union of string * string list
    | `Enum of string * X.enum
    | `Type_alias of string * string

    | `Event of string * int * X.event
    | `Generic_event of string * int * X.generic_event
    | `Event_struct of string * X.allowed_events list
    | `Event_alias of string * int * string

    | `Error of string * int * X.error
    | `Error_alias of string * int * string

    | `Struct of string * X.struct_fields
    | `Union of string * X.static_field list
    | `Request of string * int * X.request ]


  let sep_imports (decls : X.declaration list) : string list * declaration_p0 list =
    let imports, decls =
      ListLabels.fold_left decls ~init:([], []) ~f:(fun (imports, decls) ->
        function
        | `Import id ->
          (id :: imports), decls

        | #declaration_p0 as d ->
          imports, (d :: decls)
      )
    in
    List.rev imports, List.rev decls


  type extension_p0 = declaration_p0 extension


  let lift_imports : X.protocol_file -> extension_p0 = function
    | Core decls ->
      let imports, declarations = sep_imports decls in
      { name = "Xproto"; file_name = "xproto"
      ; query_name = None; version = None
      ; imports; declarations }

    | Extension ({ name; query_name; version; file_name; _ }, decls) ->
      let imports, declarations = sep_imports decls in
      { query_name = Some query_name; version = Some version
      ; name; file_name; imports; declarations }
end



module Cache : sig
  val init : Pass_0.extension_p0 list -> unit

  val lookup_type : 'a extension -> string -> x_type

  val lookup_type_id : 'a extension -> string -> id
  (** Like [lookup_type], but doesn't look at primitive types *)

  val lookup_enum : 'a extension -> [ `Enum | `Mask ] -> string -> id
  (** We want to gather usage statistics on the enums so that we can output
     the declarations that are needed, so we also need to know if the caller
     requests it as an enum or as mask. *)
end = struct
  type 'a table =
    { name : string
    ; ext  : string
    ; mutable refs : 'a }

  let add_to_table tbl ext name refs =
    tbl := { ext; name; refs } :: !tbl

  let lookup tbl (update : 'a -> 'a) used_ext name =
    let get name ext t = t.name = name && t.ext = ext in
    String'.split ':' name
    |> Option.map (fun (ext, name) ->
        let item = List.find (get name ext) !tbl in
        item.refs <- update item.refs;
        if used_ext.file_name = ext then
          Id name
        else
          Ext_id (ext, name)
      )
    |> Option.or_lazy (lazy (
        List.find_opt (get name used_ext.file_name) !tbl
        |> Option.map (fun item ->
            item.refs <- update item.refs;
            Id name
          )
      ))
    |> Option.with_default_lazy (lazy (
        used_ext.imports |> List'.first_exn (fun ext ->
          List.find_opt (get name ext) !tbl
          |> Option.map (fun item ->
              item.refs <- update item.refs;
              Ext_id (ext, name)
            )
      )))


  (* *)
  let type_table : int table list ref = ref []

  let lookup_type_id used_ext name =
    lookup type_table (fun x -> x + 1) used_ext name

  let lookup_type used_ext name =
    if is_primitive name then
      Basic name
    else
      Ref (lookup_type_id used_ext name)


  (* *)
  type enum_refs = { enums : int; masks : int }

  let enum_table : enum_refs table list ref = ref []

  let lookup_enum ext x =
    let f { enums; masks } = match x with
      | `Enum -> { enums = enums + 1; masks }
      | `Mask -> { masks = masks + 1; enums }
    in
    lookup enum_table f ext

  let print_enums () =
    !enum_table |> List.iter (fun { name; ext; refs = { enums; masks } } ->
      Printf.printf "%s:%s -> enums = %d | masks = %d\n"
        ext name enums masks
    )


  let init =
    List.iter (fun ext ->
      ext.declarations |> List.iter (function
        | `X_id name
        | `X_id_union (name, _)
        | `Type_alias (name, _)
        | `Event_struct (name, _)
        | `Struct (name, _)
        | `Union (name, _) ->
          add_to_table type_table ext.file_name name 0

        | `Enum (name, _) ->
          add_to_table enum_table ext.file_name name { enums = 0; masks = 0 }

        | _ -> ()
    ))
end



(** In which we resolve {b all} types *)
module Pass_1 = struct
  module P = Pass_0

  (* Struct fields *)
  type field_type =
    { typ  : x_type
    ; enum : X.allowed_vals option }

  type static_field =
    [ `Pad of X.padding
    | `Field of string * field_type
    | `List of string * field_type * X.expression
    | `File_descriptor of string ]

  type dynamic_field =
    [ static_field
    | `List_var of string * field_type ]

  type request_field =
    [ dynamic_field
    | `Expr of string * field_type * X.expression ]


  type switch =
    { align : X.required_start_align option
    ; cond  : X.cond
    ; cases : case list }

  and case =
    { exprs   : X.expression list
    ; name    : string option
    ; align_c : X.required_start_align option
    ; fields  : static_field list
    ; switch  : (string * switch) option }


  (* Structs *)
  type event =
    { no_sequence_number : bool
    ; align  : X.required_start_align option
    ; fields : static_field list }

  type generic_event =
    { no_sequence_number : bool
    ; align  : X.required_start_align option
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
    ; reply  : reply option }


  type common_p0_p1 =
    [ `Enum of string * X.enum

    | `Event_struct of string * X.allowed_events list

    | `Event_alias of string * int * string
    | `Error_alias of string * int * string ]

  type declaration_p1 =
    [ common_p0_p1
    | `Alias of string * x_type
    | `X_id_union of string * id list

    | `Struct of string * struct_fields
    | `Union of string * static_field list

    | `Event of string * int * event
    | `Generic_event of string * int * generic_event
    | `Error of string * int * error

    | `Request of string * int * request ]


  (* A bit of boilerplate for the static/dynamic/request fields *)
  let resolve_in_static_field ext : X.static_field -> static_field = function
    | `Field (name, t) ->
      let t = { enum = t.allowed; typ = Cache.lookup_type ext t.typ } in
      `Field (name, t)
    | `List (name, t, expr) ->
      let t = { enum = t.allowed; typ = Cache.lookup_type ext t.typ } in
      `List (name, t, expr)
    | `Pad _ | (`File_descriptor _) as f ->
      f

  let resolve_in_dynamic_field ext : X.dynamic_field -> dynamic_field = function
    | `List_var (name, t) ->
      let t = { enum = t.allowed; typ = Cache.lookup_type ext t.typ } in
      `List_var (name, t)
    | #X.static_field as f ->
      (resolve_in_static_field ext f :> dynamic_field)

  let resolve_in_request_field ext : X.request_field -> request_field = function
    | `Expr (name, t, expr) ->
      let t = { enum = t.allowed; typ = Cache.lookup_type ext t.typ } in
      `Expr (name, t, expr)
    | #X.dynamic_field as f ->
      (resolve_in_dynamic_field ext f :> request_field)


  (* Boilerplate for switch/case *)
  let rec resolve_in_switch ext : X.switch -> switch =
    fun { align; cond; cases } ->
      let cases = List.map (resolve_in_case ext) cases in
      { align; cond; cases }

  and resolve_in_case ext : X.case -> case =
    fun { exprs; name; align_c; fields; switch } ->
      let fields = List.map (resolve_in_static_field ext) fields in
      let switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) switch in
      { exprs; name; align_c; fields; switch }


  (* Boilerplate for events/errors/requests. This would be so much cleaner
     with row types... *)
  let resolve_in_event ext : X.event -> event =
    fun { no_sequence_number; align; fields } ->
      let fields = List.map (resolve_in_static_field ext) fields in
      { no_sequence_number; align; fields }

  let resolve_in_generic_event ext : X.generic_event -> generic_event =
    fun { no_sequence_number; align; fields } ->
      let fields = List.map (resolve_in_dynamic_field ext) fields in
      { no_sequence_number; align; fields }

  let resolve_in_error ext : X.error -> error =
    fun { align; fields } ->
      let fields = List.map (resolve_in_static_field ext) fields in
      { align; fields }

  let resolve_in_struct_fields ext : X.struct_fields -> struct_fields =
    fun { fields; switch } ->
      let fields = List.map (resolve_in_static_field ext) fields in
      let switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) switch in
      { fields; switch }

  let resolve_in_request_fields ext : X.request_fields -> request_fields =
    fun { align; fields; switch } ->
      let fields = List.map (resolve_in_request_field ext) fields in
      let switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) switch in
      { align; fields; switch }

  let resolve_in_reply ext : X.reply -> reply =
    fun { align; fields; switch } ->
      let fields = List.map (resolve_in_dynamic_field ext) fields in
      let switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) switch in
      { align; fields; switch }


  (* Finally, we get to resolve the damn things. *)

  let resolve_type ext : P.declaration_p0 -> declaration_p1 = function
    | `X_id id ->
      `Alias (id, Basic "XID")

    | `Type_alias (name, t) ->
      let t = Cache.lookup_type ext t in
      `Alias (name, t)

    | `X_id_union (name, ids) ->
      let ids = List.map (fun t -> Cache.lookup_type_id ext t) ids in
      `X_id_union (name, ids)

    | `Struct (name, f) ->
      let f = resolve_in_struct_fields ext f in
      `Struct (name, f)

    | `Union (name, f) ->
      let f = List.map (resolve_in_static_field ext) f in
      `Union (name, f)

    | `Event (name, n, f) ->
      let f = resolve_in_event ext f in
      `Event (name, n, f)

    | `Generic_event (name, n, f) ->
      let f = resolve_in_generic_event ext f in
      `Generic_event (name, n, f)

    | `Error (name, n, f) ->
      let f = resolve_in_error ext f in
      `Error (name, n, f)

    | `Request (name, n, { combine_adjacent; params; reply }) ->
      let params = resolve_in_request_fields ext params in
      let reply = Option.map (resolve_in_reply ext) reply in
      `Request (name, n, { combine_adjacent; params; reply })

    | #common_p0_p1 as d ->
      d


  type extension_p1 = declaration_p1 extension

  let resolve_types (ext : P.extension_p0) : extension_p1 =
    let declarations = List.map (resolve_type ext) ext.declarations in
    { ext with declarations }
end



(** In which we resolve the enum references. *)
module Pass_2 = struct
  module P = Pass_1

  type field_type =
    | Prim of x_type
    | Enum of id * x_type
    | Mask of id * x_type
    | Enum_or of id * x_type
    | Mask_or of id * x_type

  type expression =
    [ `Binop of X.binop * expression * expression
    | `Unop of X.unop * expression
    | `Field_ref of string
    | `Param_ref of string * string
    | `Enum_ref of id * string
    | `Sum_of of string * expression option
    | `Current_ref
    | `Pop_count of expression
    | `Value of int
    | `Bit of int ]


  (* TYPE BOILERPLATE *)
  type static_field =
    [ `Pad of X.padding
    | `Field of string * field_type
    | `List of string * field_type * expression
    | `File_descriptor of string ]

  type dynamic_field =
    [ static_field
    | `List_var of string * field_type ]

  type request_field =
    [ dynamic_field
    | `Expr of string * field_type * expression ]


  (* SOME MORE TYPE BOILERPLATE IN CASE THAT WASN'T ENOUGH *)
  type cond =
    [ `Bit_and of expression
    | `Eq of expression ]

  type switch =
    { align : X.required_start_align option
    ; cond  : cond
    ; cases : case list }

  and case =
    { exprs   : expression list
    ; name    : string option
    ; align_c : X.required_start_align option
    ; fields  : static_field list
    ; switch  : (string * switch) option }


  (* BOILERPLATE FOR THE BOILERPLATE GOD *)
  type event =
    { no_sequence_number : bool
    ; align  : X.required_start_align option
    ; fields : static_field list }

  type generic_event =
    { no_sequence_number : bool
    ; align  : X.required_start_align option
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
    ; reply  : reply option }


  type common_p1_p2 =
    [ `Alias of string * x_type
    | `X_id_union of string * id list

    | `Enum of string * X.enum

    | `Event_struct of string * X.allowed_events list

    | `Event_alias of string * int * string
    | `Error_alias of string * int * string ]

  type declaration_p2 =
    [ common_p1_p2
    | `Struct of string * struct_fields
    | `Union of string * static_field list

    | `Event of string * int * event
    | `Generic_event of string * int * generic_event
    | `Error of string * int * error

    | `Request of string * int * request ]


  let field_type ext : P.field_type -> field_type =
    fun { enum; typ } -> match enum with
      | Some (`Enum name) ->
        Enum (Cache.lookup_enum ext `Enum name, typ)
      | Some (`Mask name) ->
        Mask (Cache.lookup_enum ext `Mask name, typ)
      | Some (`Alt_enum name) ->
        Enum_or (Cache.lookup_enum ext `Enum name, typ)
      | Some (`Alt_mask name) ->
        Mask_or (Cache.lookup_enum ext `Mask name, typ)
      | None ->
        Prim typ


  let rec expression ext : X.expression -> expression = function
    | `Enum_ref (enum, item) ->
      let enum = Cache.lookup_enum ext `Enum enum in
      `Enum_ref (enum, item)

    | `Binop (op, e1, e2) ->
      `Binop (op, expression ext e1, expression ext e2)
    | `Unop (op, e) ->
      `Unop (op, expression ext e)
    | `Sum_of (n, e) ->
      `Sum_of (n, Option.map (expression ext) e)
    | `Pop_count e ->
      `Pop_count (expression ext e)

    | `Field_ref _ | `Param_ref _ | `Value _ | `Bit _ | `Current_ref as e ->
      e


  (* BOILERPLATE *)
  let resolve_in_static_field ext : P.static_field -> static_field = function
    | `Field (name, t) ->
      let t = field_type ext t in
      `Field (name, t)
    | `List (name, t, expr) ->
      let t = field_type ext t in
      let expr = expression ext expr in
      `List (name, t, expr)
    | `Pad _ | (`File_descriptor _) as f ->
      f

  let resolve_in_dynamic_field ext : P.dynamic_field -> dynamic_field = function
    | `List_var (name, t) ->
      let t = field_type ext t in
      `List_var (name, t)
    | #P.static_field as f ->
      (resolve_in_static_field ext f :> dynamic_field)

  let resolve_in_request_field ext : P.request_field -> request_field = function
    | `Expr (name, t, expr) ->
      let t = field_type ext t in
      let expr = expression ext expr in
      `Expr (name, t, expr)
    | #P.dynamic_field as f ->
      (resolve_in_dynamic_field ext f :> request_field)


  (* MORE BOILERPLATE *)
  let resolve_in_cond ext : X.cond -> cond = function
    | `Bit_and expr ->
      let expr = expression ext expr in
      `Bit_and expr
    | `Eq expr ->
      let expr = expression ext expr in
      `Eq expr

  let rec resolve_in_switch ext : P.switch -> switch =
    fun { align; cond; cases } ->
      let cases = List.map (resolve_in_case ext) cases in
      let cond = resolve_in_cond ext cond in
      { align; cond; cases }

  and resolve_in_case ext : P.case -> case =
    fun { exprs; name; align_c; fields; switch } ->
      let exprs = List.map (expression ext) exprs in
      let fields = List.map (resolve_in_static_field ext) fields in
      let switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) switch in
      { exprs; name; align_c; fields; switch }


  (* EVEN MORE BOILERPLATE *)
  let resolve_in_event ext : P.event -> event =
    fun { no_sequence_number; align; fields } ->
      let fields = List.map (resolve_in_static_field ext) fields in
      { no_sequence_number; align; fields }

  let resolve_in_generic_event ext : P.generic_event -> generic_event =
    fun { no_sequence_number; align; fields } ->
      let fields = List.map (resolve_in_dynamic_field ext) fields in
      { no_sequence_number; align; fields }

  let resolve_in_error ext : P.error -> error =
    fun { align; fields } ->
      let fields = List.map (resolve_in_static_field ext) fields in
      { align; fields }

  let resolve_in_struct_fields ext : P.struct_fields -> struct_fields =
    fun { fields; switch } ->
      let fields = List.map (resolve_in_static_field ext) fields in
      let switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) switch in
      { fields; switch }

  let resolve_in_request_fields ext : P.request_fields -> request_fields =
    fun { align; fields; switch } ->
      let fields = List.map (resolve_in_request_field ext) fields in
      let switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) switch in
      { align; fields; switch }

  let resolve_in_reply ext : P.reply -> reply =
    fun { align; fields; switch } ->
      let fields = List.map (resolve_in_dynamic_field ext) fields in
      let switch = Option.map (fun (name, s) -> (name, resolve_in_switch ext s)) switch in
      { align; fields; switch }


  (* FINALLY, THE TOPLEVEL BOILERPLATE *)
  let resolve_enum ext : P.declaration_p1 -> declaration_p2 = function
    | `Struct (name, f) ->
      let f = resolve_in_struct_fields ext f in
      `Struct (name, f)

    | `Union (name, f) ->
      let f = List.map (resolve_in_static_field ext) f in
      `Union (name, f)

    | `Event (name, n, f) ->
      let f = resolve_in_event ext f in
      `Event (name, n, f)

    | `Generic_event (name, n, f) ->
      let f = resolve_in_generic_event ext f in
      `Generic_event (name, n, f)

    | `Error (name, n, f) ->
      let f = resolve_in_error ext f in
      `Error (name, n, f)

    | `Request (name, n, { combine_adjacent; params; reply }) ->
      let params = resolve_in_request_fields ext params in
      let reply = Option.map (resolve_in_reply ext) reply in
      `Request (name, n, { combine_adjacent; params; reply })

    | #common_p1_p2 as d ->
      d


  type extension_p2 = declaration_p2 extension

  let resolve_enums (ext : P.extension_p1) : extension_p2 =
    let declarations = List.map (resolve_enum ext) ext.declarations in
    { ext with declarations }
end



let%test_unit "pipeline test" =
  let files =
    [ "bigreq"; "composite"; "damage"; "dpms"; "dri2"; "dri3"; "ge"; "glx"
    ; "present"; "randr"; "record"; "render"; "res"; "screensaver"; "shape"
    ; "shm"; "sync"; "xc_misc"; "xevie"; "xf86dri"; "xf86vidmode"; "xfixes"
    ; "xinerama"; "xinput"; "xkb"; "xprint"; "xproto"; "xselinux"; "xtest"
    ; "xvmc"; "xv" ]
  in
  let exts = List.map (fun f -> load_extension f |> Pass_0.lift_imports) files in
  Cache.init exts;
  let exts = List.map Pass_1.resolve_types exts in
  let _exts = List.map Pass_2.resolve_enums exts in
  ()

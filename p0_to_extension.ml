type declaration =
  [ `X_id of string
  | `X_id_union of string * string list
  | `Enum of string * Parser.enum
  | `Type_alias of string * string
  | `Event of string * int * Parser.event
  | `Generic_event of string * int * Parser.generic_event
  | `Event_struct of string * Parser.allowed_events list
  | `Event_alias of string * int * string
  | `Error of string * int * Parser.error
  | `Error_alias of string * int * string
  | `Struct of string * Parser.struct_fields
  | `Union of string * Parser.static_field list
  | `Request of string * int * Parser.request ]


type extension = declaration Types.extension


let sep_imports (decls : Parser.declaration list) : string list * declaration list =
  let imports, decls =
    ListLabels.fold_left decls ~init:([], []) ~f:(fun (imports, decls) ->
      function
        | `Import id ->
          (id :: imports), decls

        | #declaration as d ->
          imports, (d :: decls)
    )
  in
  List.rev imports, List.rev decls


let lift_imports : Parser.protocol_file -> extension = function
  | Parser.Core decls ->
    let imports, declarations = sep_imports decls in
    { name = "Xproto"
    ; file_name = "xproto"
    ; query_name = None
    ; version = None
    ; imports; declarations }

  | Parser.Extension ({ name; query_name; version; file_name; _ }, decls) ->
    let imports, declarations = sep_imports decls in
    { name; file_name
    ; query_name = Some query_name
    ; version = Some version
    ; imports; declarations }

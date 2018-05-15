let rec list_extract f = function
  | [] -> None, []
  | x :: rest ->
    match f x with
    | Some x ->
      Some x, rest
    | None ->
      (* Not tail-recursive, but eh *)
      let found, ls = list_extract f rest in
      found, x :: ls


(* The "_o" suffix represents an attribute that may not be present  *)

let str_attr attrs x =
  List.assoc x attrs

let str_attr_o attrs x =
  List.assoc_opt x attrs

let int_attr attrs x =
  str_attr attrs x |> int_of_string

let int_attr_o attrs x =
  str_attr_o attrs x
  |> Option.map int_of_string

(* Attributes that are false by default *)
let bool_attr_f attrs x =
  str_attr_o attrs x
  |> Option.map bool_of_string |> Option.with_default false

(* Attributes that are true by default *)
let bool_attr_t attrs x =
  str_attr_o attrs x
  |> Option.map bool_of_string |> Option.with_default true


type xml_el = string * (string * string) list * Xml.xml list


exception Unexpected of string

let fail_unexpected str =
  raise (Unexpected str)

let fail_unexpectedf fmt =
  Printf.ksprintf fail_unexpected fmt


(* ********** Padding and alignment ********** *)
type pad =
  [ `Bytes of int
  | `Align of int ]

type padding =
  { pad : pad
  ; serialize : bool }


let pad_of_xml attrs : padding =
  let serialize = bool_attr_f attrs "serialize" in
  let pad =
    try       `Bytes (int_attr attrs "bytes")
    with _ -> `Align (int_attr attrs "align") in
  { pad; serialize }


type required_start_align =
  { align : int
  ; offset : int option }


let required_start_align_of_xml attrs =
  let align = int_attr attrs "align" in
  let offset = int_attr_o attrs "offset" in
  { align; offset }


let consume_align = function
  | Xml.Element ("required_start_align", align, []) :: rest ->
    let align = required_start_align_of_xml align in
    Some align, rest
  | children ->
    None, children



(* ********** Documentation ********** *)
type doc = unit

let consume_doc =
  list_extract (function
    | Xml.Element ("doc", _, _) -> Some ()
    | _ -> None
  )



(* ********** XID union ********** *)
let xid_union_of_xml = function
  | Xml.Element ("type", [], [Xml.PCData name]) -> name
  | _ -> fail_unexpected "invalid element in xidunion"



(* ********** Enums ********** *)
type enum_vals = (string * int64) list
type enum_bits = (string * int) list


type enum =
  { vals : enum_vals
  ; bits : enum_bits }


let enum_of_xml items =
  let open Xml in
  let rec parse_items (vals, bits) = function
    | [] ->
      vals, bits, None
    | Element ("item", ["name", name], [Element ("value", [], [PCData v])]) :: rest ->
      let vals = (name, Int64.of_string v) :: vals in
      parse_items (vals, bits) rest
    | Element ("item", ["name", name], [Element ("bit",   [], [PCData v])]) :: rest ->
      let bits = (name, int_of_string v) :: bits in
      parse_items (vals, bits) rest
    | Element ("doc", [], doc) :: [] ->
      vals, bits, Some ()
    | Element ("doc", _, _) :: _ ->
      fail_unexpected "invalid enum: doc item not in tail position"
    | _ ->
      fail_unexpected "invalid enum element"
  in
  let vals, bits, doc = parse_items ([], []) items in
  let vals, bits = List.rev vals, List.rev bits in
  { vals; bits }



(* ********** Expressions ********** *)
type binop =
  [ `Add | `Sub | `Mul | `Div
  | `Bit_and | `Bit_left_shift ]

let binop : string -> binop = function
  | "+" -> `Add
  | "-" -> `Sub
  | "*" -> `Mul
  | "/" -> `Div
  | "&amp;" -> `Bit_and
  | "&lt;&lt;" -> `Bit_left_shift
  | o -> fail_unexpectedf "invalid binary operation: %s" o


type unop =
  [ `Bit_not ]

let unop : string -> unop = function
  | "~" -> `Bit_not
  | o -> fail_unexpectedf "invalid unary operation: %s" o


type expression =
  [ `Binop of binop * expression * expression
  | `Unop of unop * expression
  | `Field_ref of string
  | `Param_ref of string * string
  | `Enum_ref  of string * string
  | `Sum_of of string * expression option
  | `Current_ref
  | `Pop_count of expression
  | `Value of int
  | `Bit of int ]


let rec expression_of_xml : xml_el -> expression =
  let open Xml in function
  | "op", ["op", op], [Element op1; Element op2] ->
    `Binop (binop op, expression_of_xml op1, expression_of_xml op2)

  | "unop", ["op", op], [Element op1] ->
    `Unop (unop op, expression_of_xml op1)

  | "fieldref", [], [PCData ref] ->
    `Field_ref ref

  | "paramref", ["type", typ], [PCData ref] ->
    `Param_ref (ref, typ)

  | "enumref", ["ref", enum], [PCData id] ->
    `Enum_ref (enum, id)

  | "popcount", [], [Element expr] ->
    `Pop_count (expression_of_xml expr)

  | "sumof", ["ref", ref], [] ->
    `Sum_of (ref, None)

  | "sumof", ["ref", ref], [Element expr] ->
    `Sum_of (ref, Some (expression_of_xml expr))

  | "listelement-ref", [], [] ->
    `Current_ref

  | "value", [], [PCData v] ->
    `Value (int_of_string v)

  | "bit", [], [PCData v] ->
    `Bit (int_of_string v)

  | n, _, _ ->
    fail_unexpectedf "invalid expression: %s" n



(* ********** Struct fields ********** *)
type allowed_vals =
  [ `Enum of string
  | `Mask of string
  | `Alt_enum of string
  | `Alt_mask of string ]

type field_type =
  { typ : string
  ; allowed : allowed_vals option }


let field_of_xml attrs : string * field_type =
  let name = str_attr attrs "name" in
  let typ  = str_attr attrs "type" in
  let allowed =
    match str_attr_o attrs "enum" with
    | Some x -> Some (`Enum x) | None ->
    match str_attr_o attrs "mask" with
    | Some x -> Some (`Mask x) | None ->
    match str_attr_o attrs "altenum" with
    | Some x -> Some (`Alt_enum x) | None ->
    match str_attr_o attrs "altmask" with
    | Some x -> Some (`Alt_mask x) | None -> None
  in
  name, { typ; allowed }


type static_field =
  [ `File_descriptor of string
  | `Pad of padding
  | `Field of string * field_type
  | `List of string * field_type * expression ]


let static_field_of_xml_el : xml_el -> static_field = function
  | "fd", ["name", name], [] ->
    `File_descriptor name

  | "pad", attrs, [] ->
    `Pad (pad_of_xml attrs)

  | "field", attrs, [] ->
    let name, typ = field_of_xml attrs in
    `Field (name, typ)

  | "list", attrs, [Xml.Element expr] ->
    let name, typ = field_of_xml attrs in
    `List (name, typ, expression_of_xml expr)

  | x, _, _ ->
    fail_unexpectedf "invalid static field: %s" x


let static_field_of_xml = function
  | Xml.PCData _ -> fail_unexpected "PCData in static field"
  | Xml.Element el -> static_field_of_xml_el el


type dynamic_field =
  [ static_field
  | `List_var of string * field_type ]


let dynamic_field_of_xml_el : xml_el -> dynamic_field = function
  | "list", attrs, [] ->
    let name, typ = field_of_xml attrs in
    `List_var (name, typ)

  | other ->
    (static_field_of_xml_el other :> dynamic_field)

let dynamic_field_of_xml : Xml.xml -> dynamic_field = function
  | Xml.PCData _ -> fail_unexpected "PCData in dynamic field"
  | Xml.Element el -> dynamic_field_of_xml_el el


type request_field =
  [ dynamic_field
  | `Expr of string * field_type * expression ]


let request_field_of_xml_el : xml_el -> request_field = function
  | "exprfield", attrs, [Xml.Element expr] ->
    let name, typ = field_of_xml attrs in
    `Expr (name, typ, expression_of_xml expr)

  | other ->
    (dynamic_field_of_xml_el other :> request_field)


let request_field_of_xml = function
  | Xml.PCData _ -> fail_unexpected "invalid element in request field"
  | Xml.Element el -> request_field_of_xml_el el



(* ********** Switch ********** *)
type cond =
  [ `Bit_and of expression
  | `Eq of expression ]

type switch =
  { align : required_start_align option
  ; cond : cond
  ; cases : case list }

and case =
  { exprs : expression list
  ; name : string option
  ; align_c : required_start_align option
  ; fields : static_field list
  ; switch : (string * switch) option }


let rec switch_of_xml fields =
  let parse_case name = function
    | Xml.Element (n, attrs, fields) when n = name -> case_of_xml attrs fields
    | _ -> fail_unexpectedf "invalid switch: expected %s field" name in
  (* Take the first element, which should be the expression, and consume
   * the start align if it exists. *)
  let expr, align, fields = match fields with
    | Xml.Element expr :: rest ->
      let align, rest = consume_align rest in
      expr, align, rest
    | _ ->
      fail_unexpected "invalid switch"
  in
  let expr = expression_of_xml expr in
  let cond, cases =
    (* Make sure that all elements are cases or bitcases. *)
    match fields with
    | Xml.Element ("case", _, _) :: _ ->
      `Eq expr,      List.map (parse_case "case") fields
    | Xml.Element ("bitcase", _, _) :: _ ->
      `Bit_and expr, List.map (parse_case "bitcase") fields
    | _ ->
      fail_unexpected "invalid switch: expected case or bitcase field"
  in
  { align; cond; cases }

and case_of_xml name fields =
  (* Parse expressions until we fail or hit a start align.
   * Clearly the designers of this format were above subtleties such as
   * "structured data". *)
  let rec parse_exprs exprs = function
    | Xml.Element ("required_start_align", a, []) :: rest ->
      let align = required_start_align_of_xml a in
      List.rev exprs, Some align, rest

    | Xml.Element expr :: rest ->
      begin try
        let expr = expression_of_xml expr in
        parse_exprs (expr :: exprs) rest
      with Unexpected _ ->
        List.rev exprs, None, ((Xml.Element expr) :: rest)
      end

    | _ ->
      fail_unexpected "invalid case field"
  in
  let rec parse_fields fields = function
    | Xml.Element ("switch", ["name", name], switch) :: [] ->
      let switch = switch_of_xml switch in
      fields, Some (name, switch)

    | Xml.Element ("switch", _, _) :: _ ->
      fail_unexpected "invalid case: switch not in tail position"

    | Xml.Element field :: rest ->
      let field = static_field_of_xml_el field in
      parse_fields (field :: fields) rest

    | [] ->
      fields, None

    | _ ->
      fail_unexpected "invalid case field: expected element, got pcdata"
  in
  let name = match name with ["name", name] -> Some name | _ -> None in
  let exprs, align_c, fields = parse_exprs [] fields in
  let fields, switch = parse_fields [] fields in
  { name; exprs; align_c; fields; switch }


let consume_switch =
  list_extract (function
    | Xml.Element ("switch", ["name", name], fields) ->
      let switch = switch_of_xml fields in
      Some (name, switch)
    | _ ->
      None
  )



(* ********** Structs ********** *)
type event =
  { no_sequence_number : bool
  ; align : required_start_align option
  ; fields : static_field list }

type generic_event =
  { no_sequence_number : bool
  ; align : required_start_align option
  ; fields : dynamic_field list }


type allowed_events =
  { extension : string
  ; opcode_range : int * int }

let allowed_events_of_xml : Xml.xml -> allowed_events = function
  | Xml.Element ("allowed", attrs, []) ->
    let extension = str_attr attrs "extension" in
    let opcode_min = int_attr attrs "opcode-min" in
    let opcode_max = int_attr attrs "opcode-max" in
    { extension; opcode_range = (opcode_min, opcode_max) }
  | _ ->
    fail_unexpected "invalid element in event struct"


type error =
  { align : required_start_align option
  ; fields : static_field list }


type struct_fields =
  { fields : static_field list
  ; switch : (string * switch) option }


type request_fields =
  { align : required_start_align option
  ; fields : request_field list
  ; switch : (string * switch) option }

type reply =
  { align : required_start_align option
  ; fields : dynamic_field list
  ; switch : (string * switch) option }


type request =
  { combine_adjacent : bool
  ; params : request_fields
  ; reply : reply option }


let reply_of_xml fields : reply =
  let align, fields  = consume_align fields in
  let doc, fields    = consume_doc fields in
  let switch, fields = consume_switch fields in
  let fields = List.map dynamic_field_of_xml fields in
  { align; fields; switch }


let request_of_xml fields : request_fields * reply option =
  let align, fields  = consume_align fields in
  let doc, fields    = consume_doc fields in
  let switch, fields = consume_switch fields in
  let rec parse_params acc = function
    | [] ->
      acc, None
    | Xml.Element ("reply", [], fields) :: [] ->
      let reply = reply_of_xml fields in
      acc, Some reply
    | Xml.Element f :: rest ->
      let f = request_field_of_xml_el f in
      parse_params (f :: acc) rest
    | _ ->
      fail_unexpected "invalid element in request"
  in
  let fields, reply = parse_params [] fields in
  { align; fields; switch }, reply



(* ********** Declarations ********** *)
type declaration =
  [ `Import of string
  | `X_id of string
  | `X_id_union of string * string list
  | `Enum of string * enum
  | `Type_alias of string * string

  | `Event of string * int * event
  | `Generic_event of string * int * generic_event
  | `Event_struct of string * allowed_events list
  | `Event_alias of string * int * string

  | `Error of string * int * error
  | `Error_alias of string * int * string

  | `Struct of string * struct_fields
  | `Union of string * static_field list
  | `Request of string * int * request ]



let declaration_of_xml : Xml.xml -> declaration =
  let failure = Unexpected "invalid declaration" in
  let open Xml in
  function PCData _ -> raise failure | Element x' ->
  match x' with
  | "import", [], [PCData file] ->
    `Import file

  | "xidtype",  ["name", id], [] ->
    `X_id id

  | "xidunion", ["name", id], children ->
    let types = List.map xid_union_of_xml children in
    `X_id_union (id, types)

  | "enum", ["name", name], children ->
    let items = enum_of_xml children in
    `Enum (name, items)

  | "typedef", ["oldname", old_name; "newname", new_name], []
  | "typedef", ["newname", new_name; "oldname", old_name], [] ->
    `Type_alias (new_name, old_name)

  | "event", attrs, fields ->
    let is_generic = bool_attr_f attrs "xge" in
    let name = str_attr attrs "name" in
    let code = int_attr attrs "number" in
    let no_sequence_number = bool_attr_f attrs "no-sequence-number" in
    let align, fields = consume_align fields in
    let doc, fields = consume_doc fields in
    if is_generic then
      let fields = List.map dynamic_field_of_xml fields in
      `Generic_event (name, code, { no_sequence_number; align; fields })
    else
      let fields = List.map static_field_of_xml fields in
      `Event (name, code, { no_sequence_number; align; fields })

  | "eventstruct", ["name", name], allowed ->
    let allowed = List.map allowed_events_of_xml allowed in
    `Event_struct (name, allowed)

  | "eventcopy", attrs, [] ->
    let new_name = str_attr attrs "name" in
    let old_name = str_attr attrs "ref" in
    let number   = int_attr attrs "number" in
    `Event_alias (new_name, number, old_name)

  | "error", attrs, fields ->
    let name = str_attr attrs "name" in
    let code = int_attr attrs "number" in
    let align, fields = consume_align fields in
    let fields = List.map static_field_of_xml fields in
    `Error (name, code, { align; fields })

  | "errorcopy", attrs, [] ->
    let new_name = str_attr attrs "name" in
    let old_name = str_attr attrs "ref" in
    let number   = int_attr attrs "number" in
    `Error_alias (new_name, number, old_name)

  | "union", ["name", name], fields ->
    let fields = List.map static_field_of_xml fields in
    `Union (name, fields)

  | "struct", ["name", name], fields ->
    let switch, fields = consume_switch fields in
    let fields = List.map static_field_of_xml fields in
    `Struct (name, { fields; switch })

  | "request", attrs, fields ->
    let name = str_attr attrs "name" in
    let opcode = int_attr attrs "opcode" in
    let combine_adjacent = bool_attr_t attrs "combine-adjacent" in
    let params, reply = request_of_xml fields in
    `Request (name, opcode, { combine_adjacent; params; reply })

  | n, _, _ ->
    fail_unexpectedf "invalid declaration: %s" n



type extension_info =
  { name : string
  ; file_name : string
  ; query_name : string
  ; multiword : bool
  ; version : int * int }


type protocol_file =
  | Core of declaration list
  | Extension of extension_info * declaration list



let parse_file fname : protocol_file =
  let xml = Xml.parse_file fname in
  match xml with
  | Xml.Element ("xcb", ["header", "xproto"], decls) ->
    Core (List.map declaration_of_xml decls)

  | Xml.Element ("xcb", attrs, decls) ->
    let name       = str_attr attrs "extension-name" in
    let file_name  = str_attr attrs "header" in
    let query_name = str_attr attrs "extension-xname" in
    let multiword  = bool_attr_f attrs "extension_multiword" in
    let version =
      let major = int_attr attrs "major-version" in
      let minor = int_attr attrs "minor-version" in
      major, minor in
    let decls = List.map declaration_of_xml decls in
    let info = { name; file_name; query_name; multiword; version } in
    Extension (info, decls)

  | _ ->
    fail_unexpected "invalid XCB root element"



let%test_unit "all files are parsed without issues" =
  let files =
    [ "bigreq"; "composite"; "damage"; "dpms"; "dri2"; "dri3"; "ge"; "glx"
    ; "present"; "randr"; "record"; "render"; "res"; "screensaver"; "shape"
    ; "shm"; "sync"; "xc_misc"; "xevie"; "xf86dri"; "xf86vidmode"; "xfixes"
    ; "xinerama"; "xinput"; "xkb"; "xprint"; "xproto"; "xselinux"; "xtest"
    ; "xvmc"; "xv" ]
  in
  files |> List.iter (fun file ->
    let fname = Filename.concat "xproto/src" (file ^ ".xml") in
    let _ = parse_file fname in
    ()
  )

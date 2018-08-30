open Util


module Attr = struct
  (* The "_o" suffix represents an attribute that may not be present  *)

  let str attrs x =
    List.assoc x attrs

  let str_o attrs x =
    List.assoc_opt x attrs

  let int attrs x =
    str attrs x |> int_of_string

  let int_o attrs x =
    str_o attrs x
    |> Option.map int_of_string

  let bool_o attrs x =
    str_o attrs x
    |> Option.map bool_of_string

  (* Attributes that are false by default *)
  let bool_f attrs x =
    bool_o attrs x
    |> Option.with_default false

  (* Attributes that are true by default *)
  let bool_t attrs x =
    bool_o attrs x
    |> Option.with_default true
end


module Xml = struct
  type attr = string * string

  type el =
    string * attr list * t list

  and t =
    | E of el
    | D of string

  let parse_file fname =
    let inp = open_in fname in
    try
      let xml_inp = Xmlm.make_input ~strip:true (`Channel inp) in
      let el ((_, name), attrs) childs =
        E (name, (List.map (fun ((_, n), v) -> (n, v)) attrs), childs)
      in
      let data str = D str in
      let (_, x) = Xmlm.input_doc_tree ~el ~data xml_inp in
      close_in inp;
      x
    with exn ->
      close_in inp;
      raise exn
end


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
  { pd_pad : pad
  ; pd_serialize : bool }


let pad_of_xml attrs : padding =
  let pd_serialize = Attr.bool_f attrs "serialize" in
  let pd_pad =
    try       `Bytes (Attr.int attrs "bytes")
    with _ -> `Align (Attr.int attrs "align")
  in
  { pd_pad; pd_serialize }


type required_start_align =
  { al_align : int
  ; al_offset : int option }


let required_start_align_of_xml attrs =
  let al_align  = Attr.int attrs "align" in
  let al_offset = Attr.int_o attrs "offset" in
  { al_align; al_offset }


let consume_align = function
  | Xml.E ("required_start_align", align, []) :: rest ->
    let align = required_start_align_of_xml align in
    Some align, rest
  | children ->
    None, children



(* ********** Documentation ********** *)
type doc = unit

let consume_doc : Xml.t list -> doc option * Xml.t list =
  List'.extract (function
    | Xml.E ("doc", _, _) -> Some ()
    | _ -> None
  )



(* ********** XID union ********** *)
let xid_union_of_xml = function
  | Xml.E ("type", [], [Xml.D name]) -> name
  | _ -> fail_unexpected "invalid element in xidunion"



(* ********** Enums ********** *)
type enum_vals = (string * int64) list
type enum_bits = (string * int) list


type enum =
  { en_vals : enum_vals
  ; en_bits : enum_bits }


let enum_of_xml items =
  let open Xml in
  let rec parse_items (vals, bits) = function
    | [] ->
      vals, bits, None

    | E ("item", ["name", name], [E ("value", [], [D v])]) :: rest ->
      let vals = (name, Int64.of_string v) :: vals in
      parse_items (vals, bits) rest

    | E ("item", ["name", name], [E ("bit",   [], [D v])]) :: rest ->
      let bits = (name, int_of_string v) :: bits in
      parse_items (vals, bits) rest

    | E ("doc", [], _doc) :: [] ->
      vals, bits, Some ()

    | E ("doc", _, _) :: _ ->
      fail_unexpected "invalid enum: doc item not in tail position"

    | _ ->
      fail_unexpected "invalid enum element"
  in
  let vals, bits, _doc = parse_items ([], []) items in
  let en_vals, en_bits = List.rev vals, List.rev bits in
  { en_vals; en_bits }



(* ********** Expressions ********** *)
type binop =
  [ `Add | `Sub | `Mul | `Div
  | `Bit_and | `Bit_left_shift ]

let binop : string -> binop = function
  | "+" -> `Add
  | "-" -> `Sub
  | "*" -> `Mul
  | "/" -> `Div
  | "&" -> `Bit_and
  | "<<" -> `Bit_left_shift
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


let rec expression_of_xml : Xml.el -> expression =
  let open Xml in function
  | "op", ["op", op], [E op1; E op2] ->
    `Binop (binop op, expression_of_xml op1, expression_of_xml op2)

  | "unop", ["op", op], [E op1] ->
    `Unop (unop op, expression_of_xml op1)

  | "fieldref", [], [D ref] ->
    `Field_ref ref

  | "paramref", ["type", typ], [D ref] ->
    `Param_ref (ref, typ)

  | "enumref", ["ref", enum], [D id] ->
    `Enum_ref (enum, id)

  | "popcount", [], [E expr] ->
    `Pop_count (expression_of_xml expr)

  | "sumof", ["ref", ref], [] ->
    `Sum_of (ref, None)

  | "sumof", ["ref", ref], [E expr] ->
    `Sum_of (ref, Some (expression_of_xml expr))

  | "listelement-ref", [], [] ->
    `Current_ref

  | "value", [], [D v] ->
    `Value (int_of_string v)

  | "bit", [], [D v] ->
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
  { ft_type : string
  ; ft_allowed : allowed_vals option }


let field_of_attrs attrs : string * field_type =
  let name    = Attr.str attrs "name" in
  let ft_type = Attr.str attrs "type" in
  let ft_allowed =
    match Attr.str_o attrs "enum"
    with Some x -> Some (`Enum x) | None ->
    match Attr.str_o attrs "mask"
    with Some x -> Some (`Mask x) | None ->
    match Attr.str_o attrs "altenum"
    with Some x -> Some (`Alt_enum x) | None ->
    match Attr.str_o attrs "altmask"
    with Some x -> Some (`Alt_mask x) | None -> None
  in
  name, { ft_type; ft_allowed }


type static_field =
  [ `File_descriptor of string
  | `Pad of padding
  | `Field of string * field_type
  | `List of string * field_type * expression ]


let static_field_of_xml_el : Xml.el -> static_field = function
  | "fd", ["name", name], [] ->
    `File_descriptor name

  | "pad", attrs, [] ->
    `Pad (pad_of_xml attrs)

  | "field", attrs, [] ->
    let name, typ = field_of_attrs attrs in
    `Field (name, typ)

  | "list", attrs, [Xml.E expr] ->
    let name, typ = field_of_attrs attrs in
    `List (name, typ, expression_of_xml expr)

  | x, _, _ ->
    fail_unexpectedf "invalid static field: %s" x


let static_field_of_xml = function
  | Xml.D _ -> fail_unexpected "D in static field"
  | Xml.E el -> static_field_of_xml_el el


type dynamic_field =
  [ static_field
  | `List_var of string * field_type ]


let dynamic_field_of_xml_el : Xml.el -> dynamic_field = function
  | "list", attrs, [] ->
    let name, typ = field_of_attrs attrs in
    `List_var (name, typ)

  | other ->
    (static_field_of_xml_el other :> dynamic_field)

let dynamic_field_of_xml : Xml.t -> dynamic_field = function
  | Xml.D _ -> fail_unexpected "D in dynamic field"
  | Xml.E el -> dynamic_field_of_xml_el el


type request_field =
  [ dynamic_field
  | `Expr of string * field_type * expression ]


let request_field_of_xml_el : Xml.el -> request_field = function
  | "exprfield", attrs, [Xml.E expr] ->
    let name, typ = field_of_attrs attrs in
    `Expr (name, typ, expression_of_xml expr)

  | other ->
    (dynamic_field_of_xml_el other :> request_field)


    (*
let request_field_of_xml = function
  | Xml.D _ -> fail_unexpected "invalid element in request field"
  | Xml.E el -> request_field_of_xml_el el
  *)



(* ********** Switch ********** *)
type cond =
  [ `Bit_and of expression
  | `Eq of expression ]

type switch =
  { sw_align : required_start_align option
  ; sw_cond : cond
  ; sw_cases : case list }

and case =
  { cs_exprs : expression list
  ; cs_name : string option
  ; cs_align : required_start_align option
  ; cs_fields : static_field list
  ; cs_switch : (string * switch) option }


let rec switch_of_xml fields =
  let parse_case name = function
    | Xml.E (n, attrs, fields) when n = name -> case_of_xml attrs fields
    | _ -> fail_unexpectedf "invalid switch: expected %s field" name in
  (* Take the first element, which should be the expression, and consume
   * the start align if it exists. *)
  let expr, sw_align, fields = match fields with
    | Xml.E expr :: rest ->
      let align, rest = consume_align rest in
      expr, align, rest
    | _ ->
      fail_unexpected "invalid switch"
  in
  let expr = expression_of_xml expr in
  let sw_cond, sw_cases =
    (* Make sure that all elements are cases or bitcases. *)
    match fields with
    | Xml.E ("case", _, _) :: _ ->
      `Eq expr,      List.map (parse_case "case") fields
    | Xml.E ("bitcase", _, _) :: _ ->
      `Bit_and expr, List.map (parse_case "bitcase") fields
    | _ ->
      fail_unexpected "invalid switch: expected case or bitcase field"
  in
  { sw_align; sw_cond; sw_cases }

and case_of_xml name fields =
  (* Parse expressions until we fail or hit a start align.
   * Clearly the designers of this format were above subtleties such as
   * "structured data". *)
  let rec parse_exprs exprs = function
    | Xml.E ("required_start_align", a, []) :: rest ->
      let align = required_start_align_of_xml a in
      List.rev exprs, Some align, rest

    | Xml.E expr :: rest ->
      begin try
        let expr = expression_of_xml expr in
        parse_exprs (expr :: exprs) rest
      with Unexpected _ ->
        List.rev exprs, None, ((Xml.E expr) :: rest)
      end

    | _ ->
      fail_unexpected "invalid case field"
  in
  let rec parse_fields fields = function
    | Xml.E ("switch", ["name", name], switch) :: [] ->
      let switch = switch_of_xml switch in
      fields, Some (name, switch)

    | Xml.E ("switch", _, _) :: _ ->
      fail_unexpected "invalid case: switch not in tail position"

    | Xml.E field :: rest ->
      let field = static_field_of_xml_el field in
      parse_fields (field :: fields) rest

    | [] ->
      fields, None

    | _ ->
      fail_unexpected "invalid case field: expected element, got pcdata"
  in
  let cs_name = match name with ["name", name] -> Some name | _ -> None in
  let cs_exprs, cs_align, fields = parse_exprs [] fields in
  let cs_fields, cs_switch = parse_fields [] fields in
  { cs_name; cs_exprs; cs_align; cs_fields; cs_switch }


let consume_switch =
  List'.extract (function
    | Xml.E ("switch", ["name", name], fields) ->
      let switch = switch_of_xml fields in
      Some (name, switch)
    | _ ->
      None
  )



(* ********** Structs ********** *)
type event =
  { ev_no_sequence_number : bool
  ; ev_align : required_start_align option
  ; ev_fields : static_field list }

type generic_event =
  { gev_no_sequence_number : bool
  ; gev_align : required_start_align option
  ; gev_fields : dynamic_field list }


type allowed_events =
  { aev_extension : string
  ; aev_opcode_range : int * int }

let allowed_events_of_xml : Xml.t -> allowed_events = function
  | Xml.E ("allowed", attrs, []) ->
    let aev_extension  = Attr.str attrs "extension" in
    let opcode_min = Attr.int attrs "opcode-min" in
    let opcode_max = Attr.int attrs "opcode-max" in
    { aev_extension; aev_opcode_range = (opcode_min, opcode_max) }
  | _ ->
    fail_unexpected "invalid element in event struct"


type error =
  { er_align : required_start_align option
  ; er_fields : static_field list }


type struct_fields =
  { sf_fields : static_field list
  ; sf_switch : (string * switch) option }


type request_fields =
  { rf_align : required_start_align option
  ; rf_fields : request_field list
  ; rf_switch : (string * switch) option }

type reply =
  { re_align : required_start_align option
  ; re_fields : dynamic_field list
  ; re_switch : (string * switch) option }


type request =
  { rq_combine_adjacent : bool
  ; rq_params : request_fields
  ; rq_reply : reply option }


let reply_of_xml fields : reply =
  let re_align, fields  = consume_align fields in
  let _doc, fields   = consume_doc fields in
  let re_switch, fields = consume_switch fields in
  let re_fields = List.map dynamic_field_of_xml fields in
  { re_align; re_fields; re_switch }


let request_of_xml fields : request_fields * reply option =
  let rf_align, fields  = consume_align fields in
  let _doc, fields      = consume_doc fields in
  let rf_switch, fields = consume_switch fields in
  let rec parse_params acc = function
    | [] ->
      acc, None
    | Xml.E ("reply", [], fields) :: [] ->
      let reply = reply_of_xml fields in
      acc, Some reply
    | Xml.E f :: rest ->
      let f = request_field_of_xml_el f in
      parse_params (f :: acc) rest
    | _ ->
      fail_unexpected "invalid element in request"
  in
  let rf_fields, reply = parse_params [] fields in
  { rf_align; rf_fields; rf_switch }, reply



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



let declaration_of_xml : Xml.t -> declaration =
  let failure = Unexpected "invalid declaration" in
  let open Xml in
  function D _ -> raise failure | E x' ->
  match x' with
  | "import", [], [D file] ->
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
    let is_generic = Attr.bool_f attrs "xge" in
    let name = Attr.str attrs "name" in
    let code = Attr.int attrs "number" in
    let no_sequence_number = Attr.bool_f attrs "no-sequence-number" in
    let align, fields = consume_align fields in
    let _doc, fields = consume_doc fields in
    if is_generic then
      let gev_fields = List.map dynamic_field_of_xml fields in
      let gev =
        { gev_no_sequence_number = no_sequence_number
        ; gev_align = align
        ; gev_fields }
      in
      `Generic_event (name, code, gev)
    else
      let ev_fields = List.map static_field_of_xml fields in
      let ev =
        { ev_no_sequence_number = no_sequence_number
        ; ev_align = align
        ; ev_fields }
      in
      `Event (name, code, ev)

  | "eventstruct", ["name", name], allowed ->
    let allowed = List.map allowed_events_of_xml allowed in
    `Event_struct (name, allowed)

  | "eventcopy", attrs, [] ->
    let new_name = Attr.str attrs "name" in
    let old_name = Attr.str attrs "ref" in
    let number   = Attr.int attrs "number" in
    `Event_alias (new_name, number, old_name)

  | "error", attrs, fields ->
    let name = Attr.str attrs "name" in
    let code = Attr.int attrs "number" in
    let er_align, fields = consume_align fields in
    let er_fields = List.map static_field_of_xml fields in
    `Error (name, code, { er_align; er_fields })

  | "errorcopy", attrs, [] ->
    let new_name = Attr.str attrs "name" in
    let old_name = Attr.str attrs "ref" in
    let number   = Attr.int attrs "number" in
    `Error_alias (new_name, number, old_name)

  | "union", ["name", name], fields ->
    let fields = List.map static_field_of_xml fields in
    `Union (name, fields)

  | "struct", ["name", name], fields ->
    let sf_switch, fields = consume_switch fields in
    let sf_fields = List.map static_field_of_xml fields in
    `Struct (name, { sf_fields; sf_switch })

  | "request", attrs, fields ->
    let name = Attr.str attrs "name" in
    let opcode = Attr.int attrs "opcode" in
    let rq_combine_adjacent = Attr.bool_t attrs "combine-adjacent" in
    let rq_params, rq_reply = request_of_xml fields in
    `Request (name, opcode, { rq_combine_adjacent; rq_params; rq_reply })

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
  | Xml.E ("xcb", ["header", "xproto"], decls) ->
    Core (List.map declaration_of_xml decls)

  | Xml.E ("xcb", attrs, decls) ->
    let name       = Attr.str attrs "extension-name" in
    let file_name  = Attr.str attrs "header" in
    let query_name = Attr.str attrs "extension-xname" in
    let multiword  = Attr.bool_f attrs "extension_multiword" in
    let version =
      let major = Attr.int attrs "major-version" in
      let minor = Attr.int attrs "minor-version" in
      major, minor in
    let decls = List.map declaration_of_xml decls in
    let info = { name; file_name; query_name; multiword; version } in
    Extension (info, decls)

  | _ ->
    fail_unexpected "invalid XCB root element"

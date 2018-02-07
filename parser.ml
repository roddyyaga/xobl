type doc = string
  [@@deriving show]


(* * Utility functions *)
let app_with_default f x = function
  | Some a -> f a | None -> x

let option_app f = function
  | Some a -> Some (f a) | None -> None


let one_attr_opt name = function
  | (n, v) :: [] -> if n = name then Some v else None
  | _ -> None


let one_attr name = function
  | (n, v) :: [] ->
    if n = name then
      v
    else
      failwith ("Expected " ^ name ^ ", got: " ^ n)
  | _ :: _ ->
    failwith "Expected one attribute, got > 1"
  | [] ->
    failwith "Expected one attribute, got none"



(* Padding bytes in a packet  *)
type pad_type =
  [ `Bytes | `Align ]
[@@deriving show]

type padding =
  { typ : pad_type
  ; amount : int
  ; serialize : bool }
[@@deriving show]

let pad_of_xml attrs =
  let serialize = app_with_default bool_of_string false
    @@ List.assoc_opt "serialize" attrs in
  let typ, amount =
    try       `Bytes, List.assoc "bytes" attrs
    with _ -> `Align, List.assoc "align" attrs in
  { typ; amount = int_of_string amount; serialize }


(*  *)
type required_start_align =
  { align  : int
  ; offset : int option }
[@@deriving show]

let required_start_align_of_xml attrs =
  let align = int_of_string @@ List.assoc "align" attrs in
  let offset = option_app int_of_string @@ List.assoc_opt "offset" attrs in
  { align; offset }


type op =
  [ `Add | `Sub | `Mul | `Div | `Bit_and | `Bit_left_shift ]
[@@deriving show]

let op_of_xml attrs =
  match List.assoc "op" attrs with
  | "+" -> `Add
  | "-" -> `Sub
  | "*" -> `Mul
  | "/" -> `Div
  | "&amp;" -> `Bit_and
  | "&lt;&lt;" -> `Bit_left_shift
  | x -> failwith (Printf.sprintf "not an operation: %s" x)


type unop =
  [ `Bit_not ]
[@@deriving show]

let unop_of_xml attrs =
  match List.assoc "op" attrs with
  | "~" -> `Bit_not
  | _ -> failwith "not an unary operation"


type expression =
  [ `Op of op * (expression * expression)
  | `Unop of unop * expression
  | `Field_ref of string
  | `Param_ref of string * string
  | `Enum_ref of string * string
  | `Population_count of expression
  | `Sum_of of string * expression option
  | `Current_ref
  | `Value of int
  | `Bit of int ]
[@@deriving show]

let rec expression_of_xml =
  let open Xml in function
  | "op", attrs, Element op1 :: Element op2 :: [] ->
      `Op (op_of_xml attrs,
        (expression_of_xml op1, expression_of_xml op2))
  | "unop", attrs, [Element op1] ->
      `Unop (unop_of_xml attrs, expression_of_xml op1)
  | "fieldref", [], [PCData ref] ->
      `Field_ref ref
  | "paramref", ["type", typ], [PCData ref] ->
      `Param_ref (typ, ref)
  | "enumref", ["ref", typ], [PCData ref] ->
      `Enum_ref (typ, ref)
  | "popcount", [], [Element expr] ->
      `Population_count (expression_of_xml expr)
  | "sumof", ["ref", ref], [Element expr] ->
      `Sum_of (ref, Some (expression_of_xml expr))
  | "sumof", ["ref", ref], [] ->
      `Sum_of (ref, None)
  | "listelement-ref", [], [] ->
      `Current_ref
  | "value", [], [PCData v] ->
      `Value (int_of_string v)
  | "bit", [], [PCData v] ->
      `Bit (int_of_string v)
  | n, _, _ ->
      failwith ("not an expression: " ^ n)


(*  *)
type allowed_val =
  [ `Enum | `Alt_enum
  | `Mask | `Alt_mask ]
[@@deriving show]

let allowed_val_type_of_string = function
  | "enum" -> `Enum
  | "mask" -> `Mask
  | "altenum" -> `Alt_enum
  | "altmask" -> `Alt_mask
  | x -> failwith (Printf.sprintf "not a var_attr: %s" x)


type 'a field_t =
  { name    : string
  ; typ     : string
  ; allowed : (allowed_val * string) option
  ; value   : 'a }
  [@@deriving show]

let field_t_of_xml value attrs =
  let name, typ, allowed =
    let rec loop =
      function
      | (None, t, a), ("name", name) :: rst ->
          loop ((Some name, t, a), rst)
      | (n, None, a), ("type", typ) :: rst ->
          loop ((n, Some typ, a), rst)
      | (n, t, None), ("enum"    as name, enum) :: rst
      | (n, t, None), ("mask"    as name, enum) :: rst
      | (n, t, None), ("altenum" as name, enum) :: rst
      | (n, t, None), ("altmask" as name, enum) :: rst ->
          let typ = allowed_val_type_of_string name in
          loop ((n, t, Some (typ, enum)), rst)
      | (Some n, Some t, a), [] ->
          n, t, a
      | _ ->
          failwith "not a field" in
    loop ((None, None, None), attrs) in
  { name; typ; allowed; value }


type field = unit field_t
  [@@deriving show]

let field_of_xml = field_t_of_xml ()


type expr_field = (expression list) field_t
  [@@ deriving show]

let expr_field_of_xml = function
  | "exprfield", attrs, children ->
      let value = children |> List.map (function
        | Xml.Element el -> expression_of_xml el
        | _ -> failwith "not an exprfield") in
      field_t_of_xml value attrs
  | _ -> failwith "not an exprfield"


type list_field = (expression option) field_t
  [@@deriving show]

let list_field_of_xml attrs children =
  let value = match children with
    | [Xml.Element el] -> Some (expression_of_xml el)
    | [] -> None
    | _ -> failwith "not a list field" in
  field_t_of_xml value attrs


type field_type =
  [ `Fd of string
  | `Pad of padding
  | `Field of field
  | `List of list_field
  | `Required_start_align of required_start_align
  | `Doc of doc ]
  [@@deriving show]

let field_type_of_xml = function
  | "fd", ["name", name], [] ->
      `Fd name
  | "pad", attrs, [] ->
      `Pad (pad_of_xml attrs)
  | "field", attrs, [] ->
      `Field (field_of_xml attrs)
  | "list", attrs, children ->
      `List (list_field_of_xml attrs children)
  | "required_start_align", attrs, [] ->
      `Required_start_align (required_start_align_of_xml attrs)
  | "doc", _, _ ->
      `Doc "FIXME"
  | x, _, _ ->
      failwith (Printf.sprintf "not a field type: %s" x)

let field_type_of_xml' = function
  | Xml.Element el ->
    field_type_of_xml el
  | _ ->
    failwith "invalid field type"


type case_type = [ `Bit | `Int ]
[@@deriving show]

let case_type_of_string = function
  | "bitcase" ->  `Bit
  | "case" -> `Int
  | _ -> failwith "not a case type"

let case_type_of_string' = function
  | "value" -> `Int | "bit" -> `Bit
  | _ -> failwith "not a case type"


(*
 * Parse switches
 *)
type switch =
  { name : string
  ; expr : expression
  ; align : required_start_align option
  ; cases : case list }
  [@@deriving show]

and case =
  { typ : case_type
  ; name_c : string option
  ; exprs : expression list
  ; align_c : required_start_align option
  ; fields : field_type list
  ; switch : switch option }
  [@@deriving show]

let rec case_of_xml (typ, name_c, children) =
  (* Parse expressions until we fail or we hit a start align declaration.
   * Clearly the designers of this format were above subtleties such as
   * "structured data". *)
  let rec parse_exprs exprs = function
    | Xml.Element ("required_start_align", a, []) :: rst ->
      let align = required_start_align_of_xml a in
      (List.rev exprs), Some align, rst
    | (Xml.Element x) as f :: rst ->
      (try
        let expr = expression_of_xml x in
        parse_exprs (expr :: exprs) rst
      with Failure _ ->
        (List.rev exprs), None, (f :: rst))
    | _ ->
      failwith "invalid switch"
  in
  let rec parse_fields fields = function
    | Xml.Element ("switch", ["name", name], children) :: [] ->
      let switch = switch_of_xml name children in
      fields, Some switch
    | Xml.Element ("switch", _, _) :: _ ->
      failwith "switch not in tail position"
    | [] ->
      fields, None
    | Xml.Element f :: rst ->
      let field = field_type_of_xml f in
      parse_fields (field :: fields) rst
    | _ ->
      failwith "invalid case field"
  in
  let typ = case_type_of_string typ in
  let name_c = one_attr_opt "name" name_c in
  let exprs, align_c, children = parse_exprs [] children in
  let fields, switch = parse_fields [] children in
  { typ; name_c; exprs; align_c; fields; switch }

and switch_of_xml name children =
  let parse_case = function
    | Xml.Element (("bitcase", _, _) as c)
    | Xml.Element (("case", _, _)    as c) ->
      case_of_xml c
    | _ ->
      failwith "invalid switch case element"
  in
  (* The first element should be the switch expression, so we only want that,
   * plus the start align if it exists. *)
  let expr, align, children = match children with
    | Xml.Element x :: Xml.Element ("required_start_align", a, []) :: rst ->
      let align = required_start_align_of_xml a in
      x, Some align, rst
    | Xml.Element _ :: [] ->
      failwith "invalid switch: missing fields"
    | Xml.Element x :: rst ->
      x, None, rst
    | _ ->
      failwith "invalid switch"
  in
  let expr = expression_of_xml expr in
  let cases = List.map parse_case children in
  { name; expr; align; cases }


type x_struct =
  { name : string
  ; fields : field_type list
  ; switch : switch option }
[@@deriving show]

let struct_of_xml name children =
  let rec loop = function
    | (fields, switch), [] ->
        fields, switch
    | (f, None), Xml.Element ("switch", ["name", name], children) :: rst ->
        loop ((f, Some (switch_of_xml name children)), rst)
    | (f, s), Xml.Element x :: rst ->
        loop ((field_type_of_xml x :: f, s), rst)
    | _ -> failwith "not a struct" in
  let fields, switch =
    let (f, s) = loop (([], None), children) in
    List.rev f, s in
  { name; fields; switch }


type event_type_selector =
  { extension : string
  ; xge : bool
  ; opcode_min : int
  ; opcode_max : int }
[@@deriving show]

let event_type_selector_of_xml attrs =
  let get_attr x = List.assoc x attrs in
  let extension = get_attr "extension" in
  let xge = bool_of_string @@ get_attr "xge" in
  let opcode_min = int_of_string @@ get_attr "opcode-min" in
  let opcode_max = int_of_string @@ get_attr "opcode-max" in
  { extension; xge; opcode_min; opcode_max }


type event_struct =
  { name : string
  ; allowed : event_type_selector list }
[@@deriving show]

let event_struct_of_xml name children =
  let allowed = children |> List.map (function
    | Xml.Element ("allowed", attrs, []) ->
        event_type_selector_of_xml attrs
    | _ -> failwith "not an eventstruct") in
  { name; allowed }


type struct_contents =
  { fields : field_type list
  ; switch : switch option }
[@@deriving show]

let struct_contents_of_xml children =
  let rec loop = function
    | (fields, switch), [] ->
        fields, switch
    | (f, None), Xml.Element ("switch", ["name", name], children) :: rst ->
        loop ((f, Some (switch_of_xml name children)), rst)
    | (f, s), Xml.Element x :: rst ->
        loop ((field_type_of_xml x :: f, s), rst)
    | _ -> failwith "not a struct" in
  let fields, switch =
    let (f, s) = loop (([], None), children) in
    List.rev f, s in
  { fields; switch }


type field_or_expr_field =
  [ `Field of field_type
  | `Expr_field of expr_field ]
[@@deriving show]

type request_children =
  { fields : field_or_expr_field list
  ; switch : switch option }
[@@deriving show]

type request =
  { name : string
  ; opcode : int
  ; combine_adjacent : bool
  ; children : request_children
  ; reply : struct_contents option }
[@@deriving show]

let request_of_xml attrs children =
  let get_attr x = List.assoc x attrs in
  let name = get_attr "name" in
  let opcode = int_of_string @@ get_attr "opcode" in
  let combine_adjacent = app_with_default bool_of_string false
    @@ List.assoc_opt "combine-adjacent" attrs in
  let children, reply =
    let rec loop = function
      | (c, r), [] -> c, r
      | (c, None), Xml.Element ("reply", [], children) :: rst ->
          loop ((c, Some (struct_contents_of_xml children)), rst)
      | ((f, None), r), Xml.Element ("switch", ["name", name], children) :: rst ->
          loop (((f, Some (switch_of_xml name children)), r), rst)
      | ((f, s), r), Xml.Element x :: rst ->
          let field =
            try `Field (field_type_of_xml x)
            with _ -> `Expr_field (expr_field_of_xml x) in
          loop (((field :: f, s), r), rst)
      | _ -> failwith "not a request" in
    let (f, s), r = loop ((([], None), None), children) in
    { fields = List.rev f; switch = s }, r in
  { name; opcode; combine_adjacent; children; reply }


type error =
  { name : string
  ; number : int
  ; fields : field_type list }
[@@deriving show]

let error_of_xml attrs children =
  let name = List.assoc "name" attrs in
  let number = int_of_string @@ List.assoc "number" attrs in
  let fields = List.map field_type_of_xml' children in
  { name; number; fields }



type event =
  { name : string
  ; code : int
  ; no_sequence_number : bool
  ; align : required_start_align option
  ; fields : field_type list }
  [@@deriving show]

let event_of_xml attrs children =
  let name = List.assoc "name" attrs in
  let code = int_of_string @@ List.assoc "number" attrs in
  let no_sequence_number = match List.assoc_opt "no-sequence-number" attrs with
    | Some x -> bool_of_string x | None -> false
  in
  let align, children = match children with
    | Xml.Element ("required_start_align", align, []) :: rst ->
      let align = required_start_align_of_xml align in
      Some align, rst
    | _ ->
      None, children
  in
  let fields = List.map field_type_of_xml' children in
  { name; code; no_sequence_number; align; fields }


(*
 * Parse enums
 *)
type enum_item = string * int
  [@@deriving show]

type enum_bitmask =
  { bits : enum_item list
  ; vals : enum_item list }
  [@@deriving show]

type enum =
  [ `Bitmask of enum_bitmask
  | `Enum of enum_item list ]
  [@@deriving show]

let enum_items_of_xml items =
  let open Xml in
  let rec parse_items (v1, v2) = function
    | [] ->
      v1, v2, None
    | Element ("item", ["name", name], [Element ("value", [], [PCData v])]) :: rest ->
      let v1 = (name, int_of_string v) :: v1 in
      parse_items (v1, v2) rest
    | Element ("item", ["name", name], [Element ("bit",   [], [PCData v])]) :: rest ->
      let v2 = (name, int_of_string v) :: v2 in
      parse_items (v1, v2) rest
    | Element ("doc", _, _) :: [] ->
      v1, v2, Some "FIXME"
    | Element ("doc", _, _) :: _ ->
      failwith "invalid enum: doc item not in tail position"
    | _ ->
      failwith "invalid enum element"
  in
  let v1, v2, doc = parse_items ([], []) items in
  match v2 with
  | [] -> `Enum v1, doc
  | v2 -> `Bitmask { bits = v2; vals = v1 }, doc


type declaration =
  [ `Import of string
  | `X_id of string
  | `Type_alias of string * string
  | `Event_alias of string * (string * int)
  | `Error_alias of string * (string * int)
  | `X_id_union of string * string list
  | `Enum of string * enum * doc
  | `Struct of x_struct
  | `Event_struct of event_struct
  | `Union of x_struct
  | `Request of request
  | `Event of event
  | `Generic_event of event
  | `Error of error ]
  [@@deriving show]


type extension_info =
  { file : string
  ; name : string
  ; xname : string
  ; multiword : bool
  ; version : int * int }


let declaration_of_xml =
  let failure = Failure "unrecognized element" in
  let open Xml in
  function PCData _ -> raise failure | Element x ->
  match x with
  | "import", [], [PCData file] ->
      `Import file

  | "xidtype", ["name", id], [] ->
      `X_id id

  | "xidunion", ["name", id], children ->
      let get_type = function
        | Element ("type", [], [PCData name]) -> name
        | _ -> failwith "unrecognized element in X_id_union" in
      let types = List.map get_type children in
      `X_id_union (id, types)

  | "enum", ["name", name], children ->
      let items, doc = enum_items_of_xml children in
      `Enum (name, items, doc)

  | "typedef", ["oldname", old_name; "newname", new_name], []
  | "typedef", ["newname", new_name; "oldname", old_name], [] ->
      `Type_alias (old_name, new_name)

  | "union", ["name", name], children ->
      `Union (struct_of_xml name children)

  | "struct", ["name", name], children ->
      `Struct (struct_of_xml name children)

  | "eventstruct", ["name", name], children ->
      `Event_struct (event_struct_of_xml name children)

  | "event", attrs, children ->
    let is_generic = match List.assoc_opt "xge" attrs with
      | Some x -> bool_of_string x | None -> false in
    if is_generic then
      `Generic_event (event_of_xml attrs children)
    else
      `Event (event_of_xml attrs children)

  | "error", attrs, children ->
      `Error (error_of_xml attrs children)

  | "eventcopy", attrs, [] ->
      let get_attr x = List.assoc x attrs in
      let new_name = get_attr "name" in
      let old_name = get_attr "ref" in
      let number = int_of_string @@ get_attr "number" in
      `Event_alias (old_name, (new_name, number))

  | "errorcopy", attrs, [] ->
      let get_attr x = List.assoc x attrs in
      let new_name = get_attr "name" in
      let old_name = get_attr "ref" in
      let number = int_of_string @@ get_attr "number" in
      `Error_alias (old_name, (new_name, number))

  | "request", attrs, children ->
      `Request (request_of_xml attrs children)

  | _ ->
      raise failure


let extension_of_xml =
  let failure = Failure "not an xcb root element" in
  function
  | Xml.Element ("xcb", attrs, children) ->
      begin try
        let get_attr x = List.assoc x attrs in
        let version =
          let major = int_of_string @@ get_attr "major-version" in
          let minor = int_of_string @@ get_attr "minor-version" in
          major, minor in
        let multiword =
          match List.assoc_opt "extension-multiword" attrs with
          | Some x -> bool_of_string x | None -> false in
        let file = get_attr "header" in
        let xname = get_attr "extension-xname" in
        let name = get_attr "extension-name" in
        let declarations = List.map declaration_of_xml children in
        { file; name; xname; multiword; version }, declarations
      with Not_found -> raise failure end
  | Xml.PCData _ | Xml.Element _ ->
      raise failure


let xproto = function
  | Xml.Element ("xcb", ["header", "xproto"], children) ->
      List.map declaration_of_xml children
  | Xml.PCData _ | Xml.Element _ ->
      failwith "not the core protocol specification"


let is_xproto = function
  | Xml.Element ("xcb", ["header", "xproto"], children) -> true
  | Xml.PCData _ | Xml.Element _ -> false


let () =
  List.iter (fun file ->
    let file = Xml.parse_file file in
    let _ =
      if is_xproto file then
        xproto file
      else
        let (info, decls) = extension_of_xml file in
        Printf.printf "%s: %s %s %B\n" info.file info.name info.xname info.multiword;
        decls in
    (* List.iter (function `Event ev -> Format.printf "%s\n" (show_event ev) | _ -> ()) decls; *)
    (* List.iter (fun x -> Format.printf "%s\n" @@ show_declaration x) decls;*)
    ())
    (List.tl (Array.to_list Sys.argv))


  (*
open OUnit2

let test_parse f test_ctx =
  let file = Xml.parse_file f in
  let _ =
    if is_xproto file then
      xproto file
    else
      let ext = extension_of_xml file in
      ext.declarations in
  ()


let test_files =
  [ "bigreq.xml"; "composite.xml"; "damage.xml"; "dpms.xml"; "dri2.xml"
  ; "dri3.xml"; "ge.xml"; "glx.xml"; "present.xml"; "randr.xml"
  ; "record.xml"; "render.xml"; "res.xml"; "screensaver.xml"; "shape.xml"
  ; "shm.xml"; "sync.xml"; "xc_misc.xml"; "xevie.xml"; "xf86dri.xml"
  ; "xfixes.xml"; "xinerama.xml"; "xinput.xml"; "xkb.xml"; "xprint.xml"
  ; "xproto.xml"; "xselinux.xml"; "xtest.xml"; "xvmc.xml"; "xv.xml"
  ]


let suite =
  "Testing parsing" >:::
    List.map (fun f ->
      ("Parsing " ^ f) >:: (test_parse ("xproto/src/" ^ f)))
      test_files


let () =
  run_test_tt_main suite2
  *)

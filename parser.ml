let app_with_default f x = function
  | Some a -> f a | None -> x

let option_app f = function
  | Some a -> Some (f a) | None -> None


type pad_type =
  [ `Bytes | `Align ]

type padding =
  { typ : pad_type
  ; amount : int
  ; serialize : bool }

let pad_of_xml attrs =
  let serialize = app_with_default bool_of_string false
    @@ List.assoc_opt "serialize" attrs in
  let typ, amount =
    try       `Bytes, List.assoc "bytes" attrs
    with _ -> `Align, List.assoc "align" attrs in
  { typ; amount = int_of_string amount; serialize }


type required_start_align =
  { align  : int
  ; offset : int option }

let required_start_align_of_xml attrs =
  let align = int_of_string @@ List.assoc "align" attrs in
  let offset = option_app int_of_string @@ List.assoc_opt "offset" attrs in
  { align; offset }


type op =
  [ `Add | `Sub | `Mul | `Div | `Bit_and | `Bit_left_shift ]

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
  | `Pop_count of expression
  | `Sum_of of string * expression option
  | `List_element_ref
  | `Value of int
  | `Bit of int ]

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
      `Pop_count (expression_of_xml expr)
  | "sumof", ["ref", ref], [Element expr] ->
      `Sum_of (ref, Some (expression_of_xml expr))
  | "sumof", ["ref", ref], [] ->
      `Sum_of (ref, None)
  | "listelement-ref", [], [] ->
      `List_element_ref
  | "value", [], [PCData v] ->
      `Value (int_of_string v)
  | "bit", [], [PCData v] ->
      `Value (int_of_string v)
  | _ ->
      failwith "not an expression"


type var_attr =
  [ `Enum | `Alt_enum
  | `Mask | `Alt_mask ]

let var_attr_of_string = function
  | "enum" -> `Enum
  | "mask" -> `Mask
  | "altenum" -> `Alt_enum
  | "altmask" -> `Alt_mask
  | x -> failwith (Printf.sprintf "not a var_attr: %s" x)


type field =
  { name : string
  ; typ  : string
  ; attr : (var_attr * string) option }

let field_of_xml attrs =
  let name, typ, attr =
    let rec loop =
      function
      | (None, t, a), ("name", name) :: rst ->
          loop ((Some name, t, a), rst)
      | (n, None, a), ("type", typ) :: rst ->
          loop ((n, Some typ, a), rst)
      | (n, t, None), (name, attr) :: rst ->
          let typ = var_attr_of_string name in
          loop ((n, t, Some (typ, attr)), rst)
      | (Some n, Some t, a), [] ->
          n, t, a
      | _ ->
          failwith "not a field" in
    loop ((None, None, None), attrs) in
  { name; typ; attr }


type expr_field =
  { name : string
  ; typ  : string
  ; attr : (var_attr * string) option
  ; children : expression list }

let field_expr_of_xml = function
  | "exprfield", attrs, children ->
      let { name; typ; attr } : field = field_of_xml attrs in
      let children = children |> List.map (function
        | Xml.Element el -> expression_of_xml el
        | _ -> failwith "not an exprfield") in
      { name; typ; attr; children }
  | _ -> failwith "not an exprfield"


type list_var =
  { name : string
  ; typ  : string
  ; attr : (var_attr * string) option
  ; children : expression option }

let list_var_of_xml attrs children =
  let { name; typ; attr } : field = field_of_xml attrs in
  let children = match children with
    | [Xml.Element el] -> Some (expression_of_xml el)
    | [] -> None
    | _ -> failwith "not an exprfield" in
  { name; typ; attr; children }


type field_type =
  [ `Fd of string
  | `Pad of padding
  | `Field of field
  | `List of list_var
  | `Required_start_align of required_start_align
  | `Doc ]

let field_type_of_xml = function
  | "fd", ["name", name], [] ->
      `Fd name
  | "pad", attrs, [] ->
      `Pad (pad_of_xml attrs)
  | "field", attrs, [] ->
      `Field (field_of_xml attrs)
  | "list", attrs, children ->
      `List (list_var_of_xml attrs children)
  | "required_start_align", attrs, [] ->
      `Required_start_align (required_start_align_of_xml attrs)
  | "doc", _, _ ->
      `Doc
  | x, _, _ ->
      failwith (Printf.sprintf "not a field type: %s" x)


type case_type = [ `Bit | `Int ]

let case_type_of_string = function
  | "bitcase" ->  `Bit
  | "case" -> `Int
  | _ -> failwith "not a case type"

let case_type_of_string' = function
  | "value" -> `Int | "bit" -> `Bit
  | _ -> failwith "not a case type"


type case =
  { typ : case_type
  ; expressions : expression list
  ; fields : field_type list }

let case_of_xml typ children =
  let typ = case_type_of_string typ in
  let rec loop = function
    | (e, f), [] -> e, f
    | (e, f), Xml.Element x :: rst ->
        let e, f =
          try expression_of_xml x :: e, f
          with Failure _ -> e, field_type_of_xml x :: f in
        loop ((e, f), rst)
    | _ -> failwith "not a case" in
  let expressions, fields =
    loop (([], []), children) |> fun (e, f) ->
      List.rev e, List.rev f in
  { typ; expressions; fields }


type switch =
  { name : string
  ; expr : expression
  ; align : required_start_align option
  ; cases : case list }

let switch_of_xml name children =
  let rec loop = function
    | (Some expr, align, cases), [] ->
        expr, align, cases
    | (e, a, cases), Xml.Element ("bitcase" as typ, [], ch) :: rst
    | (e, a, cases), Xml.Element ("case" as typ, [], ch) :: rst ->
        loop ((e, a, case_of_xml typ ch :: cases), rst)
    | (e, None, c), Xml.Element ("required_start_align", attrs, []) :: rst ->
        let align = required_start_align_of_xml attrs in
        loop ((e, Some align, c), rst)
    | (None, a, c), Xml.Element x :: rst ->
        let expr = expression_of_xml x in
        loop ((Some expr, a, c), rst)
    | _ -> failwith "not a switch" in
  let expr, align, cases =
    let (e, a, c) = loop ((None, None, []), children) in
    e, a, List.rev c in
  { name; expr; align; cases }


type x_struct =
  { name : string
  ; fields : field_type list
  ; switch : switch option }

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

let event_struct_of_xml name children =
  let allowed = children |> List.map (function
    | Xml.Element ("allowed", attrs, []) ->
        event_type_selector_of_xml attrs
    | _ -> failwith "not an eventstruct") in
  { name; allowed }


type struct_contents =
  { fields : field_type list
  ; switch : switch option }

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

type request_children =
  { fields : field_or_expr_field list
  ; switch : switch option }

type request =
  { name : string
  ; opcode : int
  ; combine_adjacent : bool
  ; children : request_children
  ; reply : struct_contents option }

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
            with _ -> `Expr_field (field_expr_of_xml x) in
          loop (((field :: f, s), r), rst)
      | _ -> failwith "not a request" in
    let (f, s), r = loop ((([], None), None), children) in
    { fields = List.rev f; switch = s }, r in
  { name; opcode; combine_adjacent; children; reply }


type error =
  { name : string
  ; number : int
  ; fields : field_type list }

let error_of_xml attrs children =
  let get_attr x = List.assoc x attrs in
  let name = get_attr "name" in
  let number = int_of_string @@ get_attr "number" in
  let fields = children |> List.map (function
    | Xml.Element els -> field_type_of_xml els
    | _ -> failwith "not an error") in
  { name; number; fields }



type event =
  { name : string
  ; number : int
  ; sequence_number : bool (* NOTE: opposite of spec *)
  ; xge : bool
  ; fields : field_type list }

let event_of_xml attrs children =
  let get_attr x = List.assoc x attrs in
  let get_attr_opt x = List.assoc_opt x attrs in
  let name = get_attr "name" in
  let number = int_of_string @@ get_attr "number" in
  let sequence_number = not @@ app_with_default bool_of_string false
    @@ get_attr_opt "no-sequence-number" in
  let xge = app_with_default bool_of_string false @@ get_attr_opt "xge" in
  let fields = children |> List.map (function
    | Xml.Element els -> field_type_of_xml els
    | _ -> failwith "not an error") in
  { name; number; sequence_number; xge; fields }


type enum =
  { name : string
  ; items : (string * (case_type * int)) list }

let rec enum_items_of_xml acc =
  let open Xml in function
  | [] -> acc
  | Element ("item", ["name", name], [Element ("value" as t, [], [PCData v])]) :: rst
  | Element ("item", ["name", name], [Element ("bit" as t, [], [PCData v])]) :: rst ->
      enum_items_of_xml ((name, (case_type_of_string' t, int_of_string v)) :: acc) rst
  | Element ("doc", _, _) :: rst ->
      enum_items_of_xml acc rst
  | Element (x, _, _) :: _ -> failwith (Printf.sprintf "not an enum: %s" x)
  | _ -> failwith "ayy"

let enum_of_xml name children : enum =
  let items = enum_items_of_xml [] children in
  { name; items }


type declaration =
  [ `Import of string
  (* Import types declared in another file *)

  | `X_id of string
  (* Generic X resource ID
   * implemented with an uint32_t in C *)

  | `Type_alias of string * string
  (* Alias a type to a new name *)

  | `Event_alias of string * (string * int)
  (* Alias an event with a new event name and number *)

  | `Error_alias of string * (string * int)
  (* Alias an error with a new error name and number *)

  | `X_id_union of string * string list
  (* Union of X resource IDs *)

  | `Enum of enum
  (* Enumeration type, where default is 0 for the first
   * and one more than the previous for all the others *)

  | `Struct of x_struct
  | `Event_struct of event_struct
  | `Union of x_struct
  | `Request of request
  | `Event of event
  | `Error of error
  ]


type extension =
  { file : string
  (* the filename of the extension's XML description file *)

  ; name : string
  (* extension name (in CamelCase) *)

  ; xname : string
  (* name used by QueryExtension check for the presence of the extension *)

  ; multiword : bool
  (* whether the resulting C prefix should be composed
   * of a single or multiple words separated by underscores. *)

  ; version : int * int
  (* major * minor version *)

  ; declarations : declaration list }


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
      `Enum (enum_of_xml name children)

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

  (*
let print_declaration x =
  let t, name =
    match x with
    | `Import file ->
        "import", file
    | `Type_alias (old, nw) ->
        "type-alias", nw ^ " = " ^ old
    | `Event_alias (old, (nw, num)) ->
        "event-alias",  nw ^ "(" ^ string_of_int num ^ ") = " ^ old
    | `Error_alias (old, (nw, num)) ->
        "error-alias", nw ^ "(" ^ string_of_int num ^ ") = " ^ old
    | `X_id name ->
        "X-id", name
    | `X_id_union (name, types) ->
        "X-id-union", name ^ " = " ^ String.concat " | " types
    | `Enum (name, items) ->
        let to_s (name, n) = name  ^ "(" ^ string_of_int n ^ ")" in
        "enum", name ^ " = " ^ String.concat " | " (List.map to_s items)
    | `Struct name ->
        "struct", name
    | `Event_struct name ->
        "event-struct", name
    | `Union name ->
        "union", name
    | `Request (name, num, _) ->
        "request", name ^ "(" ^ string_of_int num ^ ")"
    | `Event ((name, num), _, _) ->
        "event", name ^ "(" ^ string_of_int num ^ ")"
    | `Error (name, num) ->
        "error", name ^ "(" ^ string_of_int num ^ ")"
  in
  print_string t;
  print_char ' ';
  print_string name
  *)

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
        { file; name; xname; multiword; version; declarations }
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
  let file = Xml.parse_file Sys.argv.(1) in
  let _ =
    if is_xproto file then
      xproto file
    else
      let ext = extension_of_xml file in
      ext.declarations in
  ()

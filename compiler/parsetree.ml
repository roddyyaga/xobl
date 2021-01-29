(**
Here's an explaination of some of the most puzzling aspects of the X11 spec.

{2 Types}

Types in the spec refer to two differen kinds of types, which we'll call
{b basic} and {b composite}.
Types are used to give a wire representation to the values defined in the spec,
so they define size, alignment, and in the case of integers signedness.

{3 Basic types}
Basic types such as [INT8], [CARD16] and [float] are used to define the wire
representation of numbers. All types except for [void] should map to a certain
type in the output bindings.

The [void] type is used for defining fields in which the type doesn't matter,
like padding, or where it cannot be known in advance, like in [GetProperty].

{3 Composite types]
Composite types are aggregations of basic types in the form of simple fields,
lists, or expressions, and also padding and alignment information.

Their size is known at compile time except for generic events, requests, and
responses, which 
*)

type doc = Doc [@@deriving show]

(** From what I could gather, the X11 protocol used to be defined by its C
    implementation so the padding between fields used to be "whatever the C
    compiler said". When it was formalized in the XML protocol files they
    decided to make padding explicit and probably ran into 

    This led to the introduction of an alignment checker in commit
    [c499401bdac3b87bd4f9cd4bc64cfd1781ab447f], and of the
    required_start_align field.
    The algorithm is described in the commit message on the xcb/proto repo. *)
type required_start_align = { al_align : int; al_offset : int option }
[@@deriving show]

type enum_item = Item_value of int64 | Item_bit of int [@@deriving show]

type binop = Add | Sub | Mul | Div | Bit_and | Bit_left_shift
[@@deriving show]

type unop = Bit_not [@@deriving show]

type ident = { id_module : string option; id_name : string } [@@deriving show]

type prim =
  | Void
  | Char
  | Byte
  | Bool
  | Int8
  | Int16
  | Int32
  | Fd
  | Card8
  | Card16
  | Card32
  | Card64
  | Float
  | Double
  | Xid  (** maps to a Card32 *)
[@@deriving show]

type type_ = Type_primitive of prim | Type_ref of ident [@@deriving show]

type expression =
  | Binop of binop * expression * expression
  | Unop of unop * expression
  | Field_ref of string
  | Param_ref of { param : string; type_ : type_ }
  | Enum_ref of { enum : ident; item : string }
  | Pop_count of expression
  | Sum_of of { field : string; by_expr : expression option }
  | List_element_ref
  | Expr_value of int64
  | Expr_bit of int
[@@deriving show]

type 'a range = { min : 'a; max : 'a } [@@deriving show]

type allowed_events =
  { ae_module : string; ae_opcode_range : int range; ae_is_xge : bool }
[@@deriving show]

type pad = Pad_bytes of int | Pad_align of int [@@deriving show]

type field_allowed =
  | Allowed_enum of ident
  | Allowed_mask of ident
  | Allowed_alt_enum of ident
  | Allowed_alt_mask of ident
[@@deriving show]

type field_type = { ft_type : type_; ft_allowed : field_allowed option }
[@@deriving show]

type switch_cond = Cond_bit_and of expression | Cond_eq of expression
[@@deriving show]

type switch = { sw_name : string; sw_cond : switch_cond; sw_cases : case list }
[@@deriving show]

and case =
  { cs_name : string option; cs_cond : expression list; cs_fields : field list }
[@@deriving show]

and field =
  | Field_expr of { name : string; type_ : field_type; expr : expression }
  | Field_list of
      { name : string; type_ : field_type; length : expression option }
  | Field_file_descriptor of string
  | Field_pad of { pad : pad; serialize : bool }
  | Field_switch of switch
  | Field of { name : string; type_ : field_type }
[@@deriving show]

type request_reply = { fields : field list; doc : doc option } [@@deriving show]

type declaration =
  | Import of string
  | Xid of string
  | Xid_union of { name : string; types : ident list }
  | Typedef of { name : string; type_ : type_ }
  | Event_copy of { name : string; event : ident; ev_number : int }
  | Error_copy of { name : string; error : ident; er_number : int }
  | Enum of
      { name : string; items : (string * enum_item) list; doc : doc option }
  | Event_struct of { name : string; allowed_events : allowed_events list }
  | Union of { name : string; members : field list }
  | Event of
      { name : string
      ; number : int
      ; is_generic : bool
      ; no_sequence_number : bool
      ; fields : field list
      ; doc : doc option
      }
  | Error of { name : string; number : int; fields : field list }
  | Struct of { name : string; fields : field list }
  | Request of
      { name : string
      ; opcode : int
      ; combine_adjacent : bool
      ; fields : field list
      ; reply : request_reply option
      ; doc : doc option
      }
[@@deriving show]

type xcb =
  | Core of declaration list
  | Extension of
      { name : string
      ; file_name : string
      ; query_name : string
      ; multiword : bool
      ; version : int * int
      ; declarations : declaration list
      }
[@@deriving show]

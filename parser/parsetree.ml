(** {2 A word on data alignment}
C compilers will automatically add some padding between struct members as
needed to make sure the members are aligned in memory for optimal access
(refer to http://www.catb.org/esr/structure-packing/ for a much better
explaination). libxcb was designed only with C in mind, so alignment
padding used to implicit and not marked in the protocol files in any way.

In an admirable effort, somebody (I haven't followed the mailing list, but
the commit adding this was authored by Christian Linhart) decided to not only
explicitate these alignment pads in the protocol (through the <pad align />
fields, here [Pad_align]), but also to add an alignment checker that ensures
there's no implicit alignment left in any of the protocol descriptions.

Checking the protocol files for correctness is way beyond the scope of this
project, so I'll just trust the Xproto team to run their own checkers.

See commit [c499401bdac3b87bd4f9cd4bc64cfd1781ab447f] in the xproto repo
for more information on the alignment checker.
*)

type padding =
  | Pad_bytes of int
  | Pad_align of int * bool
(** The second argument here represents the [serialize] flag some align pad
    fields have in xkb, and can be safely ignored.

    When adding the alignment checker, the maintainers probably forgot to
    special-case the align pads in the generator so that they wouldn't be
    added to the generated C code, so a few of them slipped through and
    made it into an official release. To keep ABI compatility with
    both the versions prior and after this mistake, [serialize] was added.
    It is only [true] in a few requests inside xkb; when not specified
    it is false and will hide the field in the typedef.

    Since we don't give a damn about ABI compatibility with libxcb, this
    argument can be safely ignored.
*)

(** {2 Enums}
Enums are named values. They do not have a wire representation of their own,
so they can only be used in conjunction with a basic type which will define
their wire representation.

Enums are used for two different purposes: {b enumerations} and {b bitmasks}.
An enum may be one or both at the same time, depending on whether it's referred
to as [enum]/[altenum] or [mask]/[altmask] in a field type, or as [enumref] in
an expression (which counts as an [enum]).

{3 Enumerations}
Enumerations are simple mappings from names to values. Only one value from an
enumeration is allowed at a time.

{3 Bitmasks}
Bitmasks contain both [bit] values and [value] values.
{ul {- [bit] is the 0-based position of the set bit in the resulting value
       ([3] is [0b1000], or [1 << 3]).}
    {- [value] is a useful literal default, such as [NoEvent] in
       [xproto:EventMask].}}
A bitmask can be either a list of [bit]s or a single [value].
*)

type enum_vals = (string * int64) list
type enum_bits = (string * int) list


type enum =
  { en_vals : enum_vals
  ; en_bits : enum_bits }


(** {2 Expressions} *)

type binop =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Bitwise_and
  | Bitwise_left_shift

type unop =
  | Bitwise_not

type expression =
  | Binop of binop * expression * expression
  | Unop of unop * expression
  | Field_ref of string
    (** The value of another field in the same structure. *)
  | Param_ref of string * string
    (** The value of a field in a structure that contains the current one.
        [(name, type)] *)
  | Enum_ref of string * string
    (** The value of an identifier in an enum. [(enum, item)] *)
  | Sum_of of string * expression option
    (** Sum f the elements in a list field in the same structure.
        If present, the expression is applied to each list element
        in the element's context before summing it. *)
  | Current_ref
    (** The current element in a Sum_of expression. *)
  | Pop_count of expression
    (** The number of set bits (e.g. popcount 0b01101 = 3). *)
  | Const_value of int
    (** A literal value. *)
  | Const_bit of int
    (** A literal bitmask index. *)


(** {2 Struct fields} *)

type allowed_vals =
  | Enum of string
  | Mask of string
  | Alt_enum of string
  | Alt_mask of string

type field_type =
  { ft_type    : string
  ; ft_allowed : allowed_vals option }

type switch_cond =
  | Cond_bitwise_and of expression
    (** Test that [expression & case_expression <> 0]. *)
  | Cond_eq of expression
    (** Test that [expression = case_expression]. *)

type field =
  | Pad of padding
  | Field of string * field_type
  | List of string * field_type * expression
    (** The expression defines the length of the list. *)
  | List_var of string * field_type
    (** A list of variable length, only present in structs that don't
        have a fixed size (generic events and requests), and it can only
        be the last element of the struct. *)
  | Expr_field of string * field_type * expression
    (** A field whose value is computed based on the other fields. *)
  | File_descriptor of string
  | Switch of string * switch

(** {3 Switch} *)

and switch =
  { sw_cond  : switch_cond
  ; sw_cases : case list }

and case =
  { cs_name   : string option
  ; cs_exprs  : expression list
  ; cs_fields : field list }

type declaration =
  | Import of string
  | X_id of string
  | X_id_union of string * string list
  | Enum of string * enum
  | Type_alias of string * string
  | Event of string * int * field list
  | Generic_event of string * int * field list
  | Event_no_sequence_number of string * int * field list
  | Event_alias of string * int * string
  | Error of string * int * field list
  | Error_alias of string * int * string
  | Struct of string * field list
  | Union of string * field list
  | Request of string * int * bool * field list * field list

type module_info =
  { name : string
  ; file_name : string
  ; query_name : string
  ; multiword : bool
  ; version : int * int}


type protocol_file =
  | Core of declaration list
  | Extension of module_info * declaration list

(* val parse_file : string -> protocol_file *)
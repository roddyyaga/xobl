type required_start_align =
  { al_align : int
  ; al_offset : int option }

type 'a with_align = 'a * required_start_align option
(* 
type switch =
  { sw_cond : cond
  ; sw_cases : case with_align list }

and case =
  { cs_expr : expression list
  ; cs_name : string option
  ; cs_fields : static_field list
  ; cs_switch : (string * switch with_align) option }

type 'a with_switch = 'a * (string * switch with_align) option

type 'a event_t =
  { ev_no_sequence_number : bool
  ; ev_fields : 'a list }

type event = static_field event_t

type generic_event = dynamic_field event_t

type error = static_field list *)

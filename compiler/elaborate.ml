(* 
TODO

- Resolve all identifiers to structs and enums and all types.
  i.e. some of them specify the module they're in, but some don't.

- Figure out what's the field that holds the length of a list and simply
  calculate it from the length of the list rather than exposing it to the API.

- Some list fields don't explicitly specify a length field even though they
  need one (e.g. QueryTextEvents in xproto), so we need to infer it somehow.
  There's some code in xcbgen that does this.

- There's a single exprfield in the spec and it's to check whether the length
  of a list is odd or even, we could just special-case it.

- Turn unions into switches. We definitely need some ad-hoc stuff for this
  but there's like 5 unions in total in the spec so it shouldn't be a problem.

  event randr.Notify
    switch -> u
    switch condition -> subCode
  event xproto.ClientMessage
    switch -> data
    switch condition -> format

  There's also some other confusing cases in xkb but we're explicitly not
  supporting xkb for the moment, because it seems to be broken.

- Turn switches whose expression is equality to an enum value into variants,
  and make another separate kind of field for the enum value like for the list
  length one.
*)

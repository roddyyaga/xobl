Finish the code generator in the Analyzer to narrow down what information is actually needed, and understand how to model the types.

Improve the Parser with stuff taken from the Analyzer so that it outputs types similar to those in the Analyzer, but without info that has to be inferred by looking at the previous declarations. The Parser shouldn't have to do module resolution.

The Analyzer should then take this output and figure out the types of all the fields in the declarations, what module the aliased types come from, which declarations and functions to output and the size of all structs.


# What needs to be done

- For list fields, we have to figure out what's the field that holds the length
  and have a separate kind of field for it; it's low-level stuff that shouldn't
  really be exposed to the user. (**DONE**)

- Some list fields don't explicitly specify a length field, but they need one
  (e.g. QueryTextExtents in xproto), so we need to infer it somehow.
  There's some code in xcbgen that does it, but I'm not sure how it works.

- We need to figure out how to fold unions into switches. This has to be done
  ad-hoc, because there's no agreed place to put the enum flag to distinguish
  between the members of the union.

- Turn switches whose expression is equality to an enum value into variants,
  and make another separate kind of field for the enum value, so that it's
  not exposed to the end user.

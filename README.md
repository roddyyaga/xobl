## STATUS
Trying to understand the X protocol spec and turn it into something usable.
Not nearly done.


## Contents
`Parser` attempts to parse the X11 XML spec into something structured.
In this stage, all information is preserved and what can be inferred in a
single pass without considering other declaration will be inferred.

`Analyzer` analyzes `Parser`'s output and infers all information that could be
useful for generating code. It resolves all bindings to figure out the type of
each declaration, tracks usage of all types to figure out what conversion
functions to generate and prune dead code, and in general attempts to output
a rich AST with all information that could be useful to languages with complex
type systems.

`Generator` takes the output of `Analyzer` and generates the OCaml code. In the
future I will attempt to partly functorize this so that generators for other
languages may easily be plugged in.


## Useful links
* https://www.x.org/releases/X11R7.7/doc/
* https://www.x.org/wiki/Development/
* [](https://www.youtube.com/watch?v=_U0guRQrlMA)

## Other implementations generated from the spec
* [Guile](https://github.com/mwitmer/guile-xcb)
* [Rust](https://github.com/sstewartgallus/rust-xcb)
* [Ruby](https://github.com/nbaum/alembic)
* [Javascript](https://github.com/sidorares/node-x11) (Node)
* [Clojure](https://github.com/noodlewiz/xcljb)
* [Emacs Lisp](https://github.com/ch11ng/xelb)
* [Elixir](https://github.com/chrys-h/XEB)
* [Haskell](https://github.com/aslatter/xhb) ([parser](https://github.com/aslatter/xcb-types))

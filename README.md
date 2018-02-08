## STATUS
Trying to understand the X protocol spec and turn it into something usable.
Not nearly done.


## Contents
`parser.ml` attempts to parse the horrors of the X11 XML spec into something
structured and understandable. `parser.mli` documents the output.

`analyzer.ml` analyzes the output of the parser and tries to turn it into
something more useful to a less braindead language than C.

In the future there will be an OCaml code generator, hopefully functorized so
that you can plug other languages into it.


## Useful links
* https://www.x.org/releases/X11R7.7/doc/
* https://www.x.org/wiki/Development/

## Other implementations generated from the spec
* [Guile](https://github.com/mwitmer/guile-xcb)
* [Rust](https://github.com/sstewartgallus/rust-xcb)
* [Ruby](https://github.com/nbaum/alembic)
* [Javascript](https://github.com/sidorares/node-x11) (Node)
* [Clojure](https://github.com/noodlewiz/xcljb)
* [Emacs Lisp](https://github.com/ch11ng/xelb)
* [Elixir](https://github.com/chrys-h/XEB)
* [Haskell](https://github.com/aslatter/xhb) ([parser](https://github.com/aslatter/xcb-types))

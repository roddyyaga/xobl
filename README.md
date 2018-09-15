## STATUS
The parser is ostensibly complete. The analyzer is still missing many passes,
and the generator is in its infancy.


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


## X documentation
* https://www.x.org/releases/X11R7.7/doc/
* https://www.x.org/wiki/Development/
* Many of the reasons behind decisions in the XCB protocol spec were documented in the commit messages on https://cgit.freedesktop.org/xcb/proto/log/

## Porting Xlib applications to XCB
* [Martin Gräßlin: KWin went XCB](https://www.youtube.com/watch?v=_U0guRQrlMA)
* https://blog.martin-graesslin.com/blog/2013/02/porting-kwin-to-xcb-making-c-usable-through-raii/
* https://blogs.oracle.com/solaris/porting-x-apps-to-xcb-v2
* https://community.kde.org/KWin/Xcb
* https://xcb.freedesktop.org/XcbPorting/
* https://linuxx.info/porting-inputplug-to-xcb-2/

## Other implementations generated from the spec
* Guile: [guile-xcb](https://github.com/mwitmer/guile-xcb)
* Emacs Lisp: [xelb](https://github.com/ch11ng/xelb)
* Haskell: [XHB](https://github.com/aslatter/xhb) ([parser](https://github.com/aslatter/xcb-types))
* Elixir: [XEB](https://github.com/chrys-h/XEB)
* Javascript (Node): [node-x11](https://github.com/sidorares/node-x11)
* Clojure: [xcljb](https://github.com/geremih/xcljb)
* Rust: [rust-xcb](https://github.com/sstewartgallus/rust-xcb)
* Ruby: [alembic](https://github.com/nbaum/alembic)

## Xlib implementations (not generated) of note
* Python: [python-xlib](https://github.com/python-xlib/python-xlib)
* Common Lisp: [CLX](https://github.com/sharplispers/clx)

## Further reading/watching
* [The real story behind Wayland and X](https://www.youtube.com/watch?v=GWQh_DmDLKQ) (Daniel Stone)

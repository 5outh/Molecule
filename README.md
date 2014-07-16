Molecule
========

A teeny-tiny statically typed programming language with inference.

Molecule is a contained experiment on using a zipper-based scheme for type inference in a non-polymorphic,
statically typed language. It only supports three data types, `Int`s, `Bool`s, and morphisms between them, i.e. `Int` -> `Bool`.

There are only a handful of commands that Molecule can handle:

- Int literals
- Boolean literals (`t` and `f`)
- Addition (`1 + 2`)
- Boolean Or (`f | t`)
- Lambda abstractions (`\x. x + x`), and
- Function application (`(\x.x) 80`)

`Molecule` can be interpreted using the `mi` REPL, which supports 3 operations:

- `:t <expr>` prints the inferred type of `<expr>`
- `:q` quits the REPL
- Anything else attempts to be interpreted as an expression, typechecked and evaluated.


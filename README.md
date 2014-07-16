Molecule
========

![molecule logo](./molecule-logo.png)

A teeny-tiny statically typed programming language with inference.

Molecule is a contained experiment on using a zipper-based scheme for type inference in a monomorphic,
statically typed language. It only supports three data types, `Int`s, `Bool`s, and morphisms between them, i.e. `Int -> Bool`.

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

An example `mi` session:

```haskell
> t
t
> 10
10
> 10 + 20
30
> f | f
f
> (\x.x+x) 30
60
> \x.\y.x+y
\x . \y . x + y
> :t \x.\y.x+y
\x . \y . x + y : Int -> Int -> Int
> :t (\x.\y.x+y) 10
(\x . \y . x + y) 10 : Int -> Int
> :t (\x.\y.x+y) 10 10
((\x . \y . x + y) 10) 10 : Int
> (\x.\y.x+y) 10 10
20
> :q
peace!
```

##### Type inference in Molecule (technical-ish discussion)

Molecule is very close to the simply typed lambda calculus (STLC) in the way that it operates, but doesn't support explicit type
annotations. Type checking the STLC is very simple, but it's a tad more difficult to infer types; I actually couldn't find any
great material out there for type inference in STLC, so I decided to put together a minimal programming language that would allow
me to come up with a nice scheme for doing just that, as an exploratory exercise.

Molecule's type inference mechanism is located in `Typechecker.hs`, and the types involved are located in `Types.hs`.  When designing [fievel](https://github.com/5outh/fievel), another tiny programming language I'm working on, I noticed that there was a lot of redundancy in
type inference. For example, If I wanted to find the type of the variable `a` in the expression `a + 10`, I had pattern match on both
`a + _` and `_ + b` for variables `a` and `b`. I came up with the following solution after realizing that I didn't want to have to do this:
I'd rather be able to tell the type of a variable *just from the fact that it's a variable*. Obviously that's impossible without context,
so that's where my idea came from.

The main idea driving the type inferencer comes from the idea of [Zippers](http://www.haskell.org/haskellwiki/Zipper), but we don't 
necessarily need an entire traversal on hand. In the case of Molecule, we only need to know *where the variable came from, one evaluation step prior*. Keeping track of this information while traversing the AST (which is done with use of `ReaderT`) allows Molecule to infer the type of a variable any time it is encountered.

For example, if we reach a variable `v` and we're coming from the first position in a `+` constructor above (a `CPlusA` `Crumb` in `Types.hs`),
`v` has type `Int`, by inversion of typing (given that `a : Int, b : Int => a + b : Int`). This is similarly done for `|`, and that's mostly
all there is to it, barring some caveats with function scope and typing contexts. The full code should be pretty readable, and the whole
type inference/ type checking algorithm is located in `Typechecker.hs`, which clocks in at 130 lines, including all of those pesky error messages.

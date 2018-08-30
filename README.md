Lasca Language
==============

[![Build Status](https://travis-ci.org/nau/lasca-compiler.svg?branch=master)](https://travis-ci.org/nau/lasca-compiler)
[![Join the chat at https://gitter.im/lasca-lang/compiler](https://badges.gitter.im/lasca-lang/Lobby.svg)](https://gitter.im/lasca-lang/compiler?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Lasca is Scala shifted towards Haskell.

Lasca is a LLVM-based statically or dynamically typed strict functional programming language. Simplified OCaml if you like.

It has a 'dynamic' compilation mode, meaning instant code generation without compile time type checking/inference, allowing instant compilation/execution cycle, and more freedom dynamic languages give.

It has a full type inference, parametric polymorphism, GC, algebraic data types, pattern matching,
and type classes are coming soon.

Imagine

- Scala with fast compilation/start time, optional dynamic typing, and without null
- Go with ADTs, global type inference, and parametric polymorphism
- Haskell with decent records syntax, runtime polymorphism, and string interpolation
- OCaml with typeclasses, overloaded +-*/ for ints and floats, and do-notation
- Rust with garbage collector without <::>!?
- Erlang with types and fast execution
- Python with multithreading, pattern matching, and multiline lambdas
- TypeScript with indentation significant syntax, and LLVM
- Julia with static type checking, and zero-based indexing

Inspired by:

- Scala
- Haskell, Liquid Haskell, Linear Haskell, Idris
- OCaml/SML/F#/1ML
- Clojure (persisted data structures, HAMT/CHAMP)
- Go (simplicity, speed, Any interface?, all-in-one compiler)
- Erlang (actors, immutability, simplicity, distributed)
- Python (docstrings, doctests, syntax)
- Julia
- Swift
- Nim
- Pony (ref caps, behaviours, actors)
- [Koka](https://github.com/koka-lang/koka) (algebraic effects)

Ideas
---

- light, non-symbol-polluted syntax (Python)
- indentation-based
- readability first
- fast development cycle
- presentation compiler for IDE integration
- IDE-friendly (intellisence 'dot-autocomplete', auto-formatting, compiler API)
- type-safe
- strict functional
- expression-based
- practical first, but as clean and concise as possible
- prefer things done one way
- LLVM backend
- JavaScript/WebAssembly backend (native or via LLVM/emscripten)
- no OOP and data subclassing/inheritance?
- syntactic sugar is ok
- no null
- annotations (Java/Python-style)
- annotation-based extensions
- macros based metaprogramming (like Scala Macros, Template Haskell)
- Concurrency Oriented Programming (Erlang). Objects are out. Concurrency is in.
- [Gradual Typing](http://homes.soic.indiana.edu/jsiek/what-is-gradual-typing/)
- Deferred Type Errors (runtime compilation mode, Haskell)
- Linear/affine types (Rust, Linear Haskell)?
- Liquid Type system (refinement types, [Leon](http://leon.epfl.ch), [Liquid Haskell](https://github.com/ucsd-progsys/liquidhaskell)) and
  [Z3](https://github.com/Z3Prover/z3)/[CVC4](https://cvc4.cs.stanford.edu/web/) as proof assistant.
- [Algebraic Subtyping for module system](https://www.cl.cam.ac.uk/~sd601/thesis.pdf)
- import features (Scala-like)
- compile-time and runtime reflection
- save/distribute AST (Scala TASTY). Full program optimization/reflection
- important things must be greppable and googlable, call it searchability :)
- compiler as a service: [Language Server Protocol](https://langserver.org/)
- markdown/rst comments/docs, doctest (Julia, Python)
- CPS/Actors/π-calculus/STM?, non-blocking IO, reactive

Example
---

Current implementation has braces and semicolons, but it's planned to change into something like this:

```scala
    Sun = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SolarMass]

    -- global type inference, type ascriptions allowed, types are unified. Here, `i` is Int, return type inferred is [[Float]]
    def go(bodies: [[Float]], i, pxyz: [Float]) =  
        body = bodies[i] -- body is an immutable val by default
        updatedPs = [
            pxyz[0] + body[3] * body[6],
            pxyz[1] + body[4] * body[6],
            pxyz[2] + body[5] * body[6], -- optional commas allowed
        ]
        if i == 0 then updatedPs else go(bodies, i - 1, updatedPs) -- function returns result of last expression
```

Package System
---

Consider [Nix](https://nixos.org/nix/) as package manager

Compiler Modes
----

- Dynamic Mode, aka Prototype Mode.
  Syntax is checked.
  All types are dynamically checked.
- Static Mode.
  Syntax is checked.
  Typechecking/inference, faster execution.
- Hardcore
  Liquid types enabled.  (See Liquid Haskell)
  Proves checked.
  Array bounds checks eliminated.

Type System
---

- Hindley-Milner by default, dependent types if needed
- traits, kind of type classes
- Liquid types as in Liquid Haskell

Memory Management
----

GC, concurrent mark and sweep
per actor/green thread GC
Consider [MultiCore Ocaml GC](http://kcsrk.info/multicore/gc/2017/07/06/multicore-ocaml-gc/)

for now, use [Boehm conservative GC](http://www.hboehm.info/gc/)

Other
---

- indentation significant (i.e. Python, Haskell)
- uppercase Typenames, lowercase idents and type arguments (Haskell/Scala style)
- pattern-matching
- ADT, traits, type classes
- easy C interoperability
- no exceptions (Go/Rust panic style errors)
- don't overuse `'~!@#$%^&* symbols
- fast compilation
- default immutability
- string interpolation: "$ident = ${expression}"
- multiline strings
- Uniform Function Call Syntax (Rust, D).
  For example, any function can be a method for its first argument:
    def toString(s: String)
    "Hello".toString
    def plus(l: Num, r: Num)
    1.plus(2)
    Benefits: 'traditional' syntax, dot-completion, more 'flat', less LISP
- uniform select principle. Use (.) for record field selection, func calls, package name resolution etc
- UTF-8 strings
- Haskell-like application for type functions: Option Int, Either Int String, etc
- Option TypeName as TypeName? sugar (considered)

Install on Mac OS using Homebrew
---

    brew install cmake llvm-hs/llvm/llvm-5.0 boehmgc pcre2
    brew install nau/lasca/lasca-compiler

Setup LASCAPATH environment variable. Add this to your .bash_profile

    export LASCAPATH="$(brew --prefix lasca-compiler)/src"

Try it!

    cat "def main() = println("Hello Lasca!")" > hello.lasca
    lasca -e hello.lasca
    Hello Lasca!
    
Add bash completion config for lasca compiler options:

    lasca --bash-completion-script lasca > $(brew --prefix)/etc/bash_completion.d/lasca    

Build on Mac OS
---

You need LLVM 5.0 installed, and latest Haskell Stack.

    brew install cmake llvm-hs/llvm/llvm-5.0 boehmgc pcre2

    brew install haskell-stack

You need to build Lasca Runtime System library liblascart.a

    make rts

Setup stack, build and install lasca compiler

    stack setup

    stack install

Add your `~/.local/bin` directory to your `$PATH`

Add bash completion config for lasca compiler options:

    lasca --bash-completion-script lasca > $(brew --prefix)/etc/bash_completion.d/lasca

Run hello.lasca

    lasca --exec examples/hello.lasca

If you want to build everything with profiling support
uncomment -rtsopts, -prof, -fprof-auto ghc options in stack.yaml, and run

    stack build --executable-profiling --library-profiling

Build on Ubuntu
---

Requirements: Haskell Stack > 1.6, Cabal > 2.0, LLVM 5, CMake

Don't install Haskell Stack from apt. [It's likely to be older than 1.6 and won't be able to upgrade](https://askubuntu.com/questions/986596/how-to-upgrade-haskell-stack-on-ubuntu-16-04)

Do this instead:

    wget -qO- https://get.haskellstack.org/ | sh

    sudo apt install llvm-5.0-dev libgc-dev zlib1g-dev cmake
    stack build -j 8
    export LASCAPATH=${lasca-compiler-dir}
    stack test

Current n-body run
---

    There are several implementation of n-body problem
    http://benchmarksgame.alioth.debian.org/u64q/nbody.html
    Currently it's quite slow due to boxing.

    $ time lasca -e -O2 examples/nbody.lasca -- 50000000
    -0.169075164
    -0.169059907

    real      7m13.261s
    user      7m39.476s
    sys       0m38.716s

    find src -name "*.hs"  | xargs cat | wc -l
    3714

    find src -name "*.c"  | xargs cat | wc -l
    681

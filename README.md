Lasca Language
==============

[![Build Status](https://travis-ci.org/nau/lasca-compiler.svg?branch=master)](https://travis-ci.org/nau/lasca-compiler)
[![Join the chat at https://gitter.im/lasca-lang/compiler](https://badges.gitter.im/lasca-lang/Lobby.svg)](https://gitter.im/lasca-lang/compiler?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Lasca is Scala shifted towards Haskell.

Lasca is a LLVM-based statically or dynamically typed strict functional programming language. Simplified OCaml if you will.

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
- Go (CSP)
- Erlang (actors, immutability, minimalism)
- Python (docstrings, doctests, syntax)
- Julia
- Swift
- Nim
- Pony
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
- GraalVM backend?
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

Current implementation uses braces and semicolons, but I consider adding indentation-based syntax, or semicolon inference.

```haskell
-- Algebraic data type a la Haskell
data JValue
    = JNull
    | JNum(n: Float)
    | JString(s: String)
    | JBool(v: Bool)
    | JArray(v: [JValue])
    | JObject(v: Map String JValue)

-- function argument type annotations are optional, compiler infers those
def jsonToString(js: JValue) = match js {
    JNull -> "null"
    JNum(n) -> toString(n)
    JBool(v) -> toString(v)
    JArray(v) -> {
        values = Array.map(v, jsonToString);
        toString(values);
    }
    JString(v) -> "\"${v}\""
    JObject(m) -> {
        if Map.isEmpty(m) then "{}" else {
            res = Array.makeArray(m.size, "");
            var idx = 0;
            Map.foreachWithKey(m, { k, v ->
                setIndex(res, idx.readVar, "\"${k}\": ${jsonToString(v)}");
                idx := idx.readVar + 1;
            });
            s = String.join(", ", res);
            "{ ${s} }"
        };
    }
}
```

What Works Right Now
---

- JIT and AOT compilation and execution (via LLVM OrcJIT)
  - lasca -e hello.lasca to execute
  - lasca hello.lasca to create a binary
- type inference
- dynamic typing mode (```lasca -e --mode dynamic hello.lasca```)
- ADTs, inner functions, out of order function definitions
- pattern matching
- calling external C functions
- string interpolation, UTF8 encoded immutable strings
- builtin types: `String`, `Bool`, `Int`, `Byte`, `Int16`, `Int32`, `Float`, `Array`, `ByteArray`, `Var`, `FileHandle`
- implemented `List`, `Option`, `Either`, `Map`, `ArrayBuffer`
- regular expressions with [PCRE-2](https://www.pcre.org/)
- overloaded `+` `-` `*` `/` operators

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
- default immutability
- string interpolation: "${ident} = ${expression}"
- multiline strings
- Uniform Function Call Syntax (Rust, D).
  For example, any function can be a method for its first argument:

```scala
    def toString(s: String) = ...
    "Hello".toString
    def plus(l: Num, r: Num)
    1.plus(2)
```

- uniform select principle. Use (.) for record field selection, func calls, package name resolution etc
- UTF-8 strings
- Haskell-like application for type functions: Option Int, Either Int String, etc

Install on Mac OS using Homebrew
---

    brew install cmake llvm@6 boehmgc pcre2
    brew install nau/lasca/lasca-compiler

Setup LASCAPATH environment variable. Add this to your .bash_profile

    export LASCAPATH="$(brew --prefix lasca-compiler)/src"

Try it!

    echo 'def main() = println("Hello Lasca!")' > hello.lasca
    lasca -e hello.lasca
    > Hello Lasca!

Add bash completion config for lasca compiler options:

    lasca --bash-completion-script lasca > $(brew --prefix)/etc/bash_completion.d/lasca

Build on Mac OS
---

You need LLVM 6.0 installed, and latest Haskell Stack.

    brew install cmake llvm@6 boehmgc pcre2

    brew install haskell-stack

    stack setup

Setup LASCAPATH environment variable. Add this to your .bash_profile

    export LASCAPATH="${lasca-compiler-src-dir}/libs/base"

Add your `~/.local/bin` directory to your `PATH`

    export PATH=$PATH:~/.local/bin

Build and install lasca compiler

    make install

Add bash completion config for lasca compiler options:

    lasca --bash-completion-script lasca > $(brew --prefix)/etc/bash_completion.d/lasca

Run hello.lasca

    lasca --exec examples/hello.lasca

Build on Ubuntu
---

Requirements: Haskell Stack > 1.6, Cabal > 2.0, LLVM 6, CMake

Don't install Haskell Stack from apt. [It's likely to be older than 1.6 and won't be able to upgrade](https://askubuntu.com/questions/986596/how-to-upgrade-haskell-stack-on-ubuntu-16-04)

Do this instead:

    curl -sSL https://get.haskellstack.org/ | sh

    sudo apt install llvm-6.0-dev libgc-dev zlib1g-dev cmake
    sudo add-apt-repository universe
    sudo apt install libpcre2-dev
    export LASCAPATH="${lasca-compiler-src-dir}/libs/base"
    export PATH=$PATH:~/.local/bin
    stack setup
    make install
    lasca -e examples/hello.lasca

Current n-body run
---

There are several implementation of [n-body problem](
http://benchmarksgame.alioth.debian.org/u64q/nbody.html)
Currently it's quite slow due to boxing.

    $ time lasca -e -O2 examples/nbody.lasca -- 50000000
    -0.169075164
    -0.169059907

    real      7m13.261s
    user      7m39.476s
    sys       0m38.716s

    find src -name "*.hs"  | xargs cat | wc -l
    4738

    cat rts/runtime.c rts/builtin.c rts/lasca.h | wc -l
    1324

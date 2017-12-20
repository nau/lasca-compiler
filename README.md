Lasca Language
==============

[![Join the chat at https://gitter.im/lasca-lang/compiler](https://badges.gitter.im/lasca-lang/Lobby.svg)](https://gitter.im/lasca-lang/compiler?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Lasca is Scala shifted towards Haskell. 

Lasca is a LLVM-based statically typed general purpose programming language.

It has a 'dynamic' compilation mode, meaning instant code generation without compile time type checking/inference, 
allowing instant compilation/execution cycle, like dynamic languages give.
It's planned to have a full type inference, parametric polymorphism, and sort of type classes.
 
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
- OCaml/SML/1ML
- Clojure (persisted data structures, HAMT/CHAMP)
- Go (simplicity, speed, Any interface?, all-in-one compiler)
- Rust (linear types, borrowing, method syntax)
- Erlang (actors, immutability, simplicity, distributed)
- Python (docstrings, doctests, syntax)
- Julia
- Swift
- Nim
- Pony (ref caps, behaviours, actors)
- Koka (https://github.com/koka-lang/koka) (algebraic effects)
 

Ideas
---
- light, non-symbol-polluted syntax (Python)
- Uniqueness type (inplace write, no gc)
  See Rust, Pony, Idris Unique type, Linear Haskell
  http://lampwww.epfl.ch/~phaller/doc/capabilities-uniqueness2.pdf  
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
- Gradual Typing (http://homes.soic.indiana.edu/jsiek/what-is-gradual-typing/)
- Deferred Type Errors (runtime compilation mode, Haskell)
- Linear/affine types (Rust, Linear Haskell)?
- Liquid Type system (refinement types, Leon, Liquid Haskell) 
  https://github.com/pleiad/Refinements
  http://leon.epfl.ch
  https://github.com/ucsd-progsys/liquidhaskell
  Z3/CVC4 as proof assistant.
- Algebraic Subtyping for module system? (https://www.cl.cam.ac.uk/~sd601/thesis.pdf)   
- import features (Scala-like)
- compile-time and runtime reflection
- save/distribute AST (Scala TASTY). Full program optimization/reflection
- important things must be greppable and googlable, call it searchability :)
- compiler as a service: Language Server Protocol (https://langserver.org/)
- markdown/rst comments/docs, doctest (Julia, Python)
- CPS/Actors/π-calculus/STM?, non-blocking IO, reactive
- import of a package must not introduce any side effects?! (hello Go)

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
Consider Nix as package manager (https://nixos.org/nix/)

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

  
Dev Cycle
---
1. Prototyping
  Dynamic mode.
  Compiler warnings disabled.
  Types are inferred as much as possible, compile-time type errors are ignored.
  Whatever can be executed is executed, type errors are thrown at runtime.
2. This should work now
  Normal Mode. 
  Warnings are enabled.
  IDE allows you to automatically fill type annotations, leaving '?'-holes where it's uncertain.
  Warning if no test found for a function?
3. The shit is done (CI mode)
  Hardcore mode.
  IDE suggests @pure, @total, @nothrow, IO annotations
4. Want performance
  Hardcore mode
  IDE suggests places to help escape analysis etc.

Type System
---
- Hindley-Milner by default, System F(w) if needed
- traits, kind of type classes
- Liquid types as in Liquid Haskell

Memory Management
----
GC, concurrent mark and sweep
per actor/green thread GC
Consider [MultiCore Ocaml GC](http://kcsrk.info/multicore/gc/2017/07/06/multicore-ocaml-gc/)

for now, use [Boehm conservative GC](http://www.hboehm.info/gc/)

Syntax
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
================================

    brew install nau/lasca/lasca-compiler
    
Setup LASCA_HOME environment variable. Add this to your .bash_profile    
    
    export LASCA_HOME=$(brew --prefix lasca-compiler)
    
Try it!

    cat "def main() = println("Hello Lasca!")" > hello.lasca
    lasca -e hello.lasca
    Hello Lasca!
        
    
Build on Mac OS
===============
You need LLVM 5.0 installed, and latest Haskell Stack.

    brew install llvm-hs/llvm/llvm-5.0 
    
    brew install boehmgc

    brew install haskell-stack
    
You need to build Lasca Runtime System library liblascart.so

    make rts
    
Setup stack, build and install lasca compiler    
    
    stack setup
    
    stack install

Add your `~/.local/bin` directory to your `$PATH`

Add bash completion config for lasca compiler options:

    lasca --bash-completion-script lasca > $(brew --prefix)/etc/bash_completion.d/lasca
    
Run hello.lasca
     
    lasca --exec src/main/lasca/hello.lasca
    
If you want to build everything with profiling support
uncomment -rtsopts, -prof, -fprof-auto ghc options in stack.yaml, and run
    
    stack build --executable-profiling --library-profiling
   
    
Current n-body run:
--------------------
    There are several implementation of n-body problem
    http://benchmarksgame.alioth.debian.org/u64q/nbody.html
    Currently it's quite slow due to boxing.

    $ time lasca -e -O2 src/main/lasca/nbody.lasca -- 50000000
    -0.169075164
    -0.169059907

    real      7m13.261s
    user      7m39.476s
    sys       0m38.716s

    find src -name "*.hs"  | xargs cat | wc -l
    3714
    
    find src -name "*.c"  | xargs cat | wc -l
    681

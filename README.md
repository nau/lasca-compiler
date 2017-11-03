Lasca Language
==============

[![Join the chat at https://gitter.im/lasca-lang/compiler](https://badges.gitter.im/lasca-lang/Lobby.svg)](https://gitter.im/lasca-lang/compiler?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Lasca is Scala shifted towards Haskell. 

Lasca is a LLVM-based statically typed general purpose programming language.

It has a 'dynamic' compilation mode, meaning instant code generation without compile time type checking/inference, 
allowing instant compilation/execution cycle, like dynamic languages give.
It's planned to have a full type inference, parametric polymorphism, and sort of type classes.
 
Imagine
- Scala with type classes, fast compilation/start time, and optional dynamic typing
- Go with ADTs, global type inference, and parametric polymorphism 
- Haskell with decent records syntax, runtime polymorphism, and string interpolation
- OCaml with typeclasses, overloaded +-*/ for ints and floats, and do-notation
- Rust with garbage collector without <::>!?
- Erlang with types and fast execution
- Python with multithreading, pattern matching, and multiline lambdas

Inspired by:
- Scala
- Haskell, Liquid Haskell, Linear Haskell
- OCaml/SML/1ML
- Clojure (persisted data structures, HAMT/CHAMP)
- Idris/Agda (dependent types?)
- Go (simplicity, speed, Any interface?, all-in-one compiler)
- Rust (linear types, borrowing, method syntax)
- Erlang (actors, immutability, simplicity, distributed)
- Python (docstrings, doctests, syntax)
- Swift
- Pony (ref caps, behaviours)
- Julia
- Koka (https://github.com/koka-lang/koka)
- Whiley (http://whiley.org/about/overview/)
- Sage, Unison, Cloud Haskell
 

Ideas
---
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
- type-level immutability?
- var are local by default (investigate if usable)
- prefer things done one way
- make it host language: allow simple creation of external DSLs, configs, etc via compiler API ??
- LLVM backend
- JVM backend? (hope not)
- JavaScript backend via compiler API
- no OOP and data subclassing/inheritance?
- syntactic sugar is ok
- no null
- annotations (Java/Python-style)
- annotation-based extensions, like visibility (@private, public by default)
- macros based metaprogramming (Scala Meta, Template Haskell)    
- macro-based extensions?
- implicits? (Scala/Haskell/Idris)
- implicit macro? (Scala). No, if possible.  
- import features (Scala-like)
- compile-time and runtime reflection
- totality? (Idris)
- mixfix syntax? (Agda). No.
- save/distribute AST (Scala TASTY). Full program optimization/reflection
- ABI?
- important things must be greppable and googlable, call it searchability :)
- prefer offensive programming style
- compiler as a service (like Scala sbt)
- markdown/rst comments/docs, doctest (Julia, Python)
- CPS/Actors/π-calculus/STM?, non-blocking IO, reactive
- import of a package must not introduce any side effects?! (hello Go)
- JSON ready, included
- libuv for async I/O?
- https://www.youtube.com/watch?v=buQNgW-voAg (future functional programming language)
- blockchain based storage of proven software?

Example
---
```scala
    Sun = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SolarMass]
    
    -- global type inference, type ascriptions allowed, types are unified. Here, `i` is Int, return type inferred is [[Float]]
    def go(bodies: [[Float]], i, pxyz: [Float]) = { 
        body = bodies[i]; -- body is an immutable val by default
        updatedPs = [
            pxyz[0] + body[3] * body[6],
            pxyz[1] + body[4] * body[6],
            pxyz[2] + body[5] * body[6]
        ];
        if i == 0 then updatedPs else go(bodies, i - 1, updatedPs); -- function returns result of last expression
    }
```

Package System
---
Consider IPFS or alike
P2P systems, bittorrent for package sharing
Basically, package hosing site is a torrent tracker.
Distributed blockchain-based storage of proven software libraries

Practical things that matter
----
- immutability as concurrently shared data
- https://www.infoq.com/presentations/Value-Identity-State-Rich-Hickey
- Persisted Data Structures!
- performance! (mutable non-shared state?, mutable collections, in-place updates for unique values)
- consider language support for StateT monad.
- sized collections in some cases
- pure functions for concurrency and caching
- readability
- simplicity
- familiarity/intuitiveness
- principle of least surprise
- caches/memoization out of the box (annotations?)

Performance (need research)
---
- mutable non-shared state
- mutable collections + convertions to immutable
- specialization
- inlining
- escape analysis?
- tail-call optimization (LLVM)
- stack allocation
- typesafe move semantics? borrowing etc? (Rust)
- typesafe array bounds check (runtime checks elimination)
- vectorization stack ghci liquidhaskell (CUDA, OpenCL, CPU (MMX, SSE))

Compiler Modes
----
- FuckIt Mode. Prototype Mode. Refactoring Mode. Bash Mode.
  Program executes even with syntax errors. 
  Whatever is parsed - gets executed.
  All types are dynamic. Call whatever you like.
- Dynamic Mode.
  Syntax is checked.
  All types are dynamic. Call whatever you like.
- Normal Mode.
  Syntax is checked.
  Typechecking/inference.
- Hardcore
  Dependent types/liquid types enabled.  (See Liquid Haskell)
  Totality checked. 
  Proves checked.

  
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

for now, use Boehm conservative GC

Evaluation
---
Strict, applicative order of evaluation, 
lazy keyword for normal order

lazy val a = readDb()

def Or(lhs: Expr, lazy rhs: Expr)
            
Immutability
---
A value is immutable by default

Mutable things must require more things to do than immutable for people to prefer immutable.

Below are raw ideas. Could be bullshit.

Consider embedding Lens/StateT monad to simplify working with immutable structures and state.

Consider
https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type

Concurrency
---
- Future/Promise/async/await
- STM?
- CPS (Go, Clojure core.async) or Actors (Erlang, Akka)?

Syntax
---
- uppercase Typenames, lowercase idents and type arguments (Haskell)
- pattern-matching
- no OOP
- ADT, traits, type classes
- easy C interop (
- no exceptions?
- sane syntax 
  avoid garbage-symbols pollution, 
  no semicolons, 
  no braces, 
  no <>, 
  less (), 
  less [],:`'~!@#$%^&*
  no _ in ident names
- strict formatting rules
- fast compilation
- preferred immutability (immutable refs?, views?)
- string interpolation: "$ident = ${expression}"
- multiline strings
- IDE-friendly!
- Uniform Function Call Syntax (Rust, D).
  For example, any function can be a method for its first arguement:
    def toString(s: String)
    "Hello".toString
    def plus(l: Num, r: Num)
    1.plus(2)
    Benefits: 'traditional' syntax, dot-completion, more 'flat', less LISP
- uniform select principle. Use (.) for record field selection, func calls, package name resolution etc
- A&B, A|B types?
- no overloading/overriding
- UTF-8 strings
- Haskell-like application for type functions: Option Int, Either Int String, etc
- Option TypeName as TypeName? sugar   
   

Orthogonal type systems
=====
Consider having several orthogonal type systems.

One is a standard HM/System F(w)

Another for side effect tracking. Consider annotations or inside comments annotations
 
```scala
	@ pure, total 
	def toString(a): String = "a"
	@  pure
	def head(l: List a): a = {
	  require(l.size > 0) -- compile time proof
	  match l {
	    Cons(v) -> v
	    Nil -> panic("Empty list")
	  }
	}
  @ String -> IO ()
	def println(s: String): Unit
``` 

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
You need LLVM 4.0 installed, and latest Haskell Stack.

    brew install llvm-hs/homebrew-llvm/llvm-4.0
    
    brew install boehmgc

    brew install haskell-stack
    
Update extra-lib-dirs in stack.yaml, add absolute path to a lasca-compiler directory. 
ghc will search there for liblascart.so
    
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
    $ time lasca -e -O2 src/main/lasca/nbody.lasca -- 50000000
    -0.169075164
    -0.169059907

    real      7m13.261s
    user      7m39.476s
    sys       0m38.716s

    find src -name *.hs  | xargs cat | wc -l
    3714
    
    find src -name *.c  | xargs cat | wc -l
    681

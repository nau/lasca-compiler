Lasca Language
==============

Lasca is Scala shifted towards Haskell. 
Lasca is a LLVM-based statically typed general purpose programming language.
It has a 'dynamic' compilation mode, meaning instant code generation without compile time type checking/inference, 
allowing instant compilation/execution cycle, like dynamic languages give. 
 
Imagine
- Scala with type classes, fast compilation/startup time, without OOP, inheritance, variance, implicits, and other complexities
- Go with ADTs, global type inference and parametric polymorphism 
- Haskell that is strict and has more 'traditional' syntax and semantics, normal String, and string interpolation
- OCaml with overloaded +-*/ for ints and floats, type classes 
- Rust with garbage collector and without all those scopes and <>::!?
- Erlang with types and fast execution
- Python with multiline lambdas
- IntelliJ Idea support :)

Inspired by:
- Scala
- Haskell, Liquid Haskell
- OCaml/SML
- Closure (persisted data structures)
- Idris/Agda (dependent types?)
- Go (simplicity, speed, Any interface?, all-in-one compiler)
- Rust (borrowing?, method syntax)
- Erlang (actors, immutability, simplicity, distributed)
- Python (docstrings, doctests)
- Swift (???)
- D (unified method syntax, macros?) 
- Pony (ref caps, behaviours)
- Julia
- Whiley (http://whiley.org/about/overview/)
- Sage
- Unison
- Cloud Haskell
- Oczor

Motivation
---

You write code once and read it hundred times. 
Hence, readability and simplicity but expressivity and conciseness is the essence.
One of goals is to speed-up and partition a usual development cycle:

 change -> get result
 
 prototype -> get fast result -> improve code -> test -> produce optimized program

Compilation time matters. A lot. 
 
To speed-up prototyping I suggest disable/simplify typechecking during prototyping.
This can be done by compiler option with per source file, or even per definition granularity
See Haskell Deffered type checking.

    lasca --mode dynamic hello.lasca // compile in dynamic typing mode
    
Right balance between expressiveness and readability is the essence.

IDE support is very important. Lasca will have IntelliJ Idea support from early stages.
 

Ideas
---
- Concurrency Oriented Programming (Erlang). Objects are out. Concurrency is in.
- Gradual Typing (http://homes.soic.indiana.edu/jsiek/what-is-gradual-typing/)
- Deferred Type Errors (runtime compilation mode, Haskell)
- Linear types (Rust)?
- Liquid Type system (refinement types) + Dependent Type system? 
  https://github.com/pleiad/Refinements
  
  Consider optional refinement typing. Z3 (commercial license?), CVC4
  
- light, non-symbol-polluted syntax
- Uniqueness type (inplace write, no gc)
  See Pony, Idris Unique type
  http://lampwww.epfl.ch/~phaller/doc/capabilities-uniqueness2.pdf  
- indentation-based? 
	no, curly braces: merge conflicts!, reformat, easier to copy-paste from SO 
- readability first
- fast development cycle
- presentation compiler (JSON compiler API?)
- IDE-friendly ('dot-autocomplete', auto-formatting, compiler API) 
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
- annotations (Java-style)
- annotation-based extensions, like visibility (@private, public by default)
	- nope
    - Consider private by default, bc adding public function to a package may require full source recompilation. If it is.
- macro-based extensions?
- implicits? (Scala/Haskell/Idris)
- implicit macro? (Scala). No, if possible.  
- import features (Scala-like)
- compile-time and runtime reflection
- theorem prover? (Idris)
- dependent types? (Idris)
- totality? (Idris)
- mixfix syntax? (Agda)
- save/distribute AST (Scala TASTY). Full program optimization/reflection
- ABI?
- important things must be greppable and googlable, call it searchability :)
- prefer offensive programming style
- compiler as a service (like Scala sbt)
- markdown/rst comments/docs, doctest (Julia, Python)
- CPS/Actors/π-calculus/STM?, non-blocking IO, reactive
- import of a package must not introduce any side effects?! (hello Go)
- JSON ready, included
- grades for code by the compiler (A+, A, B, C, D, E, F). From untyped undocumented F-code, to proven, documented with examples A+ code
- libuv for async I/O?
- https://www.youtube.com/watch?v=buQNgW-voAg (future functional programming language)
- blockchain based storage of proven software?


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
- sized collections in some cases
- pure functions for concurrency and caching
- readability
- simplicity
- familiarity/intuitiveness
- principle of least surprise
- caches/memoization out of the box (annotations?)

Performance (need research)
---
- specialization
- inlining
- escape analysis?
- tail-call optimization? (LLVM?)
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
@lazy annotation/keyword or (Lazy expr) type for normal order

@lazy val a = readDb()

val a = Lazy readDb()
            
Immutability
---
A value is immutable by default

Mutable things must require more things to do than immutable for people to prefer immutable.

Mutable value can be seen as immutable snapshot (CoW?)

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
- easy C interop
- no exceptions?
- sane syntax 
  avoid garbage-symbols pollution, 
  no semicolons, 
  no braces, 
  no <>, 
  less (), 
  less ','
  no _ in ident names
- strict formatting rules
- fast compilation
- preferred immutability (immutable refs?, views?)
- string interpolation: "$ident = ${expression}"
- multiline strings
- IDE-friendly!
- Uniform Function Call Syntax (Rust, D)
- uniform select principle. Use (.) for func calls, package name resolution etc
- A&B, A|B types?
- no overloading/overriding
- UTF-8 strings
- Option TypeName as TypeName? sugar
- slices are stupid bullshit. 
    Just fucking use normal take/head/init/last/slice methods. 
    There is zero point to make this a language feature.
- no language-embedded data structures    
   

Orthogonal type systems
=====
Consider having several orthogonal type systems.

One is a standard HM/System F(w)

Another for side effect tracking. Consider annotations or inside comments annotations
 
Another for, say, O-complexity tracking.
 
```scala
	--@ O(1), pure, total 
	def toString(a): String =
	--@ O(1), pure
	def head(l: List a): a = {
	  require(l.size > 0) -- compile time proof
	  match l {
	  | Cons(v) -> v
	  }
	}
	-- @ String -> IO ()
	def println(s: String): Unit
``` 

Build
===
You need LLVM 3.5.2 installed, and Haskell Stack.

    brew install haskell-stack
    
    stack setup
    
    stack install

    brew install homebrew/versions/llvm35

    cabal install llvm-general -fshared-llvm

    stack install
    
You need to build Lasca Runtime System library liblascart.so

    make rts
    
First time n-body run:
----------------------

    $ time lasca --mode static src/main/lasca/nbody.lasca --exec --print-llvm -g 2

    Parsed OK
    Compiler mode is static
    typechecked OK
    Read module OK
    Running JIT
    ; ModuleID = 'src/main/lasca/nbody.lasca'
    
    Init Lasca 0.0.0.1 runtime. Enjoy :)
    -0.169075164
    -0.169059907
    
    real	26m3.883s
    user	28m14.192s
    sys	8m36.289s
    
    $ find src/main/haskell -name *.hs  | xargs cat | wc -l
        1850
    
Current time n-body run:
--------------------
    Init Lasca 0.0.0.1 runtime. Enjoy :)
    -0.169075164
    -0.169059907
    
    real	13m33.031s
    user	14m26.219s
    sys	3m48.577s

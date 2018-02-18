==========================
Lasca Programming Language
==========================
.. class:: center

Lasca Programming Language Design Draft v0.0.1

Simple yet powerful modern functional programming language.


.. contents::

Overview
========

- strict
- functional
- expression based
- ADT and pattern matching
- no null
- strongly statically typed with type inference, and dynamic modes
- System F with optional liquid types (`Liquid Haskell`_)
- type inference (Hindley-Milner alike)
- type classes (Haskell), or implicit instances (Idris)
- syntactic sugar for function application (named arguments, defaults, method calls), lenses, records, indexed access,
  applicatives and monads (do-notation).
- configurable type checking passes (types, side-effects/purity checks, totality checks)
- immutable data by default
- persisted data structures (see [1]_, [2]_)
- message passing concurrency (see Erlang_ actors, Akka_) (or CPS like in Go?)
- batteries included (json, lenses, collections, vars, actors)
- LLVM_ backend
- JS backend
- REPL


Inspired by:

- Scala/Dotty
- Haskell, Liquid Haskell
- OCaml/SML/F#/1ML
- Clojure (persisted data structures)
- Idris/Agda (dependent types?)
- Go (simplicity, speed, Any interface?, all-in-one compiler)
- Rust (linear types, borrowing, method syntax)
- Erlang (actors, immutability, simplicity, distributed)
- Python (docstrings, doctests)
- Swift
- D (unified method syntax, macros)
- Pony_ (ref caps, behaviours)
- Julia
- Koka (https://github.com/koka-lang/koka)
- Whiley (http://whiley.org/about/overview/)
- Sage, Unison, Cloud Haskell

Domain
======

* Highly concurrent applications (P2P, blockchain)
* Distributed server side applications development
* Machine learning tasks
* Web development

Goals – substitute Go, Erlang, Java/Scala/Scala.js, Python, Julia and JavaScript/Node.js for server-side development.


Platforms And OS
================

To simplify and speed-up development I suggest to concentrate on the most wide spread systems.
Mainly x86-64 architecture, later ARM

Operating Systems: MacOS X, Linux (Ubuntu LTS, NixOS), POSIX-compatible.



Motivation
==========

You write code once and read it hundred times.

Hence, readability and simplicity but expressivity and conciseness is the essence.
One of the goal is to speed-up and partition a usual development cycle:

	change -> get result

	prototype -> get fast result -> improve code -> test -> produce optimized program

Compilation time matters. A lot.

To speed-up prototyping I suggest simplify disable/simplify typechecking during prototyping.
This can be done by compiler option with per source file, or even per definition granularity.

See `Haskell Defered type checking`_.

Future of Programming by Uncle Bob Martin
https://www.youtube.com/watch?v=ecIWPzGEbFc

Resume
- Lisp for the rescue



https://www.quorumlanguage.com/evidence.html

The Programming Language Wars
https://www.youtube.com/watch?v=bvtD8Bg8Dv0

http://web.cs.unlv.edu/stefika/research.html

Resume
- static typing is generally better
- documentations matters
- ide doesn't matter o_O

https://www.quorumlanguage.com/evidence.html

http://dl.acm.org/citation.cfm?id=2962592

TDD is bullshit

Defaults
========

One of the most important thing in almost every aspect of our life are defaults.
What's default that's going to be choosen in most cases, unless it's reasonably bad for you.
There is a psycological aspect of not willing to choose if a reasonable default is present.
I claim it's an energy conservation mechanism at work :)

Most people use default settings in most software they use.
And that's one of the things that make Apple so great – they choose reasonable defaults for you.

Right defaults are those, that are (or should be) used most of the time.
For a modern functional programming modern language intended for server-side application development

I consider the following to be good defaults:
- strict for both structures and values
- statically typed
- effectful
- immutable
- green threads
- no type annotations required (global type inference)
- macros (hygienic is possible)
- have a coding style, but don't force it with fmt-like tools.
People tend to follow the rules, but go crazy when those are inavoidably forced. 
No fmt tool know the reason why your formatting in this place is right.
There should be one official standard coding style (see Python PEP 8, Scala Coding Style)

And syntactically, I find these to be important:

- avoid visual noise (reduce usage of special symbols #<>~, semicolons, braces etc)
- visually appealing (reduce ugly symbols, like !#<>~) 
- indentation significant (much cleaner looks)
- allow adding operators but don't overuse them (i.g. hide the feature under a compiler flag, 
force an operator to be a bridge to a normal function with an annotation or something)
- support named arguments calls
- do-notation

Hence, I think most programming languages have wrong defaults in their design.

Haskell: laziness, purity, easiness of adding a symbolic operator
OCaml: bad syntax
Go: mutable variables, global mutable state, compile error on unused imports (sic!), no generics
Python: mutable variables, dynamic, not-so-strong type system
JavaScript: mutable variables, dynamic wat?-weak type system
F#: .NET

Although laziness and purity are very nice and usefull features, they should not be _defaults_.
There is a huge space of small effectfull programms (basically all scripts) that would benefit from
clean statically typed ML-like language with type inference.


Even Haskell people seem to agree that laziness was a poor decision. 
You almost never need lazy data structures, and oh my god, how often you actually need a strict function argument.

Scala and F# are almost there. But I'd rather have something much simplier, and not Microsoft/Oracle bound.

Human perception driven approach for a programming language syntax design
-------------------------------------------------------------------------

Lambda-calculus, and Haskell are great for a compiler. Not so much for a human.
People are not computers. Yet, at least.

We must consider human perceptive characteristic designing the language syntax.

For example, at least for me, it's very important to get some working result as quick as possible.
That means I would rather not wait for compilation/type checking/tests/system startup.
Hence, either all of that happen very quickly, or we need to postpone/disable some of that.

We can postpone compilation/typechecking, run in interpreter mode and do jitting.
We can do a gradual typecheck, run in interpreter mode and do jitting.

When things are getting... TODO

Program Lifecycle
-----------------

Prototyping
   - Require fastest change-run cycles, it's very important!
   - Don't care about types, compilation errors in other packages.
   - If it can be run, it should be run. (JavaScript mode :)

Settlement
   - Things are getting cleaner, APIs can be seen and refactored.
   - Here we need a typechecker. Mostly in IDE, suggesting things.
   - You can define and polish your tests.

Production
   - All API have a valid documentation with examples and tests.

Continuous Integration
   - Builds are made with all type/style checks, tests runs, and optimizations enabled.

Critical Software
   - Refinement types proofs, Effects proofs.

Maintenance
   - Navigation support is crucial.
   - Readability is crucial.
   - Avoid complex concepts: HKT, implicits, operators.

Refactoring
   - IDE support!


Entropy
-------

- Entropy must be in "perceptive" range.
- Not too much of duplication.
- Not too much of entropy

Too much entropy:

.. code:: haskell

	foldl :: (a -> b -> a) -> a -> [b] -> a
	foldl f z []     = z
	foldl f z (x:xs) = foldl f (f z x) xs

Better:

.. code:: scala

	def foldLeft[A, B](col: List[A], z: B, f: (B, A) => B): B =
	  (col, z, f) match {
		case (Nil, z, f) => z
		case (x :: xs, z, f) => xs.foldLeft(z, f(z, x))
	  }

Optimal?

.. code:: scala

	def foldl(col: Seq a, zero: z, f: z -> a -> z): a = match
	  [] zero _        -> zero
	  (x :: xs) zero f -> xs.foldl zero (f zero x)
	end

Vision
======

You get a multipurpose modern programming language.

When you need to write a shell script or quickly prototype an idea – use gradual typing mode and interpretation.

When you need speed – compile before use, types are inferred

When you need speed and correctness – compile and validate your liquid types with CVC4/Z3 solvers.


Type System
===========

Goals
-----

What type systems are for

#. Find type errors at compile time (can't pass Int where String is expected)
#. Help IDE with suggestions
#. Optimal code generation:

	- memory alignment
	- on stack allocation
	- life-time tracking with linear types
	- array bounds checks elimination
	- other runtime checks elimination
	- vectorization

#. Correctness
#. Side effects control

	- why? what benefits?

Our goal is to get the best from type system while not making it too complex and intrusive.

Switchable gradual/static typing. Both are strong.

System F with Liquid Types. (see `Liquid Haskell`_, Leon_)

Type classes (Haskell or Idris like implicits).

Discussion
~~~~~~~~~~

- `Edward Kmett - Type Classes vs. the World <https://www.youtube.com/watch?v=hIZxTQP1ifo>`_
- `Scrap your type classes <http://www.haskellforall.com/2012/05/scrap-your-type-classes.html>`_

Thoughts on Subtyping
~~~~~~~~~~~~~~~~~~~~~

Implementing data Subtyping implies

- variance
- complex typer
- protected visibility
- least upper bound for if/case

Consider not having subclassing at all.

Will have type classes subtyping, and liquid types subtyping.

Consider Algebraic Subtyping https://www.cl.cam.ac.uk/~sd601/thesis.pdf


.. include:: Syntax.rst


Package System
==============

Packages are compressed serialized Lasca AST trees.
This allows target machine compilation/interpretation with whole program optimizations.
And it's cross-platform representations.

Packages are transferred via bittorrent or alike P2P service.
https://lasca.io would be kind of a torrent tracker site.

Collections
===========

Immutable
---------

Efficient Immutable Collections
https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf

CHAMP for Map/Set
Vector (AMT)

Mutable
-------

Not sure how to call it better. I'd like to have kind of ndarray built in.
Array it just a one dimensional tensor/ndarray.

Tensor (Numpy ndarray)
----------------------
type Array a = Tensor a
type Ti32 = Tensor Int32
type Ti64 = Tensor Int64
type Tf32 = Tensor Float32
type Tf64 = Tensor Float64

Memory Management
=================

For starter, just use Boehm_ conservative gc.

Interesting Statistics
----------------------

The DaCapo Benchmarks: Java Benchmarking Development and
Analysis (Extended Version)
http://www.dacapobench.org/dacapo-TR-CS-06-01.pdf

Most objects less than 128 bytes.

Less than 10% objects survive a single collection (Generational Hypothesis).

Mean Object Size
===  ====== ====== ===========
     Alloc  Live   Survival, %
===  ====== ====== ===========
Min  22     25     1.1
Max  28,031 24,425 50.5
Avg  77     110    8.7
===  ====== ====== ===========

Local Heaps
-----------

Per actor stack and heap. GC when actor is waiting. (See Erlang_, Pony_)

These 3 papers share similar design decisions:

Garbage Collection for Multicore NUMA Machines
http://manticore.cs.uchicago.edu/papers/mspc11-numagc.pdf

Multicore OCaml GC
https://speakerdeck.com/kayceesrk/multicore-ocaml-gc

Multicore Garbage Collection with Local Heaps
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/local-gc.pdf

Brief Conclusion:

"Our scaling results are not as dramatic as we had hoped when
embarking on this line of research, and if we consider parallel
throughput alone, it is not clear whether the improvements are
worth the (substantial) increase in complexity imposed by the local heap
collector over a stop-the-world implementation."

So, as far as I understand, all the complexity of managing local heaps is not worth it
in case of mutable data. So I'm thinking of just using Immix.

Immix
http://ts.data61.csiro.au/publications/nictaabstracts/Lin_BHN_16.abstract.pml
https://gitlab.anu.edu.au/mu/immix-rust

Summarizing Garbage Collection
https://eschew.wordpress.com/2016/09/02/summarizing-gc/

Data Structure Aware Garbage Collector
http://www.cs.technion.ac.il/~erez/Papers/dsa-ismm-15.pdf

Bounding Data Races in Space and Time
http://kcsrk.info/papers/pldi18-memory.pdf

Concurrency
===========

Ideas
-----

- Actors or CPS.

- Async/await

- Future/Promise

I think Actors/Erlang is better choice for further distributed application.
See Erlang/OTP Scala/Akka for best practices.

Reactiveness
============

Fully asynchronous APIs.

Consider libuv, see Julia.

FFI (Foreign Functions Interface)
=================================

Must be as straightforward and simple as possible.
See `Pony FFI <https://tutorial.ponylang.org/c-ffi/calling-c.html>`_
or `Rust FFI <https://doc.rust-lang.org/book/ffi.html>`_.

Exception Handling
==================

Go style ``panic``/``recover`` ??
Don't see how it's different from try/catch/finally.

See:
* https://blog.golang.org/defer-panic-and-recover
* https://dave.cheney.net/2012/01/18/why-go-gets-exceptions-right
* http://stackoverflow.com/questions/3413389/panic-recover-in-go-v-s-try-catch-in-other-languages

Thoughts About OOP
==================
Pros:

OOP gives subtyping, inheritance, and dynamic dispatch.
It can work as a decent module system (Scala, 1ML)

Cons:

Subtyping implies all sorts of problems: least upper bound search for type inference,
variance complexities (variance annotations, covariant/contravariant positions),
conforming to Liskov Substitutions Principle.

I suggest getting benefits and avoiding problems by allowing subtyping only for operations, in form of Type Classes.
No data subtyping/inheritance. Consider Go-like embedding.

Side Effects
============

http://softwareengineering.stackexchange.com/questions/15269/why-are-side-effects-considered-evil-in-functional-programming

Writing your functions/methods without side effects - so they're pure functions -
makes it easier to reason about the correctness of your program.

It also makes it easy to compose those functions to create new behaviour.

It also makes certain optimisations possible,
where the compiler can for instance memoize the results of functions, or use Common Subexpression Elimination.

Edit: at Benjol's request:
Because a lot of your state's stored in the stack
(data flow, not control flow, as Jonas has called it here),
you can parallelise or otherwise reorder the execution of those parts of your computation
that are independent of each other.
You can easily find those independent parts because one part doesn't provide inputs to the other.

In environments with debuggers that let you roll back the stack and resume computing (like Smalltalk),
having pure functions means that you can very easily see how a value changes,
because the previous states are available for inspection.
In a mutation-heavy calculation, unless you explicitly add do/undo actions to your structure or algorithm,
you cannot see the history of the computation.
(This ties back to the first paragraph: writing pure functions makes it easier to inspect the correctness of your program.)

I'm not buying these arguments, though.
In practise, even the most mature FP languages don't have anything like that implemented in a usable form.


Useful Links and Papers
=======================
A Critical Analysis of String APIs https://arxiv.org/pdf/1711.10713.pdf
Fixing Faults in C and Java Source Code: Abbreviated vs. Full-word
Identifier Names http://www2.unibas.it/gscanniello/Giuseppe_Scanniello%40unibas/Home_files/TOSEM.pdf
Fast 64-bit integers for Scala.js http://lampwww.epfl.ch/~doeraene/presentations/jslongs-vmm2017/


Optimizations
=============
# Common Subexpression Elimination

Looks like LLVM does it with Global Value Numbering

# Automatic Memoization

Annotations? Caches?

.. code:: scala
    @cache(type=[LRU/LFU], size=100)
    def calculate(∀ a. Num a => x: a, y: a, z: a): a = x * y * z
    def calculate(∀ a. Num a => x: a, y: a, z: a): a = x * y * z

Support Standards/Specifications
================================

- Support the XDG Base Directory Specification


.. include:: LangsOverview.rst

.. _Liquid Haskell: https://github.com/ucsd-progsys/liquidhaskell
.. _LLVM: http://llvm.org/
.. _Haskell Defered type checking: https://ghc.haskell.org/trac/ghc/wiki/DeferErrorsToRuntime
.. _Akka: http://akka.io
.. _Erlang: https://www.erlang.org
.. _Boehm: http://www.hboehm.info/gc/
.. _Pony: https://www.ponylang.org
.. _Leon: https://github.com/epfl-lara/leon
.. [1] https://en.wikipedia.org/wiki/Persistent_data_structure
.. [2] https://www.infoq.com/presentations/Value-Identity-State-Rich-Hickey

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
- practical
- expression based
- pattern matching
- no null
- strongly statically typed with type inference, and dynamic modes
- System F with liquid types (`Liquid Haskell`_)
- type inference (Hindley-Milner alike)
- type o (Haskell), or implicit instances (Idris)
- syntactic sugar for function application (named arguments, defaults, method calls), lenses, records, indexed access,
  applicatives and monads (do-notation).
- multiple type checking passes (types, side-effects/purity checks, totality checks)
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
- OCaml/SML/1ML
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

Programme Lifecycle
-------------------

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

Memory Management
=================

Concurrent Mark and Sweep for main actor. (Boehm_ conservative gc for starter).
Consider https://wiki.haskell.org/GHC/Memory_Management

Per actor stack and heap. GC when actor is waiting. (See Erlang_, Pony_)

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

# Automatic parallelisation

Annotations? Macros?

.. code:: scala
    @par
    def calculate(∀ a. Num a => x: a, y: a, z: a): a = x * y * z

.. include:: LangsOverview.rst

.. _Liquid Haskell: https://github.com/ucsd-progsys/liquidhaskell
.. _LLVM: http://llvm.org/
.. _Haskell Defered type checking: https://ghc.haskell.org/trac/ghc/wiki/DeferErrorsToRuntime
.. _Akka: http://akka.io
.. _Erlang: https://www.erlang.org
.. _Boehm: https://www.hboehm.info/gc/
.. _Pony: https://www.ponylang.org
.. _Leon: https://github.com/epfl-lara/leon
.. [1] https://en.wikipedia.org/wiki/Persistent_data_structure
.. [2] https://www.infoq.com/presentations/Value-Identity-State-Rich-Hickey

Lasca Design Draft v0.0.1
====

Overview
----
- strict
- functional
- expression based
- statically typed with gradual typing and dynamic modes
- system F with liquid types
- type inference (Hindley-Milner alike)
- type classes (Haskell), or implicit instances (Idris)
- Dynamic/Any type
- several type checking passes (types, big O checks, side-effects/purity checks, totality checks)
- immutable data by default
- batteries included (json, lenses, collections, vars, actors)
- LLVM backend
- JS backend
- no IO monad

Domain
----

Distributed server side applications development. Machine learning tasks. Web development

Goals - substitute Erlang, Scala/Scala.js, Python, Julia and Node.js for server-side development.


Motivation
---

You write code once and read it hundred times. 
Hence, readability and simplicity but expressivity and conciseness is the essence.
One of the goal is to speed-up and partition a usual development cycle:

 change -> get result
 
 prototype -> get fast result -> improve code -> test -> produce optimized program

Compilation time matters. A lot. 
 
To speed-up prototyping I suggest simplify disable/simplify typechecking during prototyping.
This can be done by compiler option with per source file, or even per definition granularity
See Haskell Deffered type checking.


Type System
----

What type systems are for

1. Find type errors at compile time (can't pass Int where String is expected)
1. Help IDE with suggestions
1. Optimal code generation:
    - memory alignment
	- on stack allocation
	- life-time tracking with linear types
	- array bounds checks elimination
	- other runtime checks elimination
1. Correctness	

Our goal is to get the best from type system while not making it too complex and intrusive.	

Switchable gradual/static typing. Both are strong.

System F with Liquid Types. (Liquid Haskell)

Type classes (Haskell or Idris like implicits).

## Thoughts on Subtyping

Implementing data Subtyping implies:
- variance
- complex typer
- protected visibility
- least upper bound for if/case            


Package System
----
Packages are compressed serialized Lasca AST trees.
This allows target machine compilation/interpretation with whole program optimizations.
And it's cross-platform representations.

Packages are transferred via bittorrent or alike P2P service. 
https://lasca.io would be kind of a torrent tracker site.

Memory Management
----

Concurrent Mark and Sweep for main actor. (Boehm conservative gc for starter).
Consider https://wiki.haskell.org/GHC/Memory_Management

Per actor stack and heap. GC when actor is waiting. (See Erlang, Pony)

Concurrency
----

Actors or CPS.

I think Actors/Erlang is better choice for further distributed application.
See Erlang/OTP Scala/Akka for best practices.

Future/Promise

Reactiveness
---
Fully asynchronous APIs.

Consider libuv, see Julia.

FFI (Foreign Functions Interface)
----

Must be as straightforward and simple as possible.
See Pony or Rust.

Syntax
----

- curly-braced blocks
- limited set of definable operators
- Scala-like imports
- Haskell if/then/else
- var inside a function
- method syntax


package test

import something.{Data => D}, D._

pi: F64 = D.pi
 
def len(d) = d * pi

def len(Num n => d: n): F64 = d.toF64 * pi

-- {d: n | d > 0 }
def len(Num n => d: n): F64 = d.toF64 * pi

-- arguments are either inferred or dynamicly typed
-- {x: Int, y: Int, z: List Int | size z > 0 and x + y > 0 }
def example(x, y: Int, z) = {
  assert (size z > 0 and x + y > 0) -- liquid type
  a = x + y -- val declaration, let binding
  b = a :: z -- list cons
  s = "x = $x, x + y = ${x + y}"
  var i = 0 -- var declaration
  while i < a {
  
    step = b.last match { -- pattern matching
      1 | 2 => 1
      name@3 => 2 -- name binding
      name if name < 5 => 3
      _ -> 4 
    }
  
  
    lambda = { x => x + step }  // lambda definition
    
    newlist = { 
    	elem <- z
    	pure z.toString
    }
  
    i := lambda i // variable assignment
  }
}


## Method syntax

Dot syntax implies passing prefix as a called function first argument

1.toString <=> toString 1
1.plus 2 <=> plus 1 2
a.b.c.d e f.g <=> d (c (b a)) e (g f)

Much more familiar and 

## Discourage point-less expressions

Those are hard to read.

## Operators

Provide a limited set of redefinable operator with forced laws to satisfy.

'+', '*' - commutative, associative binary operation
'-', '/' - associative binary operation
'++' - associative binary operation 'append'
'!' - unary not
'!' - binary operation (actors?)
'?' - binary operation

## Visibility
   
Public by default. 

Explicit private keyword or Haskell-like export mechanism.
   
## Comment-based extensions
   
Annotate things in comments, e.g. Haskell-like Liquid type annotations etc.
   
This allows to compose a general textual comment about a function/type with semantically significant informations, 
like liquid types annotations, totality, purity, big O annotations.   
   
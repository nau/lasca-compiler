Lasca Design Draft v0.0.1
====

Overview
----
- strict
- functional
- expression based
- statically typed with gradual typing, and dynamic mode
- system F with liquid types
- type inference (Hindley-Milner alike)
- type classes (Haskell), or implicit instances (Idris)
- Dynamic/Any type
- several type checking passes (types, big O checks, side-effects/purity checks, totality checks)
- batteries included (lenses, collections, vars)
- no IO monad

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

Switchable gradual/static typing. Both are strong.

System F with Liquid Types. (Liquid Haskell)

Type classes (Haskell or Idris like implicits).

Thoughts on Subtyping
----
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


Syntax
----

- curly-braced blocks
- limited set of definable operators
- Scala-like imports
- Haskell if/then/else
- var inside a function


package test

import something.{Data => D}, D._

pi: F64 = D.pi
 
def len(d) = d * pi

def len(Num n => d: n): F64 = d.toF64 * pi

-- {d: n | d > 0 }
def len(Num n => d: n): F64 = d.toF64 * pi
   
   
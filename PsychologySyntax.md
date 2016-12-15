Psychology of Perceptive Programming Language

Motivation
--
Human psychology driven approach for a programming language design.
Lambda-calculus, and Haskell are great for a compiler. Not so much for a human.
People are not computers. Yet, at least.

We must consider human perceptive characteristic designing the language.
For example, at least for me, it's very important to get some working result as quick as possible.
That means I would rather not wait for compilation/type checking/tests/system startup. 
Hence, either all of that happen very quickly, or we need to postpone/disable some of that.

We can postpone compilation/typechecking, run in interpreter mode and do jitting.
We can do a gradual typecheck, run in interpreter mode and do jitting.

When things are getting... TODO

Programme Lifecycle
--
# Prototyping
 Require fastest change-run cycles, it's very important!
 Don't care about types, compilation errors in other packages. If it can be run, it should be run. (JavaScript mode :)
 
# Settlement
 Things are getting cleaner, APIs can be seen and refactored.
 Here we need a typechecker. Mostly in IDE, suggesting things.
 You can define and polish your tests.

# Production
 All API have a valid documentation with examples and tests.
 
# Continuous Integration
 Builds are made with all type/style checks, tests runs, and optimizations enabled.
  
# Critical Software
 Refinement types proofs, Effects proofs. 

Entropy
--
Entropy must be in "perceptive" range.
Not too much of duplication.
Not too much of entropy

Too much entropy:
```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs
```

Better:
```scala
def foldLeft[A, B](col: List[A], z: B, f: (B, A) => B): B =
  (col, z, f) match {
    case (Nil, z, f) => z
    case (x :: xs, z, f) => xs.foldLeft(z, f(z, x))
  }
```

Optimal?
```scala
def foldl(col: Seq a, zero: b, f: (b -> a -> b)): a = match
  nil zero f        => zero                  
  (x cons xs) zero f => xs.foldl zero (f z x)
```



- Not too verbose
- Not too cryptic
- Dilute with keywords (non)



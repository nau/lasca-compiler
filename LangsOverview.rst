Language Rant
=============

Scala
-----

Pros
~~~~

One of the best langs in the world. Love it. Huge type system, quite
powerful. Dotty looks very promising.

Cons
~~~~

Very complicated combination of features affected by numerous Java
restrictions. Hence, very slow compilation Even though it has decent
syntax, it's still quite polluted with (): stuff, especially with
generics

Example
-------

https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Monad.scala

.. code:: scala

        package scalaz
        
        trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
          override def map[A,B](fa: F[A])(f: A => B) = bind(fa)(a => point(f(a)))
        
          def whileM[G[_], A](p: F[Boolean], body: => F[A])(implicit G: MonadPlus[G]): F[G[A]] = {
            val f = Need(body)
            ifM(p, bind(f.value)(x => map(whileM(p, f.value))(xs => G.plus(G.point(x), xs))), point(G.empty))
          }
        
          def whileM_[A](p: F[Boolean], body: => F[A]): F[Unit] = {
            val f = Need(body)
            ifM(p, bind(f.value)(_ => whileM_(p, f.value)), point(()))
          }
        
          def untilM[G[_], A](f: F[A], cond: => F[Boolean])(implicit G: MonadPlus[G]): F[G[A]] = {
            val p = Need(cond)
            bind(f)(x => map(whileM(map(p.value)(!_), f))(xs => G.plus(G.point(x), xs)))
          }
        
          def untilM_[A](f: F[A], cond: => F[Boolean]): F[Unit] = {
            val p = Need(cond)
            bind(f)(_ => whileM_(map(p.value)(!_), f))
          }
        
          def iterateWhile[A](f: F[A])(p: A => Boolean): F[A] =
            bind(f)(y => if (p(y)) iterateWhile(f)(p) else point(y))
        
          def iterateUntil[A](f: F[A])(p: A => Boolean): F[A] =
            bind(f)(y => if (p(y)) point(y) else iterateUntil(f)(p))
        
          def product[G[_]](implicit G0: Monad[G]): Monad[λ[α => (F[α], G[α])]] =
            new ProductMonad[F, G] {
              def F = self
              def G = G0
            }
        
          trait MonadLaw extends ApplicativeLaw with BindLaw {
            /** Lifted `point` is a no-op. */
            def rightIdentity[A](a: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(bind(a)(point(_: A)), a)
            /** Lifted `f` applied to pure `a` is just `f(a)`. */
            def leftIdentity[A, B](a: A, f: A => F[B])(implicit FB: Equal[F[B]]): Boolean = FB.equal(bind(point(a))(f), f(a))
          }
          def monadLaw = new MonadLaw {}
          ////
          val monadSyntax = new scalaz.syntax.MonadSyntax[F] { def F = Monad.this }
        }

Haskell
-------

Great language. But it has 25 years of legacy none could improve.

- Laziness may cause performance uncertainties.
- There are 5 types for strings: String, Text, Text.Lazy, ByteString, ByteString.Lazy. And
  String is the worst one, and is not recommended to use in a real
  program. And it's a default String. It's in Prelude. You can't do a shit
  without enabling at least 5 language extensions. And you have to know
  about them. And there are about 70 of those.

.. code:: haskell

	{-# LANGUAGE OverloadedStrings #-}
	{-# LANGUAGE FlexibleInstances #-}
	{-# LANGUAGE FlexibleContexts #-}
	{-# LANGUAGE MultiParamTypeClasses #-}
	{-# LANGUAGE Rank2Types #-}
	{-# LANGUAGE DeriveGeneric #-}
	{-# LANGUAGE DeriveFunctor #-}
	{-# LANGUAGE BangPatterns #-}
	{-# LANGUAGE PatternSynonyms #-}
	{-# LANGUAGE ScopedTypeVariables #-}
	{-# LANGUAGE TupleSections #-}
	{-# LANGUAGE ViewPatterns #-}
	...

- And there is ``$`` operator. I guess people use it because it's hard to
  introduce another name in you perfect 120 character long one-liner. And
  people hate parens.

SML/Ocaml
---------

Module system is weird. I'd consider type classes instead. Also, there
is a paper "ML Modules and Haskell Type Classes: A Constructive
Comparison", so looks those are interchangable.

::

    This article demonstrates how to translate essential features of ML modules to
    Haskell type classes and vice versa.

https://www.cse.unsw.edu.au/~chak/papers/modules-classes.pdf

Agda/Idris
----------

Pros
~~~~

Great languages. Good type systems, nice and mostly clean syntax Full
type inference Dependent types

Cons
~~~~

Quite hard to grok for an average programmer Complicated (compared to
"usual" imperative lang) effect system with monads/do notations Data
immutability implies complications for an average programmer Unusual
syntax

Syntax I Consider Hard to Interpret
-----------------------------------

Space application
-----------------

Takes some time to get used to. But significantly reduces number of parens used.
It's quite readable, though

.. code:: haskell

        foo x y z = undefined
        
        foo 1 2 3

let-bindings/where-clauses
--------------------------

.. code:: haskell

        foo : Int -> Int
        foo x = case isLT of
                    Yes => x*2
                    No => x*4
            where
               data MyLT = Yes | No
        
               isLT : MyLT
               isLT = if x < 20 then Yes else No

Things defined it reverse order: firstly, isLT is used, then defined.
That's fine for mathematicians, but not ok for me. This is
isomorphic, and much more readable for a developer.

.. code:: haskell

        foo : Int -> Int
        foo x = 
            data MyLT = Yes | No -- aha, so we have MyLT
            
            isLT = if x < 20 then Yes else No -- ok, here is some checking function
            
            case isLT of -- do stuff with above -- aha, here is the actual functionality using already familiar to us things
                    Yes => x*2
                    No => x*4

Type declaration on a separate line, unnamed arguments
------------------------------------------------------

I don't like the whole idea of separate type declarations.

.. code:: haskell

        liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
        liftM f m1              = do { x1 <- m1; return (f x1) }

I think it should be bundled with arguments name for the sake of documentintation.
This allowes us to parameters with and without explicit type declaration.

.. code:: scala

	def liftM(input, f: a => r) = ???



Type class in
-------------

Example
-------

.. code:: haskell

        class Applicative m => Monad m where
            (>>=)       :: forall a b. m a -> (a -> m b) -> m b
            (>>)        :: forall a b. m a -> m b -> m b
            m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
            
            return      :: a -> m a
            return      = pure
        
            fail        :: String -> m a
            fail s      = errorWithoutStackTrace s
        
        
        (=<<)           :: Monad m => (a -> m b) -> m a -> m b
        f =<< x         = x >>= f
        
        when      :: (Applicative f) => Bool -> f () -> f ()
        when p s  = if p then s else pure ()
        
        sequence :: Monad m => [m a] -> m [a]
        sequence = mapM id
        
        mapM :: Monad m => (a -> m b) -> [a] -> m [b]
        mapM f as = foldr k (return []) as
                    where
                      k a r = do { x <- f a; xs <- r; return (x:xs) }
        
        liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
        liftM f m1              = do { x1 <- m1; return (f x1) }
        
        liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
        liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
        
        liftM3  :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
        liftM3 f m1 m2 m3       = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }
        
        liftM4  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
        liftM4 f m1 m2 m3 m4    = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }
        
        liftM5  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
        liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }
        
        ap                :: (Monad m) => m (a -> b) -> m a -> m b
        ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
        
        instance Functor ((->) r) where
            fmap = (.)
        
        instance Applicative ((->) a) where
            pure = const
            (<*>) f g x = f x (g x)
        
        instance Monad ((->) r) where
            f >>= k = \ r -> k (f r) r
        
        instance Functor ((,) a) where
            fmap f (x,y) = (x, f y)
        
        instance  Functor Maybe  where
            fmap _ Nothing       = Nothing
            fmap f (Just a)      = Just (f a)
        
        instance Applicative Maybe where
            pure = Just
        
            Just f  <*> m       = fmap f m
            Nothing <*> _m      = Nothing
        
            Just _m1 *> m2      = m2
            Nothing  *> _m2     = Nothing
        
        instance  Monad Maybe  where
            (Just x) >>= k      = k x
            Nothing  >>= _      = Nothing
        
            (>>) = (*>)
        
            fail _              = Nothing

C/C++
-----

Don't even get me started on those.

D
-

Its author was permanently harmed by C++ experience

Pros
~~~~

Uniform Function Call Syntax Macros?

Go
--

Pros
~~~~

Simplicity Fast compilation Readability defer

Cons
~~~~

Primitiveness. It's to simple. It can't do a shit. Nil Weak and
cumbersome type system. No generics. Duck-typing. Not sure if it's too
bad, though.

Rust
----

Interesting usage of kind of linear types.

Pros
~~~~

No OOP bullshit, just type-classes, ADTs and structs Type-safe memory
management. Impressive, but requires too much attention. Method-based
syntax Generics

Cons
~~~~

No GC. That's all. You can't develop fast w/o GC. No higher-kind types,
not even mentioning dependent types. Semicolons, for gods sake! Com'on,
it's 21st century already! Even JavaScript works w/o semicolons. <> for
generics is just wrong. Overall, syntax is very polluted with all that
std::io::;.,()<>{}'a&\* symbolism. It's unpleasant to read.

Swift
-----

Not bad. But it's just boring and vanilla. To explicit. ##Pros ARC

Cons
~~~~

Still cumbersome syntax, polluted with {}(). Better that C++, though,
anything is better than C++. Weird string interpolation return No real
patter-matching OOP with inheritance etc.

Julia
-----

Interesting. But dynamically typed.

Clojure
-------

Interesting. But dynamically typed. And it's a lisp. People tend to not
like working with bare AST. Compilers do.

Pros
~~~~

-  JVM
-  core.async

Cons
~~~~

-  JVM
-  Lisp

Erlang
------

Pros
~~~~

-  actors as first class citizen
-  simple
-  large infrastructure
-  widely used

Cons
~~~~

-  primitive, unexpressive?
-  no type safety (duh)
-  slow
-  weak documentation

Any dynamic language
--------------------

Writing code is the easiest part of software development. The most
interesting part comes after the code is written. Its maintenance,
changing, and refactoring – that is complex, and must be addressed.
Dynamic languages complicate productive IDE support.

Any Java
--------

You no longer can fix that pile of crap. It's too late, Java, it's too
late.

Any .NET
--------

I don't care about any language/platform bound to a single OS. And Mono
doesn't count. And it's too late, Microsoft, to make it cross-platform.
Fuck you. F# is ok, though. C# is legit too.

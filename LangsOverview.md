Language Rant
===

Scala
---
##Pros
One of the best langs in the world. Love it.
Huge type system, quite powerful. 
Dotty looks very promising.
      
##Cons
Very complicated combination of features affected by numerous Java restrictions.
Hence, very slow compilation
Even though it has decent syntax, it's still quite polluted with ()[]{}: stuff, especially with generics
    
##Example
https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Monad.scala    
    
```scala
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
```
    
Haskell/Agda/Idris
---
##Pros
Great languages. Good type systems, nice and mostly clean syntax
Full type inference
Dependent types

##Cons
Quite hard to grok for an average programmer
Laziness may cause performance uncertainties.
Complicated (compared to "usual" imperative lang) effect system with monads/do notations
Data immutability implies complications for an average programmer
Unusual syntax

Syntax Programmers Won't Accept
--
## Space application
TODO
```haskell
    foo x y z = undefined
    
    foo 1 2 3
```

## let-bindings/where-clauses

```haskell
    foo : Int -> Int
    foo x = case isLT of
                Yes => x*2
                No => x*4
        where
           data MyLT = Yes | No
    
           isLT : MyLT
           isLT = if x < 20 then Yes else No
```


Things defined it reverse order: firstly, isLT is used, then defined.
That's fine for mathematicians, but not ok for developers.
This is isomorphic, and much more readable for a developer. 
```haskell
    foo : Int -> Int
    foo x = 
        data MyLT = Yes | No -- define data
        
        isLT = if x < 20 then Yes else No -- define a function working with the data above
        
        case isLT of -- do stuff with above
                Yes => x*2
                No => x*4
```

## Type declaration on a separate line, unnamed arguments

Not good. Idris is better, but sucks too.

```haskell
    liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
    liftM f m1              = do { x1 <- m1; return (f x1) }
```

## Type class in

##Example

```haskell
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
```
  
C/C++
---
Don't even get me started on that old crap.

D
---
Its author was permanently harmed by C++ expirience

##Pros
Uniform Function Call Syntax
Macros?

Go
---
##Pros
Simplicity
Fast compilation
Readability
defer

##Cons
Primitiveness. It's to simple. It can't do a shit.
Weak and cumbersome type system. No generics.
Goroutines are bullshit. You can't be reactive with it. Future/Promis (Scala-like) are much more powerfull and useful.
Duck-typing. Not sure if it's too bad, though.
Slices are bullshit.
Nil

Rust
---
It looked as a decent language, until they threw away all the good stuff :(

##Pros
No OOP bullshit, just type-classes, ADTs and structs
Type-safe memory management. Impressive, but requires too much attention.
Method-based syntax
Generics

##Cons
No GC. That's all. You can't develop quickly w/o GC. Period.
No higher-kind types, not even mentioning dependent types.
Semicolons, for gods sake! Com'on, it's 21st century already, hello! Even JavaScript works w/o semicolons.
<> for generics is just wrong.
Overall, syntax is very polluted with all that std::io::;.,()<>{}'a&* crap.

Swift
---
Not bad. But it's just boring and vanilla. To explicit.
##Pros
ARC

##Cons
Still cumbersome syntax, polluted with <Generic>{}(). Better that C++, though, everything is better than C++.
Weird string interpolation
return
No real patter-matching
OOP with inheritance etc. I consider it bad design.

Any dynamic language
---
Bullshit. Except Erlang, which is half-bullshit.
Writting code is the easiest part of software development.
The most interesting part comes after the code is written. 
Its maintenance, changing, and refactoring – that is complex, and must be addressed.
Dynamic languages complicate productive IDE support.
 
Erlang
----
##Pros
- actors as first class citizen
- simple

##Cons
- primitive, unexpressive?
- no type safety (duh)
- slow


Any Java
---
You no longer can fix that pile of crap. It's too late, Java, it's too late.

Any .NET
---
None cares about any language/platform bound to a single OS. And Mono doesn't count.
And it's too late, Microsoft, to make it cross-platform. 
Fuck you.
F# is ok, though. C# is legit too.

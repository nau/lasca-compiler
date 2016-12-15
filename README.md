New Lang (Idris/Scala w/o OOP)
=======
Inspired by:
- Scala
- Haskell, LiquidHaskell
- Idris/Agda
- Go
- Rust
- Erlang
- Python
- Swift
- D

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

    bfc -dynamic *.bf // compile in dynamic typing mode
    
    import lang.dynamic
    
    @dynamic
    def func(arg) = ???

Right balance between expressiveness and readability is the essence.

Axis

Time spent developing a programm
Money spent developing a programm 
Cost of an error
Performance
 
 

Ideas
---
- Concurrency Oriented Programming (Erlang). Objects are out. Concurrency is in.
- Gradual Typing (http://homes.soic.indiana.edu/jsiek/what-is-gradual-typing/)
- Deferred Type Errors (runtime compilation mode)
- Liquid Type system (refinement types) + Dependent Type system 
  https://github.com/pleiad/Refinements
  
  Require research. Can be PhD dissertation.
  
- light, non-symbol-polluted syntax
- indentation-based
- readability first
- fast development cycle
- presentation compiler (JSON compiler API?)
- IDE-friendly ('dot-autocomplete', compiler API) 
- type-safe
- strict functional
- expression-based
- practical first, but as clean and concise as possible
- type-level immutability
- prefer things done one way
- make it host language: allow simple creation of external DSLs, configs, etc via compiler API.
- LLVM backend
- JVM backend?
- JavaScript backend via compiler API
- no OOP and data subclassing/inheritance!
- syntactic sugar is ok
- annotations (Java-style)
- annotation-based extensions, like visibility (@private, public by default)
  Consider private by default, bc adding public function to a package may require full source recompilation. If it is.
- macro-based extensions.
- implicits? (Scala/Haskell/Idris)
- implicit macro? (Scala)  
- import features (Scala-like)
- compile-time and runtime reflection
- theorem prover? (Idris)
- dependent types? (Idris)
- totality? (Idris)
- mixfix syntax? (Agda)
- save AST after compilation for reflection
- ABI
- important things must be greppable and googlable, call it searchability :)
- prefer offensive programming style
- compiler as a service (like Scala sbt)
- markdown comments
- https://en.wikipedia.org/wiki/Algebra_of_Communicating_Processes
- CSP?
- CPS, non-blocking IO.

Package System
---
Consider IPFS
P2P systems
Blockchain for 

Practical things that matter
----
- immutability as concurrently shared data
- sized collections in some cases
- pure functions for concurrency and caching
- readability
- simplicity
- familiarity/intuitiveness
- principle of least surprise

Performance (need research)
---
- specialization
- inlining
- escape analysis?
- tail-call optimization? (LLVM?)
- stack allocation
- typesafe move semantics? borrowing etc? (Rust)
- typesafe array bounds check (runtime checks elimination)
- vectorizationstack ghci liquidhaskell (CUDA, OpenCL, CPU (MMX, SSE))

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
  Typecheck.
- Hardcore
  Dependent types enabled. 
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
  IDE allows you to automatically fill type annotations, leaving '?' where it's uncertain.
  Warning if no test found for a function
3. The shit is done (CI mode)
  Hardcore mode.
  IDE suggests @pure, @total, @nothrow, IO annotations
4. Want performance
  Hardcore mode
  IDE suggests places to help escape analysis etc.

Type System
---
- System F? System Fw?
- type level polymorphism (Haskell/Idris/Rust)
- Higher Kind Types
- traits, kind of type clasees
- HM-like type inference 
- Dependent types? Maybe for sized collection. Make it practical. Embed Nat type or something.

Memory Management
----
GC, concurrent mark and sweep

Evaluation
---
Strict, applicative order of evaluation, 
@lazy annotation or (Lazy expr) type for normal order

@lazy val a = readDb()

val a = Lazy readDb()

Chaining?
---
val a: String -> Option MyType = Lazy ???
val b: MyType -> String = Lazy ???

    trait Chainable a = 
      def `->`(self: a, rhs: b -> c): c 

    def `->`(self: f a, rhs: a -> f b) = self.flatMap b
    def `->`(self: a, rhs: a -> b) = self.flatMap b

    r: Option String = "Test"->a->b | "" <==> 
        ("Test".a.fmap t => t.b).getOrElse "" <==> 
            getOrElse ((a "Test") fmap \t => b t) ""
            
Memory Model
----
@volatile

Type-level immutability
---
Type is immutable by default

Mutable things must be require more things to do than immutable, for people to prefer immutable.

Mutable value can be seen as immutable snapshot (CoW?)

Below are raw ideas. Could be bullshit.

Typeclass Mutable?

    var data Account = 
      id: String
      amount: Integer
      
    def modify(var acc: Account, am: Int) = acc.amount := am
    def modify(var acc: Account, am: Int) = acc.amount.update (+ am)
     
    def log(acc: Account) = println acc.amount.toString
      
    def main(args: List String): Unit = 
        val account = Account (id = "id", amount = 25) // immutable
        var acc = account // mutable
        
        // account.modify 5 -- compilation error
        acc.modify 5 // ok, acc changed, account value is copied, and doesn't change
        



Concurrency
---
Goroutins are bullshit.

- Future/Promise (copy Scala impl)
- async/await? TODO: read!
- channels? CPS?
- @volatile
- actors (Erlang, Akka)

Syntax
---
- uppercase Typenames, lowercase idents and type arguments (Haskell)
- pattern-matching
- no OOP
- ADT, traits, type classes, duck-typing?
- easy C interop
- no exceptions
- dependent types?
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
- RAII (defer?, drop?)
- preferred immutability (immutable refs?, views?)
- no null
- no new keyword
- indentation-based
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

Types:
-----
  Idris/Haskell/Scala style, uppercase
  Byte, Char, Short, Int, Long, Float32, Float64, Decimal, Bool, String, Any?, Nothing?, Unit
  
  Can do dependent Int type: 
  
  data Int : (n: Nat) -> Type
  type Int = Int 32
  type Byte = Int 8
  type Short = Int 16
  
  // mutable
  data Point = 
    var x: Int
    var y: Int

  // immutable
  struct Point(x: Int, y: Int)
   
  data Point = 
   
  trait Eq a = 
   fn == (lhs: a, rhs: a) -> Bool

Type conversion
-----
1 as Type
1: Type
implicit conversions?
@implicit?

Standard Library
---
- Types: Bool, Either a b, Result r e, Eq a, Ord a, etc
- Syntax: if_then_else_, while__, for_in_ etc
- Std Functions
- Collections 

Categorical Typeclasses Embedded
---
    trait Functor f = 
        def map(self: f a, f: a -> b): f b
    trait Applicative f <= Functor f = 
        def pure(a: a): f a 
    
                      

Collections
---
- Mutable/Immutable
- Size-dependent/independent
- Iterable a
- Iterator a
- Immutable: List, Vec, Map, TreeMap
- Mutable: Array, ListBuf, Map

Default Map - mutable HashMap
Default Seq = immutable Vec

data Iterator a
  next: Option a // a?

Other Ideas
-----
Structs and tuples are product types
Basically, structs/records = named tuples
Maybe Haskell like?



Function arguments are named tuples, if uncurried

Func return type could be a named tuple/struct - No need, bc we can pattern-match on func result

struct/record as JavaScript

type T = (key: String, value: Any)
val t = (key = "k", value = "v")
val t1 = t.copy(key = "k1").value
val m = {"name": value1, ident: value2}  
fn process T -> T
fn process(key: String, value: Any) -> T
val r = process t
r.key ++ r.value.toString

Packages: Java-like, lowercase

Syntax:
  import Scala-like
  val, var, fn
  lambda: 
    x -> x + 2
    x: Type a, y -> x + y
    \x: Type, a, y => x + y 


if True then 1 else 2
while True doStuff()
return 1

def main args: Array String arg2: -> Unit

fn effect: () -> () = println "Fuck"

fn effect() -> Unit = println "Fuck"

fn foldl[a](col: List a, a -> ())

effect(arg: String) -> () = println("Fuck $arg")

fn main (args: Array String, arg1: Int) -> Unit = 
  val a: Int = 5
  var i = 0
  val f = fn a: Int, b: String = a + 1
  lazy val t = 1, 2
  t match	
    (1, _) -> “one”
    _ if 
    

data Point = (
  x: Int,
  y: Int
)

data Bool =
  True | False |
  Dunno

val array = array 1 2 3 4 5
val list = list 1 2 3 4
val m = map (“1”, 1) (“2”, 2) 

Function definition
---
Must contain type annotations and arg names. It's its documentation. 
Enforce documentation? At least, simplify.
Java-style func name identifiers.
Forbid uppercase and _ first letter.
Only as `_IReallyNeedSuchName`
Functions are auto-curried.
Accept either tuple of args or is partially applied

fn or func or def.

func doStuff(arg1: Int, arg2: Interface, arg3: Map String Int, arg3: Map String (a: Ord & Message)) -> Bool

Either:

## (Haskell/Agda)-like

    functionName: Type -> Type -> Type
    functionName  arg1    arg2    argN

Pros: simple, no keyword, argument pattern-matching
Cons: unusual, type declarations on a separate line, separate lambda syntax
 
## (Scala, Rust, Swift, Go)-like

    // explicit type annotation 
    def/fn/fun/func functionName(arg1: Type, arg2: Type, argN: Type) -> Type = expr
    
    def/fn/fun/func functionName(arg1: Type, arg2: Type, argN: Type): Type = expr

    // type-inference
    def/fn/fun/func functionName(arg1, arg2, argN) = expr
 
Pros: simple, can be used as lambda syntax
Cons: uncurried, can't mix inferred and explicit types on arguments

## Mixed

    functionName(arg1: Type, arg2: Type, argN: Type): Type = expr // its type Type -> Type -> Type -> Type
    functionName(arg1: Type, arg2: Type, argN: Type) -> Type
    functionName(arg1, arg2, argN) = expr
Traits
---
trait Fn a r = 
  def apply(self, v1: a): r
  def defined(self, v1: a): Bool = True
  def andThen(self, f: r -> rr): a -> rr = \arg => f (self arg)  

   
@infix
type -> a b = Fn a b   // hehe
   
def map(ft: Functor a, fn: Fn a b)   

trait Functor (f : Type -> Type) =
  def map(self: f a, f: a -> b): f b
  
data List a = Nil | Cons a (List a)

@infixr(5)   
def ::(head: List a, v: a) -> List a = Cons v head

##Traits inheritance

trait Applicative a <= Functor a + Pure a = 
   
Error Handling
----
Probably, we should support both Exceptions and Errors.
Though we need to be able ensure at compile time some functions don't throw exceptions.
Maybe via some @nothrow or @pure annotation or something

Exception/Error must contain a stacktrace, error code, and message

Currently, I prefer Rust/Go way. You must declare errors, IDE do its best to automate this.
Handle errors or panic. What the hell. It's good for actors, see Erlang.


   ???

## Result (Rust)   
Right-biased Either. Maybe add some syntactic sugar
    
    data Result r e =  
      | Ok r
      | Err e
      
    map(f: Result a, f: a -> b): Result b = match
        Ok a f  => Ok (f a)
        Err e _ => Err e
      
    fmap(f: Result a, f: a -> Result b): Result b = match
        Ok a f  => f a
        Err e _ => Err e
      
    mapErr(r: Result a, f: a -> b): Result b = match
        Ok a _  => Ok a
        Err e f => Err (f e)
     
    stacktrace(r: Err e): [String] = lang.internal
          
          
Here, Rust panic/catch_unwind/resume_unwind are very close to throw/catch/throw Java et al mechanism, 
and is similar to Go panic/recover 
          
      
Consider
---
Most syntax-like constructs are functions/macros.

Agda-style mixfix function:

if_then_else_ cond expr alt
if_then_else_ True expr _ = expr
if_then_else_ False _ alt = alt

[] and {} as a special systax need to be processed with macros.
Say,

    List [1 2 3] produces List Int
    Vec [1 2 3] produces Vec Int
    Map [(1,"2") (2, "2")] produces Map Int String
    
    [1 2 3].list
    
    val a: Map Int String = {1: "1", 2: "2", three: three.toString}
    
    def typeGen(a: Type): Type = List a
        
    type LT = typeGen Int
        
## = as Bool matching operator (not sure it's a good idea)
Erlang-ish, Go/Rust if let    
        
    val (a, b) = (1, 2, 3) // panic
    val a = (a, b) = (1, 2, 3) // False        

    if (1, a) = func() then do a else do 1
     
    func() match
      (1, 2) => do
      _      => do


WIP Examples
---
Immutable ADT
    
    data Tuple = Tuple a b
    data Op = Plus | Minus
    data Expr = 
        Val a | 
        BinOp (op: Op, lhs: Expr, rhs: Expr)
    
Immutable record/struct/field labels    
    
    data Person =
      id: String
      name: String
          
    data Person(id: String, name: String)
          
Mutable record/struct/field labels
     
    var data Person =
      id: String
      name: String    
      
    var data Person(id: String, name: String)
      
or

    data Person =
      id: String
      var name: String    
         
    data Person(id: String, var name: String)
    
    trait Eq a =
      (==)(self, rhs: a)
      (!=)(self, rhs: a) = !(self == rhs)
      
Program Example
---
data Magic = 
  | Main 0xD9B4BEF
  | TestNet 0xDAB5BFFA

data Message = 
  magic: Magic
  command: [Byte] 12
  length: Int
  checksum: Int
  payload: Payload
  
data Payload = 
  | Version = 
      version: Int
      services: Int 64
      timestamp: Int 64
      addr_recv: NetAddr
  | VerAck
  | Addr = 
      count: VarInt
      addlList: [Byte]
      
val msg = Message (magic = Main, 
    command = [1 2 3 4 5 6 7 8 9 10 11 12],
    length = 15
    checksum = sha1 VerAck 
    )     


Dependent Types
----
data Integers : (signed: Bool) -> (bits : Nat) -> Type = 
  | Byte False 8
  | Int8 True 8
  ...

def Ints(bits: Nat, type: Type): type bits = 

Further Research
====
Why totality matter? What real benefits we get?
Pros: it terminates :) What else?

Why dependent types matter? What we get?
Sized collections.

    
FAQ
===
    
##Will there be support for Unicode characters for operators?
No.    
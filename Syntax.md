Literals:
-----  
1, 0xdeadbeef: Int
1L: Long
"": String
True | False or true | false...

Keywords
----
alias?
as?
break?
case?
class? -- no. ambiguous with OOP class. Use trait instead.
continue?
data
def
extend?
extern
fn, fun, func? -- no, use def. We may consider it a logical judgmental at some point, so let a function be a definition.
for
if
implicit? (better with annotation if needed)
import
infix, infixl, infixr
instance/impl?
let?
macro
match?
package
pub?/private? - no, use annotations
struct? -- use data
then
trait
type
use?
val
var
while? (maybe make as non-total function)
where?
yield?

AST
---

data Tree = Ident(name) 
    | Select(Tree, Ident) 
    | Apply(Tree, Tree) 
    | Func(name: Ident, args: Seq Tree) 

Thoughts    
---

Make a small core. 
Data: data/record
Control structs: match and recursion (hopefully, total)

Others are extensions (Nemerle)

if then else -> mixfix operator

    def if_then_else(cond: Bool, thn: Lazy a, els: Lazy a): a
    
Context Bounds Syntax
---

```scala
    
    data Person a = 
      id: a
      name: String
    derive (ToString, Hash)  
      
    type IntPerson = Person Int  
    
    trait Storage a
    
    trait Persist st a <= ToString a, Storage st = 
      def save(storage: st, value: a): Result a Err
      
    File  
      
    save(storage: File, value: a) =  
      
    impl Sort f a <=  
     
```


asdf


```scala
	Unit, UInt32, Float32
	type Float = Float32
	type Point = {
	 x: Float;
	 y: Float; <-- semicolon, to simplify copy/paste and reordering
	}
	
	def areaDyn(p) = p.x * p.y
	
	def area(p: Point): Float32 = p.x * p.y
	
	val p = Point(1, 2)
	p.area
	
	type Point = Point(x: FLoat, y: Float)
	
	type Bool = True | False
	type Expr = Val(name: String) | Func(name: String, args: List String, type: Type)
	
	trait Closable {
		def close(): Unit
	}
	
	trait ToString a {
	  def toString(self: a): String
	}
	
	trait FromString a {
	  def fromString(s: String): a
	}
	
	trait StringIso a : ToString a, FromString a 
	trait StringIso a : (ToString a, FromString a)
	 
	trait StringIso a <= ToString a, FromString a 
	trait StringIso a <= (ToString a, FromString a)
	 
	trait StringIso a where ToString a, FromString a 
	trait StringIso a where (ToString a, FromString a) 
	
	trait Resource a <= Closable a, ToString a { // class (Closable a, ToString a) => Resource a where
		def aquire(r: a): a
		def withResource(r: a, f: a -> b): b  
	}
	
	type Lock = Lock(lock: Mutex)
	
	def aquire = lockMutex
	 
	 
	trait Functor f {
	  def map(self: f, f: a -> b): f b 
	}
	
	trait FlatMappable f {
	  def flatMap(self: f, f: a -> f b): f b 
	}
	
	trait Monad m <= (Functor f a, FlatMappable f a) {
	  def pure(a): m a
	}
	
	type List a = Nil | ::(a, List a)
	
	@infixr(5) def (::)(ls: List a, el: a): List a = ::(a, ls)
	
	def map(self: List a, f: a -> b): b = match self {
		Nil -> Nil
		Cons(a, xs) -> Cons(f a, map xs f)
	}
	
	
	val Pi = 3.14
	
	def parse(input: String): Expr = 

```

```scala
	import cats._
    
    val intToString: Int => String = _.toString
    val double: Int => Int = _ * 2
    val addTwo: Int => Int = _ + 2
    
    implicit val optionApply: Apply[Option] = new Apply[Option] {
      def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
        fa.flatMap (a => f.map (ff => ff(a)))
    
      def map[A,B](fa: Option[A])(f: A => B): Option[B] = fa map f
    }
    
    implicit val listApply: Apply[List] = new Apply[List] {
      def ap[A, B](f: List[A => B])(fa: List[A]): List[B] =
        fa.flatMap (a => f.map (ff => ff(a)))
    
      def map[A,B](fa: List[A])(f: A => B): List[B] = fa map f
    }
    
    
    import cats._
    val intToString: Int => String = _.toString
    val double: Int => Int = _ * 2
    val addTwo: Int => Int = _ + 2
    
    instance Apply Option {
      def ap(fa Option a, f  List (a => b)) Option b = fa.flatMap (\a => f.map (\ff => ff a))
    }
    
    def slice (ls: State[(a, Int) => z])
    type State = forall a. State a => Int|String => z having Sum + Show a, Show z, 
    def slice (ls: State) = 
    
```

//before 2017
class Person implements Dao {
	val name
	val age
	def save(db, self) = db.execute("insert into person value(${self.name}, ${self.age})")  
}

desugared

struct Person {
  name: String
  age: Int // { age > 0 }	
}

instance Dao[Person] {
  def save(db, self: Person) = db.execute("insert into person value(${self.name}, ${self.age})")  
}

def main(args: [String]) = {
  val person = Person()
}

// untyped
data Person(name, age)
def save(p, db) = db.execute "insert into person value(${self.name}, ${self.age})"

trait Dao {
  def save(p, db)
}

//typed
data Person(name: String, age: Int) <=> data Person = Person(String, Int) <=>
data Person {
  name: String
  age: Int
}

trait Dao(a) {
  def save(a, db: Database): Unit
}

instance Dao(Person) {
  def save(a: Person, db: Database): Unit = db.execute "insert into person value(${self.name}, ${self.age})"
}

def main(args: Array(String)): Unit = {
  
  def getDataFromServer() = {
    var data = ""
    server.onReceive((str) => { data += str }) // won't compile! var leaves function scope!
  }
  
  
}

// ADT

data Expr(a: Type) = Add(lhs: Expr(Int), rhs: Expr(Int)): Expr(Int)
	| I(n: Int): Expr(Int)
	| B(b: Bool): Expr(Bool)

data Bool = True | False



// Service, Container, mock


val defaultService = data Service {
  def save(db, datum): Unit = ()
  def load(db, id): String = ""   
}

val container = data Container {
  service = defaultService  
}

generates

data Service = Service(save: (db: Any, datum: Any) => Unit, load: (db: Any, id: Any) => String)

val defaultService = Service((_, _) => (), (_, _) => "")

data Container = Container(service: Service)

container = Container {
	service = defaultService
}

val mockService = mock Service

def mock(t: Type): t = 

container.service <=> Container.service(container)

container.service.save db "test"
container.service.save db, "test"
container.service.save(db, "test")

import container.service._
db.save "test"

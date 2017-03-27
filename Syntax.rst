Comments
--------

##Inline comment:
- consider '--' or '#' or '//'
  I prefer '--' because it's less visually noisy. 
When used after a code line it looks like a hyphen.
 
```scala
doStuff() -- this call does stuff
doStuff() # this call does stuff
doStuff() // this call does stuff
```

##Block comment

- Consider: /* */, {- -}, """ """
- Must be nestable! It's much easier to comment a code with comments.

Keywords
--------
as
break
case ?
catch?
class? -- no. ambiguous with OOP class. Use trait instead.
continue
data
def
exist?
extern
fn, fun, func? -- no, use def. We may consider it a logical judgmental at some point, so let a function be a definition.
for
forall?
if
implicit? -- no (better with annotation if needed)
import
in
infix, infixl, infixr?
instance/impl?
lazy
let
macro
match
meta?
package
pattern
pub?/private? - no, use annotations or comments
return
then
try?
throw?
trait
type
use?
val
var
while? (maybe make as non-total function)
where?
with
yield?

Visibility
----------
Idea: export only functions that have doc-string.

-- | Do stuff
def publicDoStuff = ???

-- @ private, pure, total, inline
def privateDoStuff = ???

Literals:
---------
1, 0xdeadbeef: Int
1L: Long
1.2: Double
1.2f: Float
"": String
true | false: Bool
(): Unit
(1, true): (Int, Bool) - tuple

{ key1 = value, key2 = value } - map
[1, 2, 3] -- immutable array?
[1..10] -- stream? iterator?
[x | [1..10]] -- stream? iterator?
Array [1, 2, 3]
List [1, 2, 3]
Seq [1, 2, 3]
Set [1, 2, 3]

Expressions
-----------
-3*(1+7)/2 mod 3
~1.0/2.0 + 1.9*x
a or b and c

Functions
---------
def id x = x
def id(x: a): a = x
def f =

Control Flow
------------
if 3 > 2 then "X" else "Y"
if 3 > 2 then println "hello" -- Note: expression has to have type unit
while true {
  println "X"
}

do {
  i <- [1..10];
  println i.toString
}

[1..10].foreach { i -> println i.toString }

Value Declarations
------------------

Top-level:
val name = expr
def f(x, y) = {
  a = 1;
  b = 2;
  a + b
}

Type Declarations
-----------------

type T = Int -> Bool

data Option a = None | Some a deriving (ToString, FromString, Functor, Json)
data List a =
  | Nil
  | Cons a (List a)
data Age(age: Int)
data Person a (
  name: String
  age: Age
  info: a
)

-- GADT
data Either a b =
  | Left:  a -> Either a b
  | Right: b -> Either a b



Pattern Matching
----------------
def getOpt(opt, d) = {
  | Some x, _ => x
  | None, d   => d
}

def getOpt(opt, d) = opt match {
  | Some x if x > 0 => x
  | _ -> d
}

def getOpt(opt, d) = {
  t@(a: Int, b) = getTuple();
  println b;
  t
}

Tuples
------
Left to right evaluation?

type Foo = (Int, Float, String)
val bar = (0, 3.14, "hi")
f: Foo -> Float = Foo._2
bar._2 -- 3.14
f bar -- 3.14
f(bar) -- 3.14

Records
-------
data Person a (
  name: String
  age: Age = Age 18
  info: a
)

type SP = Person String

person = SP(name = "Alex", info = "Info")
person.name -- "Alex": String
person.(SP.name) -- ???
SP.name -- SP -> String
SP.name(person) -- "Alex": String
SP.name person -- "Alex": String

person.name = "Ira" -- SP (name = "Ira" ...)
person.age.age = 33 -- SP (age = Age(33))
age = person.age
age.age = 29 -- Age(29)
person.copy(name = "Alex", age = Age(33))


References
----------
var r = 0

Comparisons
-----------
Trait Eq a

2 == 2
2 != 3

Strings
-------
Immutable

data String(bytes: Array Byte, length: Int)

def concat(a: String, b: String): String = {
    otherLen = b.length;
	if otherLen == 0 then a
    else {
	  len = a.length;
	  buf = Array.copyOf(a.value, len + otherLen);
	  b.getChars(buf, len);
	  String(buf, true);
    }
}

instance StringMonoid: Monoid String {
  def empty = ""
  def ++(a, b) = String.concat(a, b)
}

"Hello " ++ "world!"

name = "Alex"
fullName = "Hello $name"
fullName = "Hello ${foo(name) ++ name}"
multiline = "1
             2
             3".stripMargin


r"[helo]+".match "hello" -- true
r"[helo]+".find "hello" -- true

Exceptions
----------

Traits/Type classes
-------------------

trait Eq a {
  def ==(a: a, b: a): Bool = not(a != b)
  def !=(a: a, b: a): Bool = not(a == b)
}

instance Eq Int {
  def ==(a: Int, b: Int): Bool = eqInt(a, b)
}
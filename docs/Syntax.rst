Syntax
======

What to consider in order of importance

- readability
- expressiveness/conciseness
- easy to type
  short keywords, less shifts for capitals and symbols
- extensibility
- unambiguous
- DSL embedding
- code generation
- easy to highlight syntax based on regexps
  Most syntax highlighters use simple regexp-based approaches (think of StackOverflow, blogs etc).
  To make things simpler, avoid context-dependent syntax (don't do like Haskell)
- copy-paste-ability

Shortest keywords for most used operations.


Thoughts
--------

- Curly-braced blocks for lambdas.
  It's often needed to enclose lambdas in parens, so

- preferred camel case naming (Java style)
- limited set of definable operators
- Scala-like imports
- Haskell if/then/else and case/of
- var inside a function
- method syntax

Curly Braces vs Indentation
---------------------------

https://www.quora.com/What-are-the-downsides-to-whitespace-indentation-rather-than-requiring-curly-braces

# Indentation Pros

#. Nice readability, less visual noise from braces, no semicolons
#. Code is always formatted

# Indentation Cons

#. No auto formatting for indentation based code.
#. Whitespace indentation makes code generation unnecessary hard.
   It is a lot easier to generate your structural code in whatever way that is easier for machine to read and
   let some post-processor format it nicely for human to read.
#. While doing a merge in Perforce/SVN/CVS, it's quite easy to unintentionally remove/introduce whitespaces
   (especially quite easy when you come from a background where whitespaces do not matter -
   for example, a Java developer changing a small piece of Python code)
#. Its much easier to copy code from one place to another if the whitespaces and indentation do not matter
#. Tabs and spaces are easy to mix-up.
   If your tab-length is 4 spaces, an indentation of 1 tab and 4 spaces will look the same.
   However, Python distinguishes between the two. You will struggle to find where the error in your code is,
   because the indentation "looks" uniform everywhere but isn't.
#. Vim allows easy navigation with curly brackets.
   Just navigate to the opening (closing) curly bracket and press '%'
   and you will be taken to the closing (opening) curly bracket. No such thing for Python.
#. Let's say you had a huge block of code in a try-catch.
   It would naturally be indented by (say) 1 tab more than the code outside it.
   Now if you want to remove the try statement, have fun removing the indentation on the huge block of code in it (if it were curly braces, simply commenting those out would do the job).
#. Conversely to point 4, adding a block of code in between is also troublesome (points 4 and 5 are mentioned in the context of code development, where you might not be sticking firmly to how the code looks. Let's say you wanted to just add/modify a block of code to see what difference it made).
#. Again, as the number of lines in the code increases, the fear that some indentation has gone wrong somewhere builds up drastically. It can also be extremely difficult to figure out where the indentation has gone wrong.
#. As the end of a block is not defined using a non-white space character, block endings should be coded carefully, lest you mistakenly put the last statement outside the indented block. With curly braces, the chances of this are low because visually there's a concrete block-ending character.

https://github.com/lampepfl/dotty/issues/2491

Comments
--------
Requirements:
- allow semantic comments
- nested comments
- clean syntax
- support some doc formatting (consider Markdown, ReStrucTured, AsciiDoc)
- extensibility

##Inline comment:

Consider ``--`` or ``#`` or ``//``. I prefer ``--`` because it's less visually noisy.
When used after a code line it looks like an em dash followed by a description.
It would be nice to also support shebang #! for scripting

.. code:: scala
  #!/usr/local/lasca -e

  doStuff() -- this call does stuff
  doStuff() # this call does stuff
  doStuff() // this call does stuff

  -- Here we do things
  doStuff()

##Block comment


.. code:: scala

  /*
      Main function
  */
  def main() = println("Hello World")

Block comment
- Consider: ``/* */`` C-style, ``{- -}`` Haskell-style, ``""" """`` Python-style, and ``(* *)`` ML-style
- Must be nestable! It's much easier to comment a code with comments.

Comment-based extensions
~~~~~~~~~~~~~~~~~~~~~~~~

Annotate things in comments, e.g. Haskell-like Liquid type annotations etc.

This allows to compose a general textual comment about a function/type with semantically significant information,
like liquid types annotations, totality, purity, big O annotations.


Keywords
--------

Idea is to reserve lots of keywords for simplify further language extensibility,
and motivate people to use more descriptive names :)

``alias``?, ``and``, ``as``?, ``break``?,
``case``?, ``const``, ``continue``?,
``data``, ``def``, ``do``,
``extend``?, ``extern``,
``for``?, ``fun``/``fn``
``if``, ``import``, ``infix``?, ``infixl``?, ``infixr``?, ``in``?
``interface``?, ``instance``/``impl``?,
``let``?,
``macro``?, ``match``?,
``not``
``or``
``package``, ``private``?,
``struct``?,
``then``, ``trait``, ``type``,
``use``?,
``val``?, ``var``,
``while``, ``with``, ``where``?,
``xor``
``yield``?,

``implicit``? (better with annotation if needed)

``class``? – no. ambiguous with OOP class. Use trait instead.

``fn``, ``fun``, ``func``? – no, use ``def``.


Visibility
----------
Idea: export only functions that have doc-string.

-- | Do stuff
def publicDoStuff = ???

-- @ private, pure, total, inline
def privateDoStuff = ???

Options:

#. Export all/explicit export of functions and types at module definition (Haskell, Erlang etc)
#. Name-dependent visibility. E.g if an identifier starts with lowercase/__ letter(s) that it's private. (Go, Python)
#. Public by default, explicit ``private`` keyword to make a function/type private. (Scala)

I'd like to disallow ``_`` in identifier names, and distinguish functions and types by first lowercase/uppercase letter.

Public by default. Explicit ``private`` keyword.

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

Identifiers Names
-----------------

Disallow [-_'] symbols in plain identifiers.
Functions and val/vars should start with a lowercase letter and must not contain underscores etc.
If it's required for some reason, use back-ticks (as in Scala):

Type names start with uppercase letter. Same rules apply.

.. code:: haskell

	`arbitrary ident_name with keywords import` = 1

	type OptString = Option String

Operators
---------

Provide a limited set of redefinable operator with forced laws to satisfy.

- ``+``, ``*`` – commutative, associative binary operation
- ``-``, ``/`` – associative binary operation
- ``++`` – associative binary operation ``append``
- ``::`` – list cons
- ``!`` – binary operation (actors?)
- ``?`` – binary operation

I'd like to avoid postfix operators at all, and restrict a set of prefix operators to [!, -, ~].
Also, I'd like to avoid compound operators (`+=`, `*=` etc)

Operators in languages, comparison table

=============================  ===============  ===============    ==========  ==========  ==========  ==========  ==========
Operator                       Lasca            Python             Julia       Scala       Haskell     Ocaml       Go
=============================  ===============  ===============    ==========  ==========  ==========  ==========  ==========
Numeric                        `+` `-` `*` `/`  `+` `-` `*` `/`
Modulus                        %                %                  mod % rem   %           mod rem     mod         %
Exponent                                        `**`               ^           pow         ^ ^^ `**`
Floor division                                  //

Logical AND                    and              and                &&          &&          &&
Logical OR                     or               or                 ||          ||          ||
Logical NOT                    not              not                !           !           not

Bitwise AND                    &                &                  &           &           .&.
Bitwise OR                     |                |                  |           |           .|.
Bitwise XOR                    xor              ^                  $           ^           xor
Bitwise NOT                    ~                ~                  ~           ~           complement
Logical shift right                             >>                 >>>         >>>
Arithmetical shift right                        >>                 >>          >>
Logical/arithmetic shift left                   <<                 <<          << <<<

Equality                                        == != <>           == !=       == !=
Relational                                      < <= > >=          < <= > >=   < <= > >=   < <= > >=   < <= > >=   < <= > >=
=============================  ===============  ===============    ==========  ==========  ==========  ==========  ==========


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
.. code:: scala
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

Method syntax
-------------

Dot syntax implies passing prefix as a called function first argument.
It's more familiar and intuitive for a programmer. May simplify adoption.

#. ``1.toString`` <=> ``toString 1``
#. ``1.plus 2`` <=> ``plus 1 2``
#. ``a.b.c.d e f.g`` <=> ``d (c (b a)) e (g f)``


Call syntax
-----------

I'm thinking on mixing applicative function call syntax with argument list call syntax, and method syntax calls.
And make it possible to use implicitly tupled functions, like



.. code:: haskell

	def foo(a: Int, b: String = "zero"): Bool

    foo 1 "one"
    foo(1, "one")
    foo(1) -- foo 1 "zero"
    foo(b = "one", a = 1) -- foo 1 "one"
    foo 1 -- partial application
    1.foo -- foo 1 "zero"
    1.foo "one" -- foo 1 "one"
    1.foo("one") -- foo 1 "one"
    1.foo(b = "one") -- foo 1 "one"




Value Declarations
------------------

Top-level:

.. code:: scala
  val name = expr
  def f(x, y) = {
    a = 1;
    b = 2;
    a + b
  }

Type Declarations
-----------------
.. code:: scala

  type T = Int -> Bool

  type IntMap a = Map Int a
  type IntMap = fun (a: Type) = Map Int a

  foo: Pi (a: Type) = IntMap a

Abstract Data Type Declarations
-------------------------------

  data Void -- empty data declaration

  data Maybe a = None | Some a -- simple ADT

  data Either a b = Left a | Right b deriving (ToString, FromString, Functor, Json) -- ADT with derivations

  data List a =
    Nil
    Cons a (List a) -- ADT

  data Expr = Var String | App Expr Expr
    Lam String Expr

Record/Struct Declarations
--------------------------

  struct Person a =
      name: String
      age: Age
      info: a

Pattern Matching
----------------
.. code::
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

.. code:: scala
  type Foo = (Int, Float, String)
  val bar = (0, 3.14, "hi")
  f: Foo -> Float = Foo._2
  bar._2 -- 3.14
  f bar -- 3.14
  f(bar) -- 3.14

Records
-------

.. code:: scala
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
.. code:: scala
  var r = 0

Comparisons
-----------
.. code:: scala
  Trait Eq a
    2 == 2
    2 != 3

Strings
-------
Immutable

.. code:: scala

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
.. code:: scala

  trait Eq a {
    def ==(a: a, b: a): Bool = not(a != b)
    def !=(a: a, b: a): Bool = not(a == b)
  }

  instance Eq Int {
    def ==(a: Int, b: Int): Bool = eqInt(a, b)
  }

Basic Types
-----------

Not decided yet.

Numbers
~~~~~~~

Not sure about naming. Either

- ``I8``/``U8``/``Byte``, ``I16``/``U16``/``Short``, ``I32``/``U32``/``Int``, ``I64``/``U64``/``Long``, ``F32``, ``F64``
- ``Byte``/``UByte``, ``Short``/``UShort``, ``Int``/``UInt``, ``Long``/``ULong``, ``Float32``, ``Float64``/``Double``
- ``Integer``/``BigInt``/``Decimal``/whatever for unlimited precision numerals
- maybe have ``Int`` be the size of target machine word?

Do we need unsigned types?

.. code:: haskell

	type Nat = { i: Int | i >= 0 } -- Natural numbers

Bool
~~~~

Consider Bool as ADT defined in Prelude.

.. code:: haskell

	data Bool = True | False

Char And String
~~~~~~~~~~~~~~~

Consider not having separate Char type.

Java, JavaScript use UCS-2 2 byte Unicode code point representation.
That used to be OK, but know we have more than 65536 code points and they can't be represented by a single 16-bit word.
So a code point must be represented with UTF-32, 32-bit unsigned word. (U32/UInt32/UInt you name it).

So, default ``String`` type is a UTF-8 encoded Unicode string.


Compound Types
--------------

- algebraic data types (Haskell like)

.. code:: haskell

	data Bool = True | False

- GADT

.. code:: haskell

	data Expr a = IntVal(value: Int): Expr Int | StringVal(value: String): Expr String

- records

.. code:: haskell

	data Point a = Point(x: a, y: a)

	data Point(x: Int, y: Int) -- syntax sugar.

	data Person {
		firstName: String
		secondName: String
		age: Nat
	}

    val p = Person "Alex" "Nemish" 33
    val p = Person("Alex", "Nemish", 33)
    val p = Person(firstName = "Alex", secondName = "Nemish", age = 33)
    val p = Person { firstName = "Alex", secondName = "Nemish", age = 33 }

    Person.firstName p == p.firstName

	-- ADTs/GADTs
	data List(a) = Nil | Cons(head: a, tail: List(a))
	type List(a) = Nil | Cons (head: a, tail: List(a))
	type List(a) = Nil | Cons { head: a, tail: List(a) }

	-- GADT
	data Expr(a) = Unit: Expr(a) | Iadd (l: Int, r: Int): Expr(Int) | Isub (l: Int, r: Int): Expr(Int)

	-- Traits/Interfaces/Type classes
	trait Functor(F) {
		-- pure
		def map(c: F(a), f: a => b): F(b)
	}

	instance Functor(List) {
		-- pure, O(c.size), total
		def map(c, f) = {
		  (Nil, f) => Nil
		  (x::xs, f) => Cons(f(x), xs.map(f))
		  (x::xs, f) => Cons(f x, xs.map f)
		  (x::xs, f) => Cons (f x) (xs.map f)
		}
	}



Data derives ``(ToString, Eq, Hash, Json)`` by default.




Lens And Immutable Data Structures
----------------------------------
.. code:: ruby

	data Vector(a: Point, b: Point, c: Point)

    v = Vector Point(0, 0) Point(1, 1) Point(2, 2)
    v1 = v.a.x := 1 --> Vector(Point(1, 0), Point(1, 1), Point(2, 2))
    v1 = v.a.x ~= { _ + 1 } --> Vector(Point(1, 0), Point(1, 1), Point(2, 2))



Donts
-----

Discourage point-free expressions!
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

From https://wiki.haskell.org/Pointfree


	Pointfree Style
		It is very common for functional programmers to write functions as a composition of other functions,
		never mentioning the actual arguments they will be applied to. For example, compare:

		 .. code:: haskell

			sum = foldr (+) 0

		with:

		 .. code:: haskell

			 sum' xs = foldr (+) 0 xs

		These functions perform the same operation, however, the former is more compact,
		and is considered cleaner. This is closely related to function pipelines (and to unix shell scripting):
		it is clearer to write let fn = f . g . h than to write let fn x = f (g (h x)).


I find this style extremely non-intuitive, hard to read, understand, and maintain.
Saving few characters doesn't worth it.



Ideas
-----

.. code:: scala

	def main = {
	-- if
		if 0 <= idx < 10 and array(idx) > 0 then a else b -- Chaining comparisons (Julia, Python)
		if true then dostuff() <=> if true then { dostuff(); () } else ()
	-- Streams, Lists, List comprehension
		list = [1, 2, 3]
		list = [x | x <- 1..10 if x < 3, y <- 1..20 if y - x > 0]
	-- Lambda
		func = { () => 1 }
		func = { () => 1 }
		func = { () -> 1 }
		func = { -> 1 }
		func = { x => x + 1 }
		func = { x -> x + 1 }
		func = { (x, y) => x + y }
		func = { (x, y) -> x + y }
		hof = list.map { _ + 1 } <=> map list { x => x + 1}
		hof = list.map { x => x + 1 }
		hof = list.collect { (x, y) if x > 0  => x + y } -- like Scala case, but without ``case`` keyword
		func = x => { x + 1 }
	-- Map literal
		a = "a"
		one = 1
		dict  = { a: one, "a": 1, "b": 2 }
		dict2 = [ a = one, "a" = 1, "b" = 2, "c" = 3 ]
		dict2 = [ a = one | (a, one) <- genTuples if a > one ]
		dict2 = Map [ a = one | (a, one) <- genTuples if a > one ]
		dict2 = [ a: one, "a" : 1, "b" => 2, "c" => 3 ]
		dict  = { a => one, "a" => 1, "b" => 2, "c" => 3 }
		dict2 = [ a => one, "a" => 1, "b" => 2, "c" => 3 ]
		dict3 = Map [ (a, one), ("a", 1), ("b", 2), ("c", 3) ] -- Haskell-like. Most reasonable
	-- Pattern matching
		patmat = array match {
		  a: Int => false
		  [1, 2, 3] => true
		  Cons(x, _) =>
		  {k: v, _} => false
		  r@ 1..3 => true
		  1 | 2 | 3 => true
		  x if x > 0 => false
		}

	-- Chaining comparisons (Julia, Python)
	   1 < 2 <= 2 < 3 == 3 > 2 >= 1 == 1 < 3 != 5


	-- Everything is {}
	-- Lambda
	   val lam = { x => x + 1 }
	   val patLambda = { (Context x _) value => value + x }
	   val patMat = {
		 (Context x None)   value          => value + x
		 (Context _ Some(y) value if y > 0 => value + y
		 (Context _ Some(y) value          => value + y + 1
	   }
	-- do block
	  val list = [1 .. 3]
  val doubled = do { i <- list; pure (i * i) } -- [1, 4, 9]
  }

Code Example
------------

.. code:: scala

	package test

	import something.{Data => D}, D._

	pi: Float64 = D.pi

	def len(d) = d * pi

	def len(Num n => d: n): Float64 = d.toFloat64 * pi

	-- {d: n | d > 0 }
	def len (Num n => d: n): Float64 = d.toFloat64 * pi

	-- arguments are either inferred or dynamicly typed
	-- { x: Int, y: Int, z: List Int | size z > 0 and x + y > 0 }
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
			elem <- z  -- do-block
			pure elem.toString
		}

		i := lambda i // variable assignment
	  }
	}
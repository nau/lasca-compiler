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
    
    data Person a
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


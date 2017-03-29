Side Effects
=====

http://softwareengineering.stackexchange.com/questions/15269/why-are-side-effects-considered-evil-in-functional-programming

Writing your functions/methods without side effects - so they're pure functions - 
makes it easier to reason about the correctness of your program.

It also makes it easy to compose those functions to create new behaviour.

It also makes certain optimisations possible, 
where the compiler can for instance memoise the results of functions, or use Common Subexpression Elimination.

Edit: at Benjol's request: 
Because a lot of your state's stored in the stack 
(data flow, not control flow, as Jonas has called it here), 
you can parallelise or otherwise reorder the execution of those parts of your computation 
that are independent of each other. 
You can easily find those independent parts because one part doesn't provide inputs to the other.

In environments with debuggers that let you roll back the stack and resume computing (like Smalltalk),
having pure functions means that you can very easily see how a value changes, 
because the previous states are available for inspection. 
In a mutation-heavy calculation, unless you explicitly add do/undo actions to your structure or algorithm, 
you cannot see the history of the computation. 
(This ties back to the first paragraph: writing pure functions makes it easier to inspect the correctness of your program.)


Optimizations
====
# Common Subexpression Elimination

Looks like LLVM does it with Global Value Numbering

# Automatic Memoization
  
Annotations? Caches?

```scala
    @cache(type=[LRU/LFU], size=100)
    def calculate(∀ a. Num a => x: a, y: a, z: a): a = x * y * z  
    def calculate(∀ a. Num a => x: a, y: a, z: a): a = x * y * z  
```

# Automatic parallelisation

Annotations? Macros?

```scala
    @par
    def calculate(∀ a. Num a => x: a, y: a, z: a): a = x * y * z  
```
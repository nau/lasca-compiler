WebLang
==

- Statically typed, System F, type inference
- ADT, data, array, val/var and functions
- Type classes/traits, Go/Rust-like
- Method syntax
- Any type (Scala Dynamic alike, Go interface{})
- Indentation based syntax 
- Future/Promise API
- Continuation Passing Style, CPS        
- Communicating Sequential Processes, CSP
- Packages
- Compiler API
- JavaScript interoperability
- JS backend (Node.js)

Examples
==
```haskell
    package Test

    val node = import "nodejs"
        
    trait Storage = 
      def save(storage: Self, x: ToJson)
    
    data Person = 
      firstName: String
      lastName: String
      
    def toJson(p: Person) = p  
      
    def save(storage: File, p: Person) = 
      storage.saveToFile(p)    
      
    val p = {firstName: "Alex", lastName: "Nemish"}
    val fileStorage = FileStorage()
    fileStorage.
    
    
    def foo[ToJson p, Hash p](person: p): Something
       
```

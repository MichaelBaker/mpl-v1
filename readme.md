# MPL

## TODO Chunks

- [X] Write iterator over Cofree that makes the annotation available at each stage
- [X] Generate a complete LLVM module
- [X] Generate valid JS for Untyped
- [X] Create automated test for generated JS
- [X] Generate valid LLVM for Untyped
- [X] Display state line in REPL
- [X] Handle wrapping correctly in REPL
- [ ] Create JIT LLVM REPL
- [ ] Create JS REPL (Maybe with V8 bindings?)
- [ ] Abstract REPL over backend
- [ ] Create automated test for generated LLVM
- [ ] Move errors into the compiler functions, rather than using Haskell's `error`
- [ ] Generate valid JS for Typed
- [ ] Generate valid LLVM for Typed
- [ ] Add typechecking phase for Typed
- [ ] Add pretty printing for Untyped
- [ ] Add pretty printing for Typed
- [ ] Use the pretty printer for auto formatting files
- [ ] Create the base Unmanaged syntax
- [ ] Create the base Unmanaged parser
- [ ] JS backend for Unmanaged
- [ ] LLVM backend for Unmanaged

## TODO Think about

- [ ] How to handle FFI
- [ ] How to structure incremental parsing (only parsing changed parts of a file and updating the otherwise already processed tree)
- [ ] How should the memory API be for Unmanaged. What guarantees should its type system encode?
- [ ] How can Untyped code call Typed code without any extra ceremony?
- [ ] How can Typed code call Unmanaged code without any extra ceremony?
- [ ] How can Untyped code call Unmanaged code without any extra ceremony?

## Language Facilities

### Common

- [ ] Solution/Program composition (Function Application/Arguments): Given existing solutions (programs), the language makes it easy to combine them to solve a larger problem. This is important for productivity and correctness. It makes us more productive because it lets us use solutions that already exist, which saves us the time of solving their problem again. It makes us more correct because _if_ the existing solution has been well tested, then using it prevents us from making all of the errors that the creator of the existing solution has overcome.
- [ ] Solution generification (Function Parameters): A solution to a specific problem e.g. What is the integer after 2?, can be parameterized to solve a large class of problems e.g. What is the integer after x, where x is an integer? This has all of the same benefits as function application because arguments and parameters are two sides of the same coin. Together they provide a general framework for code reuse.

### Untyped
### Typed
### Unmanaged

## Philosophy

* Programs are solutions to problems. You can interpret every program as being a machine that solves a specific problem. This is a useful idea because it helps you escape the trap of thinking about a programs as merely a way to shuffle symbols or give instructions to the computer. It refocuses you on why you're writing the program in the first place, which focuses you on making pragmatic choices.

## Compiler Structure

### Compilation Phases

```
InFile --Parse--> Syntax --Backend--> Outfile
```

### Shared Structures

```
Common -----> Untyped
         |
          --> Typed
```


## Concerns to Maintain

* Clear error messages with advice
* Fast, incremental compilation
* Live code reloading and interactive programming
* Separation of knowledge needed to work in a sub-language
* As much shared syntax as possible
* Safe defaults

## Notes

### Boundary Quadrants

* Bounded in time and space:         Statically determined termination and allocation.
* Bounded in space but not time:     Infinite tail call recursion and loops.
* Bounded in time but not space:     Doesn't exist?
* Bounded in neither time nor space: Infinite non-tail call recursion.

### Hot Swapping

Global support for changing bindings at runtime gives you a rapid feedback loop, live experimentation, and a facility for zero downtime upgrades. The ability to change any binding at runtime needs to be assumeda at the language level for it to be convenient.

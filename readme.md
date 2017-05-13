# MPL

## Dependendies

* v8: stable 5.1.281.47
* libffi: stable 3.0.13
* llvm: 3.5

## What do I need to actually get a prototype out the door?

- [X] A Binder data type that is distinct from expressions
  * Needed to allow type annotations to bindings.
  * Demonstrates that the recursion-schemes framework I'm using will work when expressions aren't homogenous and potentially include sub syntaxes which are recursive in their own right.
- [X] Type checking phase for Typed
  * I can implement this is now because I have integers and functions, which are distinct.
- [X] Use Core types instead of types dedicated to typechecking
- [X] Annotate types with source spans for better error messages
- [X] Add explanations for inferred types.
- [ ] Strings and string literals
- [ ] Using a function from a Typed module in Untyped.
  * Requires figuring out imports
- [ ] Records
  * Necessary for data composition.
- [ ] Eliminators
  * Necessary to operate on composed data.
- [ ] Variants/Cases (necessary for booleans)
  * Necessary for program composition. They allow you to dispatch a given input to a number of different programs while syntactically guaranteeing that all inputs will be handled.
  * This will enable a uniform control flow construct instead of having one unique to booleans.
- [ ] Conditionals/Control structures (thunks)
  * Necessary for program composition in the presence of side effects and for efficiency.
  * If you have side effects, you need to be able to express a computation that you want to run, without actually running it. Otherwise all side effects will be generated in the entire program.
  * In a strict language, you need the ability to control which expressions get executed so that your program doesn't end up doing more work than you intended.
- [ ] Javascript FFI
  * If I have this, then I can implement non-trival programs by leaning on JS libraries like React.

## Steps not yet represented in the new compiler pipeline

- [ ] Parsing
- [ ] Javascript Backend

## Backlog

- [ ] Fix source annotations for grouping parentheses
- [ ] Move errors into the compiler functions, rather than using Haskell's `error`. Use ExceptT.
- [ ] Add modules
- [ ] Add to Untyped for using identifiers that aren't in scope.
- [ ] Add explanations for the inclusion of various features
- [ ] Add bindings for top level identifiers to Common
- [ ] Add pretty printing for Untyped
- [ ] Add pretty printing for Typed
- [ ] Use the pretty printer for auto formatting files
- [ ] Create the base Unmanaged syntax
- [ ] Create the base Unmanaged parser
- [ ] JS backend for Unmanaged
- [ ] Use examples to generate syntax error heuristics: https://research.swtch.com/yyerror
- [ ] Pull in big decimal/integer library for IEEE JS math
- [ ] Create formal proofs of the type checker etc...
- [ ] String interpolation. What about a more generic syntax for compile time programming?
- [ ] Multiline strings

### Done

- [X] Write iterator over Cofree that makes the annotation available at each stage
- [X] Generate a complete LLVM module
- [X] Generate valid JS for Untyped
- [X] Create automated test for generated JS
- [X] Generate valid LLVM for Untyped
- [X] Display state line in REPL
- [X] Handle wrapping correctly in REPL
- [X] Create JIT LLVM REPL
- [X] Create JS REPL (Maybe with V8 bindings?)
- [X] Create automated test for generated LLVM
- [X] Add lambda syntax to Common
- [X] Add syntax for multiple arguments
- [X] Add parsing for infix functions to Common (`+ for left associative and +` for right associative)
- [X] Convert custom parser into a Monad to extract all of the context passing
- [X] Make all syntax constructors parametric in their fixed points
- [X] Add original source locations to AST nodes
- [X] Generate nice syntax errors
  - [X] Convert all Strings to Docs for highlighting
  - [X] Add function to process errors
  - [X] Add suggestion to Err
  - [X] Be able to gather all text from start of a perser up to the error
- [X] Generate valid JS for functions
- [X] Add native functions (functions with literal js/llvm output)
- [X] Add name resolution for Untyped
- [X] Add typechecking phase for Typed
- [X] Generate valid JS for Typed
- [X] Add a Core language that the syntax can be translated into and is simpler to work with

## To Think about

- [ ] Information hiding and backwards compatibility. Should breaking backwards compatibilty be impossible or at least exception rather than rule?
- [ ] What is the ideal place for errors to arise? I'm currently thinking that parsing should be almost sure to succeed that way you have a lot of context for producing nice errors.
- [ ] How to make _everything_ ad hoc/inlineable (like lambdas are ad hoc functions as opposed to being already declared)
- [ ] Syntax as algebra. How to make a syntax that allows for easily thinking about composition and transformation.
- [ ] How to handle FFI
- [ ] How to structure incremental parsing (only parsing changed parts of a file and updating the otherwise already processed tree)
- [ ] How should the memory API be for Unmanaged. What guarantees should its type system encode?
- [ ] How can Untyped code call Typed code without any extra ceremony?
- [ ] How can Typed code call Unmanaged code without any extra ceremony?
- [ ] How can Untyped code call Unmanaged code without any extra ceremony?
- [ ] Composition is helped when you can logically compose things without physically (syntactically) compose them. This is the value of `let` and `where`.
- [ ] Can I make the monadic parser bidirectional so that code generation and parsing are always in sync?

## Language Facilities

### Common

- [ ] Solution/Program composition (Function Application/Arguments): Given existing solutions (programs), the language makes it easy to combine them to solve a larger problem. This is important for productivity and correctness. It makes us more productive because it lets us use solutions that already exist, which saves us the time of solving their problem again. It makes us more correct because _if_ the existing solution has been well tested, then using it prevents us from making all of the errors that the creator of the existing solution has overcome.
- [ ] Solution generification (Function Parameters): A solution to a specific problem e.g. What is the integer after 2?, can be parameterized to solve a large class of problems e.g. What is the integer after x, where x is an integer? This has all of the same benefits as function application because arguments and parameters are two sides of the same coin. Together they provide a general framework for code reuse.

### Untyped
### Typed
### Unmanaged

## Philosophy

* Programs are solutions to problems. You can interpret every program as being a machine that solves a specific problem. This is a useful idea because it helps you escape the trap of thinking about a programs as merely a way to shuffle symbols or give instructions to the computer. It refocuses you on why you're writing the program in the first place, which focuses you on making pragmatic choices.
* Keep as much context as you can during each compilation phase to support the generation of rich error messages and editor support.

## Compiler Structure

## Concerns to Maintain

* Clear error messages with advice
* Fast, incremental compilation
* Live code reloading and interactive programming
* Separation of knowledge needed to work in a sub-language
* As much shared syntax as possible
* Safe defaults

## Notes

### Metrics for what makes a "good" language

* Is it easy to understand the performance impact of a decision?
* Is it easy to understand the memory impact of a decision?
* Is it efficient to write? How much effort and code need to be expended on conerns not related to the programmer's problem?
* Is it efficient to read? Can ideas be represented at different levels of fidelity in order to make grasping intent easier?
* Are the defaults safe? Does it mitigate how bad unintentional errors can be?
* Is it modular? Can independent pieces of software be understood in isolation?

### Boundary Quadrants

* Bounded in time and space:         Statically determined termination and allocation.
* Bounded in space but not time:     Infinite tail call recursion and loops.
* Bounded in time but not space:     Doesn't exist?
* Bounded in neither time nor space: Infinite non-tail call recursion.

### Hot Swapping

Global support for changing bindings at runtime gives you a rapid feedback loop, live experimentation, and a facility for zero downtime upgrades. The ability to change any binding at runtime needs to be assumeda at the language level for it to be convenient.

### Garbage Collection

* Overhead/Throughput: How much of the execution time is spent on garbage collection?
  * overhead = operations per object * cost per operation
  * Finding a way to make the operations per object sublinear would be the biggest win.
  * Allocate and free in large chunks to reduce the number of operations per object.
  * Use very cheap operations for allocation and freeing to reduce the time per operation.
* Latency: How long will the main program need to wait between operations for the garbage collector to run?
  * Running collection concurrently with the main program means more, shorter pauses.
* Predictability: Can you tell or control where pauses will happen?

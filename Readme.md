# MPL

This is the project I use to play around with programming language theory and implementation when the mood strikes me.

## Topics in programming language design

### Programming Language For Human Communication

Programmers working in teams need to communicate with each other through the code that they write.

* Comments provide a non-rigorous, but completely general tool for communicating with future programmers. You can use the full power of human language to express yourself, but none of it can be checked. It can all be a lie.
  * To build on this, a good tool chain for a language would be able to support much richer commenting facilities. Images, video, structured text, and hyperlinks enhance the expressiveness of natural language.
* Runnable examples can provide a machine verified method of communicating how to use some code. This is less expressive, but has the benefit of being verifiably correct. The downside to this approach is that you have to run the code in order to verify it, which has performance implications as well as making the verification step potentially side effectful and dangerous.
* Automated tests are effectively the same as runnable examples except that they include code which checks the correctness of the examples and they tend to be focused more on verification than communication.
* Type systems express constraints of the program that can be checked without running the code that they describe. The advantages of this are efficiency, as well as totality. Examples can only check specific cases for correctness, but types can guarantee properties of all ways that a piece of code might be run. The downside is expressiveness. A type system must be rigid enough for an algorithm to check it. It should probably also be incapable of performing side effects to that the types can be checked safely.
* Why don't we use reverse polish notation for functions so that they read left to right (evaluation order)?

### Machine model

This is the mental model I use for the way the computer actually works. How I think about the computer will determine how programming language features are implemented.

### Memory representation and management within the language

* What are factors of memory management are important to efficiency?
* What factors of memory management are important to safety?
* It should be possible, if necessary, to get lower level access to memory. This will hopefully prevent people from needing to choose worse languages for "efficiency" reasons.
* It should also be possible to use a garbage collector or other kinds of automatic memory management if you value their simplicity over efficiency (which I think is most of the time).
* Different mechanisms for memory management should compose trivially. You should only have to think about memory management when you care.

### Tool chain and development experience

* Visibility. This means being able to poke at it at runtime to see what it's currently doing and what it would do if you made minor tweaks.
* Feedback cycle. How quickly you can make and test new changes will effect how efficiently you can write software. A short feedback cycle also protects you against taking shortcuts out of laziness.
* Experimentation. The tool chain should not stand in your way if you want to quickly test an idea. REPLs are one of the primary tools that facilitate this. They allow you to try an idea with zero boilerplate.

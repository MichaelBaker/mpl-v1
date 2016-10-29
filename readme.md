# MPL

## Compiler Structure

```
File --Parse--> Syntax --Desugar--> Term --CodeGen--> Module --Linker----> Executable
                                                                      |
                                                                       --> Interpret
```

## Concerns to Maintain

* Clear error messages with advice
* Fast, incremental compilation
* Live code reloading and interactive programming
* Separation of knowledge needed to work in a sub-language
* As much shared syntax as possible
* Safe defaults

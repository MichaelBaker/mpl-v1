# MPL

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

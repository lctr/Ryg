# Ryg
A high-level programming language inspired by Lisp, Haskell and Rust. Featuring static typing, pattern matching<sup>1, 2</sup>, list-comprehension, first-class functions, and an integrated REPL. 

Ryg is the continuation of a prior TypeScript-based project language  whose name (*Wyg*) was inspired by the acronym *WYSIWYG*. Aptly named due to the runtime environment being implemented in *Rust*. 

### Notes
1. Currently, superficial pattern matching only over token trees in let bindings, lambda expressions/calls, and function expressions/calls, and distinguish between primitives and indexed collections. Functionality similar to that of JavaScript's variable destructuring.
2. Enums, lists, iterators, and custom types not yet implemented

# Outline
## Primitive types
* Bool
* Int
* Float
* Byte
* Char
* String
## Indexed types
* List
* Vector
* Tuple
* Record
## Compound types
* Struct
* Union
* Intersection
## Let bindings
```hs
  let 
    (a, b, c) = (2, 3, 15) 
    hypot = |x, y| x ** 2 + y ** 2, 
  in 
    hypot(a, b) == c ** 2;
```
## Closures
### Lambdas
### Functions
## Control flow
### Conditional expressions
Conditional expressions are not required to include an `else` clause, *however* the lack of an `else` clause leaves an implicit `nil` in the event the `if` clause does not evaluate true.
``` hs
  if true then print'ln ("I'm here!") 
          else print'ln ("I'll never show up");
```
The above can be written as
``` hs
  print'ln (if true then "I'm here!" 
                    else "I'll never show up");
```

### Case expressions
``` hs
  fn feels'like |temp| case temp of {
    temp < 70 => "cold",
    70 <= temp || temp <= 80 => "cool",
    80 <= temp || temp <= 90 => "warm",
    temp > 90 => "hot",
    _ => "don't care to know";
  };
```
### Do expressions
### Loop expressions
# SIMP
SIMP is a very small imperative language with Scheme-like syntax which compiles down to PRIMP code

It has similar style to languages like C++ and Java in that source files are a sequence of functions.

if the `main` function is defined, it will be automatically run by convention.

## Syntax
A SIMP function looks like the following:

```scheme
(fun (f x y) ;; f is the name of the function, x y .... and more are its parameter list
     (vars [(i 10)(j 10)] ;; we define variables-value pairs which we may wish to use in the function
     (set i (+ (* j x) y)) ;; the following are a set of statements
     (return (* i i)))) ;; return the computed value
```

A SIMP function contains a sequence of statements, which are defined recursively as follows in haskell syntax for clarity

```haskell
    stmt = (print aexp)
    | (print string)
    | (set id aexp)
    | (seq stmt ...)
    | (iif bexp stmt stmt)
    | (skip)
    | (while bexp stmt ...)

    aexp = (+ | * | - | div | mod aexp bexp)
    | number
    | id
    
    bexp = (= | > | < | >= | <= aexp bexp)
    | (not bexp)
    | (and | or bexp ...)
    | true | false
```

For a complete example of SIMP source code, see test1.simp

# PRIMP

PRIMP is a sudo assembly language which is very similar to MMIX or MIPS.

I included an emulator for running PRIMP

# Example

To actually run the compiler, assembler, and emulator, you need a distribution of Racket

```bash
racket simp.rkt test1.simp .... # simp source files to run
```

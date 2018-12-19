# SIMP

SIMP is a very small imperative language with Scheme-like syntax which compiles down to PRIMP code

## Syntax
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

# PRIMP

PRIMP is a sudo assembly language which is very similar to MMIX or MIPS.

I included an emulator for running PRIMP

# Example

To actually run the compiler, assembler, and emulator, you need a distribution of Racket

```bash
racket primp.rkt < test1.simp
```

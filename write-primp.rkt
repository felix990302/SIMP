#lang racket

(provide write-primp)

(define (write-primp lst out)
  (displayln "(" out)
  (for ([instr lst])
    (writeln instr out))
  (displayln ")" out))

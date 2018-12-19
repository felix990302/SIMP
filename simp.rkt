#lang racket

(require "simulator.rkt" "assembler.rkt" "compiler.rkt")

(define source-files (current-command-line-arguments))

(for ([source-file source-files])
    (define in (open-input-file source-file))
    (load-primp (primp-assemble (compile-simp (read in))))
    (close-input-port in)
    (run-primp))

#lang racket

(require "primp.rkt" "assemble.rkt" "compile.rkt")

(define source-files (current-command-line-arguments))

(for ([source-file source-files])
    (load-primp (primp-assemble (compile-simp (read (open-input-file source-file)))))
    (run-primp))

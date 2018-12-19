#lang racket

(require "simulator.rkt")

(define source-files (current-command-line-arguments))

(for ([source-file source-files])
    (define in (open-input-file source-file))
    (load-primp (read in))
    (run-primp)
    (close-input-port in))

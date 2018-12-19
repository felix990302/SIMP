#lang racket

(require "compiler.rkt" "write-primp.rkt")

(define source-files (current-command-line-arguments))

(for ([source-file source-files])
    (define in (open-input-file source-file))
    (define out (open-output-file (string-append source-file ".aprimp") #:exists 'replace))
    (define source (read in))
    (write-primp (compile-simp source) out)
    (close-input-port in)
    (close-output-port out))

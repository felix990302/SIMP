#lang racket

(require "assembler.rkt" "write-primp.rkt")

(define source-files (current-command-line-arguments))

(for ([source-file source-files])
    (define in (open-input-file source-file))
    (define out (open-output-file (string-append source-file ".primp") #:exists 'replace))
    (write-primp (primp-assemble (read in)) out)
    (close-input-port in)
    (close-output-port out))

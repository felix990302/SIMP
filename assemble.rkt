#lang racket

(provide primp-assemble)

;; helper data structures
(struct const (val) #:transparent)
(struct label (loc) #:transparent)
(struct data (loc) #:transparent)
(struct found (key val) #:transparent)
(struct lit (val) #:transparent)


;; temporary lookup for constants, labels, data 
(define NAMES (make-hash empty))

;; immediate values
(define (imm? imm)
  (or (number? imm) (boolean? imm)))


;; nested look-up for constants, assumes there is no circular definitions
(define (fetch* psymb/val)
  (cond
    [(imm? psymb/val) psymb/val]
    [else
     (match (hash-ref NAMES psymb/val empty)
       [(? empty? v) (found psymb/val empty)]
       [(label loc) loc]
       [(const val) (fetch* val)]
       [(data loc) loc]
       [x x])]))


;; wrapper for nested fetch
;; assumes all variables have already been declared
(define (fetch-psymb/val* name psymb/val)
  (define res (fetch* psymb/val))
  (cond
    [(imm? psymb/val) psymb/val]
    [(found? res)
     (if (equal? (found-key res) name)
         (error (format "circular: ~a" psymb/val))
         (error (format "undefined: ~a" psymb/val)))]
    [(equal? name res)
     (error (format "circular: ~a" psymb/val))]
    [else res]))


;; wrapper for nested fetch
;; catches attempts for circular definition when run as variables are declared
(define (pre-fetch-psymb/val* name psymb/val)
  (define res (fetch* psymb/val))
  (cond
    [(imm? psymb/val) psymb/val]
    [(found? res)
     (if (equal? (found-key res) name)
         (error (format "circular: ~a" psymb/val))
         psymb/val)]
    [(equal? name res)
     (error (format "circular: ~a" psymb/val))]
    [else res]))


;; checks for existence of variable name before putting into set
(define (safe-put key val)
  (if
   (empty? (hash-ref NAMES key empty))
   (hash-set! NAMES key val)
   (error (format "duplicate: ~a" key))))


;; main preprocessor function 
;; wraps psymbols in appropriate structs before
(define (preprocess instr)
  (define acc empty)
  (define line-num 0)
  (for ([line instr])
    (match line
      [`(const ,psymb ,psymb/val)
       ; checks for circular definitions
       (safe-put psymb (const (pre-fetch-psymb/val* psymb psymb/val)))] 
      [`(label ,psymb)
       (safe-put psymb (label line-num))]
      [`(data ,psymb (,nat ,psymb/val))
       (safe-put psymb (data line-num))
       (for ([i nat])
         (set! acc (cons (lit psymb/val) acc))
         (set! line-num (add1 line-num)))]
      [`(data ,psymb ,vals ...)
       (safe-put psymb (data line-num))
       (for ([e vals])
         (set! acc (cons (lit e) acc))
         (set! line-num (add1 line-num)))]
      [`(lit ,v)
       (set! acc (cons (lit v) acc))
       (set! line-num (add1 line-num))]
      [`(halt) 
       (set! acc (cons (lit 0) acc))
       (set! line-num (add1 line-num))]
      [x 
       (set! acc (cons line acc))
       (set! line-num (add1 line-num))]))
  (reverse acc))


;; updates all variables with actual values
;; assumes no circular definitions
(define (finalize)
  (for ([k (hash-keys NAMES)])
    (match (hash-ref NAMES k)
      [(const val) (hash-set! NAMES k (const (fetch-psymb/val* k val)))]
      [x (void)])))


;; wrapper for fetch
(define (get-psymb/val psymb/val)
  (define res (fetch psymb/val))
  (cond
    [(found? res) (error (format "undefined: ~a" psymb/val))]
    [else res]))

;; un-nested fetch
;; assumes all fetched values are immediates
(define (fetch psymb/val)
  (cond
    [(imm? psymb/val) psymb/val]
    [else
     (match (hash-ref NAMES psymb/val empty)
       [(? empty? v) (found psymb/val empty)]
       [(label loc) loc]
       [(const val) val]
       [(data loc) loc]
       [x x])]))


;; checks for existence of key-value pair
(define (safe-get key)
  (define res (hash-ref NAMES key empty))
  (cond
    [(empty? res) (error (format "undefined: ~a" key))]
    [else res]))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; the following functions deal with appropriate opd, dest, etc in arguments for A-SIMP


(define (get-dest dest)
  (match dest
    [`(,imm ,ind) (list (fetch-imm imm) (fetch-ind ind))]
    [`(,num) (if (number? num) (list num) (error (format "incorrect: ~a" num)))]
    [x (fetch-dest x)]))

(define (fetch-dest dest)
  (match (safe-get dest)
    [(data loc) (list loc)]
    [x (error (format "incorrect: ~a" dest))]))

(define (get-opd opd)
  (cond
    [(imm? opd) opd]
    [else 
     (match opd
       [`(,imm ,ind) (list (fetch-imm imm) (fetch-ind ind))]
       [`(,num) (if (number? num) (list num) (error (format "incorrect: ~a" num)))]
       [x (fetch-opd x)])]))

(define (fetch-opd opd)
  (match (safe-get opd)
    [(const val) val]
    [(data loc) (list loc)]
    [x (error (format "incorrect: ~a" opd))]))

(define (fetch-ind ind)
  (match ind     
    [`(,num) (if (number? num) (list num) (error (format "incorrect: ~a" num)))]
    [x (fetch-dest x)]))

(define (fetch-imm imm)
  (cond
    [(imm? imm) imm]
    [else
     (match (safe-get imm)
       [(const val) val]
       [(data loc) loc]
       [x (error (format "incorrect: ~a" imm))])]))

(define (get-target opd)
  (cond
    [(imm? opd) opd]
    [else 
     (match opd
       [`(,imm ,ind) (list (fetch-imm imm) (fetch-ind ind))]
       [`(,num) (if (number? num) (list num) (error (format "incorrect: ~a" num)))]
       [x (fetch-target x)])]))

(define (fetch-target opd)      
  (match (safe-get opd)
    [(const val) (list val)]
    [(data loc) (list loc)]
    [(label loc) loc]
    [x (error (format "incorrect: ~a" opd))]))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; main assemble function
;; assumes NAMES have been updated by preprocessor
(define (assemble instr)
  (define acc empty)
  (for ([line instr])
    ;(printf "--| ~a\n" line)
    (match line
      [(lit psymb/val)
       (set! acc (cons (get-psymb/val psymb/val) acc))]
      [`(jump ,opd)
       (set! acc (cons (list 'jump (get-target opd)) acc))]
      [`(branch ,opd1 ,opd2)
       (set! acc (cons (list 'branch (get-opd opd1) (get-target opd2)) acc))]
      [`(move ,dest ,opd) 
       (set! acc (cons (list 'move (get-dest dest) (get-opd opd)) acc))]
      [`(print-val ,opd) 
       (set! acc (cons (list 'print-val (get-opd opd)) acc))]
      [`(jsr ,dest ,opd)
       (set! acc (cons (list 'jsr (get-dest dest) (get-target opd)) acc))]
      [`(print-string ,str)
       (set! acc (cons (list 'print-string str) acc))]
      [`(lnot ,dest ,opd) 
       (set! acc (cons (list 'lnot (get-dest dest) (get-opd opd)) acc))]
      [`(,op ,dest ,opd1 ,opd2)
       (set! acc (cons (list op (get-dest dest) (get-opd opd1) (get-opd opd2)) acc))]
      [x 
       (if (imm? x)
           x
           (error (format "invalid: ~a" x)))]))
  (reverse acc))


;; wrapper for preprocessor and assemble function
(define (primp-assemble instr)
  (hash-clear! NAMES)
  (define preprocessed (preprocess instr))
  (finalize)
  (assemble preprocessed))

;;(primp-assemble (compile-primp test3))


(define test
  `((jump 2)
    (data x 1)
    (lit 0)))

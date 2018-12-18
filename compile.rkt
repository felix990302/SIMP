#lang racket

;; Partnered with Jia Shi

(provide compile-simp test1)

(define fens-table (make-hash))
(define params/vars-table (make-hash))
(define temps (make-hash))
(struct function (name params vars stmts) #:transparent)


(define (preprocess fens)
  (define acc empty)
  (for ([fen fens])
    (match fen
    [`(fun (,name ,params ...) (vars [,vars ...] ,stmts ...))
     (match (last stmts)
       [`(return ,_)
        (safe-put fens-table name (length params))
        (assure-unique params vars)
        (set! acc (cons (function name params vars stmts) acc))]
       [x (error (format "no return statement: ~a!" x))])]
    [x (error "invalid function: ~a" x)]))
  (reverse acc))
 

(define (compile-simp instr)
  (hash-clear! fens-table)
  (set! label-num 0)
  (define preprocessed (preprocess instr))
  (reverse
   (cons '(data stack 0)
         (cons '(data fp stack)
               (cons '(data sp stack) 
                     (append
                      (compile-fens preprocessed)
                      '((halt))
                      (feval 'main empty)))))))


(define (safe-put table key val)
  (define res (hash-ref table key empty))
  (cond
    [(empty? res) (hash-set! table key val)]
    [else (error (format "duplicate: ~a" key))]))

(define (assure-unique lst pairs)
  (hash-clear! params/vars-table)
  (for ([temp-var lst])
    (safe-put params/vars-table temp-var 0))
  (for ([pair pairs])
    (safe-put params/vars-table (first pair) 0)
    ;(printf "params/vars: ~a\n" params/vars-table)
    ))

(define (compile-fens fens)
  (define acc empty)
  (for ([fen fens])
    ;(printf "--| compiling ~a\n" fen)
    (set! acc (append (compile-fen fen) acc)))
  acc)

(define (compile-fen fen)
  (define ind 0)
  (hash-clear! temps)
  (match fen
    [(function name params vars stmts)
     ;(printf "~a ~a ~a ~a\n" name params vars stmts)
     (for ([param params])
       (hash-set! temps param ind)
       (set! ind (add1 ind)))
     (for ([pair vars])
       (hash-set! temps (first pair) ind)
       (set! ind (add1 ind)))
     (append 
      (compile-stmts stmts)
      (compile-vars vars)
      (list (label (flabel name))))]
    [x (error (format "invalid function: ~a" x))]))



(define (symbol-prepend str symb)
  (string->symbol (string-append str (symbol->string symb))))
(define (to-var symb)
  (symbol-prepend "_" symb))
;; symb -> val -> []
;(define (data name vars)
;  (list 'data (to-var name) vars))


;; [[]]->[[]]
(define (compile-vars vars)
  (define acc empty)
  (for ([var vars])
    ;(printf "acc: ~a\n" acc)
    (match var
      [`(,name ,val)       
       (set! acc (append (push val) acc))]
      [x (error (format "invalid variable declaration: ~a\n" x))]))
  acc)


(define (loc-of var)
  (define ind (hash-ref temps var))
  (if (false? ind) (error (format "undefined variable: ~a" var))
      (list ind 'fp)))


(define label-num 0)
(define (newlabel)
  (define num label-num)
  (set! label-num (add1 label-num))
  (string->symbol (format "LABEL~a" num)))
(define (flabel f-name)
  (symbol-prepend "_" f-name))
(define (label label-name)
  (list 'label label-name))


;; [[]] -> [[]]
(define (compile-stmts stmts)
  (define acc empty)
  (for ([stmt stmts])
    (set! acc (append (compile-stmt stmt) acc)))
  acc)


;; [] -> [[]]
(define (compile-stmt stmt)
  ;(printf "~a--|called compile-stmt on: ~a\n" (incr order) stmt)
  (match stmt
    [`(skip) empty]
    [`(seq ,statements ...)
     (compile-stmts statements)]
    [`(print ,str/aexp)
     (if (string? str/aexp)
         (list (list 'print-string str/aexp))
         (cons (pop)
               (cons (list 'print-val (peek -1)) 
                     (aeval str/aexp))))]
    [`(set ,id ,aexp)
     ;(printf "~a--| called set ~a ~a\n" (incr order) id aexp)
     (cons (pop)
           (cons (list 'move (loc-of id) (peek -1))
                 (aeval aexp)))]
    [`(return ,aexp)
     (cons '(jump (-1 fp))
           (cons (pop)
                 (cons '(move (-3 fp) (-1 sp))
                       (aeval aexp))))]
    [`(iif ,bexp ,stmt1 ,stmt2)
     ;(printf "compiling (iif ~a ~a ~a)\n" bexp stmt1 stmt2)
     (define IF-TRUE (newlabel))
     (define IF-FALSE (newlabel))
     (define END (newlabel))
     (cons (label END)
           (append (compile-stmt stmt2) 
                   (cons (label IF-FALSE)
                         (cons (list 'jump END)
                               (append (compile-stmt stmt1)
                                       (cons (pop)
                                             (cons (label IF-TRUE)
                                                   (cons (list 'jump IF-FALSE)
                                                         (cons (pop)
                                                               (cons (list 'branch (peek -1) IF-TRUE)
                                                                     (beval bexp)))))))))))]
    [`(while ,bexp ,statements ...)
     ;(printf "~a--|called while ~a ~a\n" (incr order) bexp statements)
     (define LOOP-TOP (newlabel))
     (define LOOP-BODY (newlabel))
     (define LOOP-END (newlabel))
     (cons (label LOOP-END)
           (cons (list 'jump LOOP-TOP)
                 (append (compile-stmts statements)
                         (cons (pop)
                               (cons (label LOOP-BODY)
                                     (cons (list 'jump LOOP-END)
                                           (cons (pop)
                                                 (cons (list 'branch (peek -1) LOOP-BODY)
                                                       (append (beval bexp)
                                                               (list (label LOOP-TOP)))))))))))]
    [x (error (format "invalid statement: ~a\n" x))]))


(define (peek ind)
  (list ind 'sp))
(define (inc steps)
  (list 'add 'sp 'sp steps))
(define (push var/val)
  (list (inc 1) (list 'move '(0 sp) var/val)))
(define (pop)
  '(sub sp sp 1))


(define (aeval aexp) 
  ;(printf "--|called aeval on: ~a\n" aexp)
  (match aexp
    [(? number? num) (push num)]
    [(? symbol? var) (push (loc-of var))]
    ;[`(array-ref ,arr ,ind) (push (list (to-var arr) (ind-trans ind)))]
    [`(,aop ,aexp1 ,aexp2)
     (if (aop? aop)
         (cons (pop)
               (cons (list (aop-trans aop) '(-2 sp) (peek -2) (peek -1))
                     (append (aeval aexp2)
                             (aeval aexp1))))
         (feval aop (list aexp1 aexp2)))]
    [`(,f ,args ...)(feval f args)]
    [x (error (format "invalid arithemetic expression: ~a\n" x))]))


(define (feval f args)
  (define main? (equal? f 'main))
  (define res (hash-ref fens-table f empty))
  (define len (length args))
  ;(printf "--|~a\n" fens-table)
  (cond
    [(empty? res) (if main? '(0)
                      (error (format "undefined function: ~a" f)))]
    [(not (equal? res len))
     (error (format "mismatched arguments for ~a, expected ~a, got ~a" f res len))]
    [else
     (cons '(move fp (-2 fp))
           (cons '(sub sp fp 2)
                 (cons (list 'jsr '(-1 fp) (flabel f))
                       (cons (list 'sub 'fp 'sp len)
                             (append (compile-args args)
                                     (cons '(move (-2 sp) fp)
                                           (list (inc 3))))))))]))
     
(define (compile-args args)
  (define acc empty)
  (for ([arg args])
    (set! acc (append (aeval arg) acc)))
  acc)
     

(define (beval bexp)
  ;(printf "~a--|called beval on: ~a\n" (incr order) bexp)
  (match bexp
    ['true (push #t)]
    ['false (push #f)]
    [`(not ,bexp)
     (cons (list 'lnot '(-1 sp) (peek -1))
           (beval bexp))]
    [`(and ,bexps ...)
     (nest-and bexps empty (newlabel))]
    [`(or ,bexps ...)
     (nest-or bexps empty (newlabel))]
    [`(,bop ,aexp1 ,aexp2)
     (cons (pop)
           (cons (list (bop-trans bop) '(-2 sp) (peek -2) (peek -1))
                 (append (aeval aexp2)
                         (aeval aexp1))))]
    [x (error (format "invalid boolean expression: ~a\n" bexp))]))

(define (nest-and bexps acc end-label-name)
  (cond
    [(empty? bexps) (cons (label end-label-name) (append (push #t) acc))]
    [else
     (nest-and (rest bexps) (append (iif (first bexps) empty (cons (list 'jump end-label-name)(push #f))) acc) end-label-name)]))

(define (nest-or bexps acc end-label-name)
  (cond
    [(empty? bexps) (cons (label end-label-name) (append (push false) acc))]
    [else
     (nest-or (rest bexps) (append (iif (first bexps) (cons (list 'jump end-label-name) (push #t)) empty) acc) end-label-name)]))

(define (iif bexp stmt1 stmt2)
  (define IF-TRUE (newlabel))
  (define IF-FALSE (newlabel))
  (define END (newlabel))
  (cons (label END)
        (append stmt2
                (cons (label IF-FALSE)
                      (cons (list 'jump END)
                            (append stmt1
                                    (cons (pop)
                                          (cons (label IF-TRUE)
                                                (cons (list 'jump IF-FALSE)
                                                      (cons (pop)
                                                            (cons (list 'branch (peek -1) IF-TRUE)
                                                                  (beval bexp))))))))))))

(define (aop? aop)
  (or
   (equal? aop '+)
   (equal? aop '*)
   (equal? aop '-)
   (equal? aop 'div)
   (equal? aop 'mod)))
   

(define (aop-trans op)
  (match op
    ['+ 'add]
    ['* 'mul]
    ['- 'sub]
    ['div 'div]
    ['mod 'mod]
    [x (error (format "invalid arithmetic operator: ~a\n" x))]))

(define (bop-trans op)
  (match op
    ['= 'equal]
    ['> 'gt]
    ['< 'lt]
    ['>= 'ge]
    ['<= 'le]
    [x (error (format "invalid boolean operator: ~a\n" x))]))




(define test1
  '(
    (fun (nested)
         (vars [(a 5) (b 69) (answer 999999)]
         (print b)
         (print "\n")
         (print (* 5 6))
         (print "\n")
         (seq
          (iif (or (= 3 1)
                   (> 2 2)
                   (< 1 1)
                   (>= 1 2)
                   (<= 6 9)
                   false
                   (and true false)
                   (or (and (< 3 4) (>= 90 12) (<= 78 4) (or false true) true) true)
                   true)
               (set answer 999)
               (set answer 0))
          (skip)
          (while (>= a -1)
                 (print (+ answer a))
                 (print " looped once\n")
                 (set a (- a 1)))
          (print (+ 5 6))
          (print "\n")
          (print (- 5 6))
          (print "\n")
          (print (* 5 6))
          (print "\n")
          (print (div 5 6))
          (print "\n")
          (print (mod (+ (div 8 1) 7) (* 10 (- 3 5))))
          (print "\n")
          (print 69)
          (print "\n")
          (print answer)
          (print "\n"))
         (return 0)))
    (fun (main)
         (vars []
               (print (fact 5))
               (print "\n")
               (print (t 1 2 8))
               (print "\n")
               (print (f 5 6))
               (print "\n")
               (print (fib 8))
               (print "\n")
               (print (nested))
               (print "\n")
               ;;(print (main))
               (return 0)))
    (fun (fact n)
         (vars [(temp 0)]
               (iif (< n 2) (return 1)
                    (set temp (fact (- n 1))))
               (return (* n temp))))
    (fun (t z h i)
         (vars [(c 9)(s 4)(g 3)]
               (return (+ c (- s (* z (div h (mod i g))))))))
    (fun (f x y)
         (vars []
               (return (+ x y))))
    (fun (fib n)
         (vars [(temp 0)]
               (iif (< n 2) (return n)
                    (set temp (+ (fib (- n 1)) (fib (- n 2)))))
               (return temp)))
    
    
       
    ))



;(compile-simp test1)




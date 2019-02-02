#lang racket

;;part 1
; 1
(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
	   ;; complete the definition
           (if (zero? n)
               ls
               (cdr (nth-cdr (sub1 n))))
           )))
      (car (nth-cdr n)))))


;2
(define union
  (lambda (ls1 ls2)
    (match ls2
      ['() ls1]
      [`(,a . ,b) (if (list? (memv a ls1))
                      (union b ls1)
                      `(,a . ,(union b ls1)))]
      )))

;3
(define extend
  (lambda (x pred)
    (lambda (foo)
      (or (eqv? x foo) (pred foo)))))

;4
(define walk-symbol
  (lambda (sym lop)
    (match lop
      ['() sym]
      [`(,a . ,b) (let ((asval (assv sym lop)))
                    (if (pair? asval)
                        (if (symbol? (cdr asval))
                            (walk-symbol (cdr asval) lop);take care of looping
                            (cdr asval))
                        sym))])))

;part2
;5
(define lambda->lumbda
  (λ (e)
    (match e
      [`,y
       #:when (symbol? y)
       `,y]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       `(lumbda (,x) ,(lambda->lumbda body))]
      [`(,rator ,rand)
       `(,(lambda->lumbda rator) ,(lambda->lumbda rand))])))

;6
(define var-occurs?
  (λ (var e)
    (match e
      [`,x
       #:when (symbol? x)
       (eqv? x var)]
      [`(lambda (,x) ,body)
       (var-occurs? var body)]
      [`(,rator ,rand)
       (or (var-occurs? var rator) (var-occurs? var rand))])))

;7
(define vars
  (λ (e)
    (match e
      [`,y
       #:when (symbol? y)
       `(,y)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (vars body)]
      [`(,rator ,rand)
       (append (vars rator) (vars rand))])))

;8
(define unique-vars
  (λ (e)
    (match e
      [`,y
       #:when (symbol? y)
       `(,y)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (unique-vars body)]
      [`(,rator ,rand)
       (union (unique-vars rator) (unique-vars rand))])))

;9
(define var-occurs-free?
  (λ (var e)
    (match e
      [`,y
       #:when (symbol? y)
       (eqv? y var)]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (if (eqv? var x)
           #f
           (var-occurs-free? var body))]
      [`(,rator ,rand)
       (or (var-occurs-free? var rator) (var-occurs-free? var rand))])))

;10
(define var-occurs-bound?
  (λ (var e)
    (match e
      [`,y
       #:when (symbol? y)
       #f]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (or (and (eqv? x var) 
                (var-occurs-free? var body))
           (var-occurs-bound? var body))]
      [`(,rator ,rand)
       (or (var-occurs-bound? var rator) (var-occurs-bound? var rand))])))

;11
(define unique-free-vars
  (λ (e)
    (filter (lambda (var) (var-occurs-free? e var)) (unique-vars e))))

;12
(define unique-bound-vars
  (λ (e)
    (filter (lambda (var) (var-occurs-bound? e var)) (unique-vars e))))

;13
(define lex
  (λ (e acc)
    (match e
      [`,y
       #:when (symbol? y)
       `(var ,(index-of acc y))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       `(lambda ,(lex body (cons x acc)))]
       [`(,rator ,rand) `(,(lex rator acc) ,(lex rand acc))])))

;Brainteasers
;14
(define walk-symbol-update
  (λ (sym lop)
    (match lop
      ['() sym]
      [`(,a . ,b) (let ((asval (assv sym lop)))
                    (if (pair? asval)
                        (if (symbol? (unbox (cdr asval)))

                            (let ((destination (walk-symbol-update (unbox (cdr asval)) lop)))
                              (set-box! (cdr asval) destination)
                              destination)
                            
                            (unbox (cdr asval)))
                        sym))])))

;just dessert
;15
(define var-occurs-both?
  (λ (var e) 1))

(require "a2-student-tests.rkt")
(test-file #:file-name "a2.rkt")
    






      
      
#lang racket
(require rackunit)

;part I
(define lex
  (λ (e acc)
    (match e
      [`,n
       #:when (number? n)
       `(const ,n)]
      [`,y
       #:when (symbol? y)
       `(var ,(index-of acc y))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       `(lambda ,(lex body (cons x acc)))]
      [`(let ((,x ,exp))
          ,body)
       #:when (symbol? x)
       `(let ,(lex exp acc)
          ,(lex body (cons x acc)))]
      [`(sub1 ,y)
       #:when (symbol? y)
       `(sub1 ,(lex y acc))]
      [`(zero? ,y)
       #:when (symbol? y)
       `(zero? ,(lex y acc))]
      [`(* ,e1 ,e2)
       `(* ,(lex e1 acc) ,(lex e2 acc))]
      [`(if ,test ,conseq ,alt)
       `(if ,(lex test acc)
            ,(lex conseq acc)
            ,(lex alt acc))]
       [`(,rator ,rand) `(,(lex rator acc) ,(lex rand acc))])))

(check-equal? (lex '((lambda (x) x) 5)  '())
              '((lambda (var 0)) (const 5)))
(check-equal? (lex '(lambda (!) (lambda (n)
  	    (if (zero? n) 1 (* n (! (sub1 n))))))'())
              '(lambda
  (lambda
    (if (zero? (var 0))
  	(const 1)
  	(* (var 0) ((var 1) (sub1 (var 0))))))))

(check-equal? (lex '(let ((! (lambda (!)
  		   (lambda (n)
  		     (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
          ((! !) 5))
       '())
              '(let (lambda
       (lambda
  	 (if (zero? (var 0))
  	     (const 1)
  	     (* (var 0) (((var 1) (var 1)) (sub1 (var 0)))))))
  (((var 0) (var 0)) (const 5))))

;Part II
(define extend-env
  (λ (x a env)
    `((,x . ,a) . ,env)))


(define apply-env
  (λ (env y)
    (match env
      ['() (error 'var "unbound ~v" y)]
      [`((,x . ,a) . ,env)
       (if (eqv? y x) a (apply-env env y))])
    ))

(define empty-env
  (λ ()
    '()))

(define closure-ds
  (λ (x body env)
    (λ (a) (value-of-ds body (extend-env x a env)))))

(define apply-closure-ds
  (λ (clos a)
    (clos a)))

(define value-of-ds
  (λ (e env)
    (match e
      [`,n
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`,y
       #:when (symbol? y)
       (unbox (apply-env env y))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       (closure-ds x body env)]
      [`(let ((,x ,e1)) ,body)
       #:when (symbol? x)
       (value-of-ds body (extend-env x (box (value-of-ds e1 env)) env))]
      [`(+ ,e1 ,e2)
       (+ (value-of-ds e1 env) (value-of-ds e2 env))]
      [`(* ,e1 ,e2)
       (* (value-of-ds e1 env) (value-of-ds e2 env))]
      [`(if ,test ,then ,alt)
       (if (value-of-ds test env)
           (value-of-ds then env)
           (value-of-ds alt env))]
      [`(zero? ,e1)
       (zero? (value-of-ds e1 env))]
      [`(sub1 ,e1)
       (sub1 (value-of-ds e1 env))]
      [`(set! ,x ,exp)
       #:when (symbol? x)
       (let ((tmp (value-of-ds exp env)))
         (set-box! (apply-env env x) tmp))]
      [`(,rator ,x)
       #:when (symbol? x)
       (apply-closure-ds (value-of-ds rator env) (box (unbox (apply-env env x))))]
      [`(,rator ,rand)
       (apply-closure-ds (value-of-ds rator env) (box (value-of-ds rand env)))])))

(define value-of-dynamic
  (λ (e env)
    (match e
      [`,n
       #:when (number? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`,y
       #:when (symbol? y)
       (unbox (apply-env env y))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       `(λ (,x) ,body)]
      [`(let ((,x ,e1)) ,body)
       #:when (symbol? x)
       (value-of-dynamic `((lambda (,x) ,body) ,e1) env)]
      [`(+ ,e1 ,e2)
       (+ (value-of-dynamic e1 env) (value-of-dynamic e2 env))]
      [`(* ,e1 ,e2)
       (* (value-of-dynamic e1 env) (value-of-dynamic e2 env))]
      [`(if ,test ,then ,alt)
       (if (value-of-dynamic test env)
           (value-of-dynamic then env)
           (value-of-dynamic alt env))]
      [`(zero? ,e1)
       (zero? (value-of-dynamic e1 env))]
      [`(null? ,e1)
       (null? (value-of-dynamic e1 env))]
      [`(cons ,a ,b)
       (cons (value-of-dynamic a env) (value-of-dynamic b env))]
      [`(car ,ls)
       (car (value-of-dynamic ls env))]
      [`(cdr ,ls)
       (cdr (value-of-dynamic ls env))]
      [`(sub1 ,e1)
       (sub1 (value-of-dynamic e1 env))]
      [`(quote ,v) v]
      [`(set! ,x ,exp)
       #:when (symbol? x)
       (let ((tmp (value-of-dynamic exp env)))
         (set-box! (apply-env env x) tmp))]
      [`(,rator ,x)
       #:when (symbol? x)
       (match (value-of-dynamic rator env)
         [`(λ (,x) ,body)
           (value-of-dynamic body (extend-env x env (box (unbox (apply-env env x)))))])]
      [`(,rator ,rand)
       (match (value-of-dynamic rator env)
         [`(λ (,x) ,body)
           (value-of-dynamic body (extend-env x (box (value-of-dynamic rand env)) env))])]
       )))

(check-equal? (value-of-dynamic '(let ([x 2])
                       (let ([f (lambda (e) x)])
                         (let ([x 5])
                           (f 0))))
                    (empty-env))
              5)

(check-equal? (value-of-dynamic
    '(let ([! (lambda (n)
                (if (zero? n) 
                    1
                    (* n (! (sub1 n)))))])
       (! 5))
    (empty-env))
              120)

(check-equal? (value-of-dynamic
    '((lambda (!) (! 5))
        (lambda (n)
          (if (zero? n) 
              1
              (* n (! (sub1 n))))))
    (empty-env))
              120)

(check-equal? (value-of-dynamic
    '(let ([f (lambda (x) (cons x l))])
       (let ([cmap 
	      (lambda (f)
		(lambda (l)               
		  (if (null? l) 
		      '()
		      (cons (f (car l)) ((cmap f) (cdr l))))))])
	 ((cmap f) (cons 1 (cons 2 (cons 3 '())))))) 
    (empty-env))
              '((1 1 2 3) (2 2 3) (3 3)))

;Brainteasers
;4
(define (value-of-ri init-env ext-env apply-env closure apply-closure)
 (letrec ((value-of
            (lambda (env)
              (lambda (exp)
                (match exp
                  [`,p
                   #:when (or (number? p) (boolean? p))
                   p]
                  [`,y
                   #:when (symbol? y)
                   (apply-env env y)]
                  [`(lambda (,x) ,body)
                   #:when (symbol? x)
                   (closure x body env value-of)]
                  [`(let ((,x ,e)) ,body)
                   ((value-of env)
                    `((lambda (,x) ,body) ,e))]
                  [`(if ,test ,conseq ,alt)
                   (if ((value-of env) test)
                       ((value-of env) conseq)
                       ((value-of env) alt))]
                  [`(zero? ,e)
                   (zero? ((value-of env) e))]
                  [`(sub1 ,e1)
                   (sub1 ((value-of env) e1))]
                  [`(+ ,e1 ,e2)
                   (+ ((value-of env) e1) ((value-of env) e2))]
                  [`(* ,e1 ,e2)
                   (* ((value-of env) e1) ((value-of env) e2))]
                  [`(,rator ,rand)
                   (apply-closure
                    ((value-of env) rator)
                    ((value-of env) rand)
                    ext-env)])))))
   (value-of (init-env))))

(define closure-fn-ri
  (lambda (x body env f)
    (lambda (ext-env)
      (lambda (a) ((f (ext-env x a env)) body)))))

(define closure-ds-ri closure-fn-ri)

(define apply-closure-fn-ri
  (lambda (clos x ext)
    ((clos ext) x)))

(define apply-closure-ds-ri
  apply-closure-fn-ri)

(define empty-env-fn
  (lambda ()
    (lambda (y) (error 'var "unbound ~v" y))))

(define empty-env-ds
  (lambda ()
    '()))

(define extend-env-fn
  (lambda (x a env)
    (lambda (y) (if (eqv? y x) a (apply-env-fn env y)))))

(define extend-env-ds
  (lambda (x a env)
    (cons `(,x . ,a) env)))

(define apply-env-fn
  (lambda (env y)
    (env y)))

(define apply-env-ds
  (lambda (env y)
    (match env
      ['() (error 'var "unbound ~v" y)]
      [`((,x . ,a) . ,env)
       (if (eqv? y x) a (apply-env-ds env y))])))
    

(define interpreter-fn (value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri))
(define interpreter-ds (value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri))

(check-equal? (interpreter-fn '((lambda (x) x) 5)) 5)
(check-equal? (interpreter-ds '((lambda (x) x) 5)) 5)

(check-equal? (interpreter-fn  '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
       0)) 12)

(check-equal? (interpreter-ds  '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
       0)) 12)

(check-equal? (interpreter-fn  '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))) 25)

(check-equal? (interpreter-ds  '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))) 25)

         
                   
              




      





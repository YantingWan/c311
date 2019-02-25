#lang racket
(require rackunit)

;Part I
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
	       ;; fill in lines here
               ((null? ls) '())
               ((zero? (car ls)) (k (last-non-zero (cdr ls))))
               (else (cons (car ls) (last-non-zero (cdr ls)))))
  	       )))
	(last-non-zero ls)))))

(check-equal? (last-non-zero '(1 0 2 3 0 4 5)) '(4 5))
(check-equal? (last-non-zero '(1 2 3 0 4 5)) '(4 5))
(check-equal? (last-non-zero '(0)) '())
(check-equal? (last-non-zero '(1 2 3 4 5)) '(1 2 3 4 5))

;Part II
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
      [`(let/cc ,k ,body)
       `(letcc ,(lex body (cons k acc)))]
      [`(throw ,e1 ,e2)
       `(throw ,(lex e1 acc) ,(lex e2 acc))]
      [`(sub1 ,y)
       `(sub1 ,(lex y acc))]
      [`(zero? ,y)
       `(zero ,(lex y acc))]
      [`(* ,e1 ,e2)
       `(mult ,(lex e1 acc) ,(lex e2 acc))]
      [`(if ,test ,conseq ,alt)
       `(if ,(lex test acc)
            ,(lex conseq acc)
            ,(lex alt acc))]
       [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))])))

;some test cases
(check-equal? (lex '(lambda (a)
                 (lambda (b)
                   (lambda (c)
                     (lambda (w)
                       (lambda (x)
                         (lambda (y)
                           ((lambda (a)
                              (lambda (b)
                                (lambda (c)
                                  (((((a b) c) w) x) y))))
                            (lambda (w)
                              (lambda (x)
                                (lambda (y)
                                  (((((a b) c) w) x) y)))))))))))
              '())
               '(lambda 
            (lambda 
              (lambda 
                (lambda 
                  (lambda 
                    (lambda 
                      (app (lambda
                         (lambda
                           (lambda
                             (app (app (app (app (app (var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
                       (lambda
                         (lambda
                           (lambda
                             (app (app (app (app (app (var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))

(check-equal? (lex '(let/cc k (throw k (((lambda (x) x) k) (* 5 5)))) '())
 '(letcc (throw (var 0) (app (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5))))))

;Part III--Final Version of value-of-cps
(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env-cps (make-mult-x1-continuation x2 env-cps k))]
      [`(sub1 ,x) (value-of-cps x env-cps (make-sub1-continuation k))]
      [`(zero ,x) (value-of-cps x env-cps (make-zero-continuation k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps
                                              (make-if-continuation conseq alt env-cps k))]
      [`(letcc ,body) (value-of-cps body (extend-env k env-cps) k)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env-cps (make-throw-k-continuation v-exp env-cps))]
      [`(let ,e ,body) (value-of-cps e env-cps (make-let-continuation body env-cps k))]         
      [`(var ,expr) (apply-env env-cps expr k)]
      [`(lambda ,body) (apply-k k (make-closure body env-cps))]
      [`(app ,rator ,rand) (value-of-cps rator env-cps (make-rator-continuation rand env-cps k))])))
 
(define empty-env
  (lambda ()
    '(empty-env)))
 
(define empty-k
  (lambda ()
    '(empty-k)))

(define make-mult-x2-continuation
  (lambda (v1^ k^)
    `(mult-x2-continuation ,v1^ ,k^)))

(define make-mult-x1-continuation
  (lambda (x2^ env-cps^ k^)
    `(mult-x1-continuation ,x2^ ,env-cps^ ,k^)))

(define make-sub1-continuation
  (lambda (k^)
    `(sub1-continuation ,k^)))

(define make-zero-continuation
  (lambda (k^)
    `(zero-continuation ,k^)))

(define make-if-continuation
  (lambda (conseq^ alt^ env-cps^ k^)
    `(if-continuation ,conseq^ ,alt^ ,env-cps^ ,k^)))

(define make-throw-v-continuation
  (lambda (k-rator^)
    `(throw-v-continuation ,k-rator^)))

(define make-throw-k-continuation
  (lambda (v-exp^ env-cps^)
    `(throw-k-continuation ,v-exp^ ,env-cps^)))

(define make-let-continuation
  (lambda (body^ env-cps^ k^)
    `(let-continuation ,body^ ,env-cps^ ,k^)))

(define make-rand-continuation
  (lambda (clos^ k^)
    `(rand-continuation ,clos^ ,k^)))

(define make-rator-continuation
  (lambda (rand^ env-cps^ k^)
    `(rator-continuation ,rand^ ,env-cps^ ,k^)))
    
(define apply-env
  (lambda (env-cps y k)
    (match env-cps
      ['(empty-env) (error 'value-of "unbound identifier")]
      [`(env ,a^ ,env-cps^) (if (zero? y) (apply-k k a^) (apply-env env-cps^ (sub1 y) k))]
      )))

(define apply-closure
  (lambda (clos a k)
    (match clos
      [`(clos ,body ,env-cps) (value-of-cps body (extend-env a env-cps) k)]
      )))

(define apply-k
   (λ (k v)
     (match k
       ['(empty-k) v]
       [`(mult-x2-continuation ,v1^ ,k^) (apply-k k^ (* v1^ v))]
       [`(mult-x1-continuation ,x2^ ,env-cps^ ,k^) (value-of-cps x2^ env-cps^ (make-mult-x2-continuation v k^))]
       [`(sub1-continuation ,k^) (apply-k k^ (sub1 v))]
       [`(zero-continuation ,k^) (apply-k k^ (zero? v))]
       [`(if-continuation ,conseq^ ,alt^ ,env-cps^ ,k^) (if v
                                                        (value-of-cps conseq^ env-cps^ k^)
                                                        (value-of-cps alt^ env-cps^ k^))]
       [`(throw-v-continuation ,k-rator^) (apply-k k-rator^ v)]
       [`(throw-k-continuation ,v-exp^ ,env-cps^) (value-of-cps v-exp^ env-cps^ (make-throw-v-continuation v))]
       [`(let-continuation ,body^ ,env-cps^ ,k^) (value-of-cps body^ (extend-env v env-cps^) k^)]
       [`(rand-continuation ,clos^ ,k^) (apply-closure clos^ v k^)]
       [`(rator-continuation ,rand^ ,env-cps^ ,k^) (value-of-cps rand^ env-cps^ (make-rand-continuation v k^))]       ;[else (k v)]
     )))

(define extend-env
  (lambda (a^ env-cps^)
    `(env ,a^ ,env-cps^)))

(define make-closure
  (lambda (body env-cps)
    `(clos ,body ,env-cps)))

;-----------------------------------TEST-------------------------------------------------------------
(check-equal? (value-of-cps '(letcc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k))
              5)
(check-equal? (value-of-cps '(letcc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k))
              5)
(check-equal? (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                             (lambda
                               (lambda 
                                 (if (zero (var 0))  
                                     (const 1)
                                     (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                       (empty-env)
                       (empty-k))
              1)

(check-equal? (value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
(check-equal? (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                       (empty-env)
                       (empty-k)) 4)

;----------------------brainTeasers---------------------------------------------------------------
(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))
 
(define car$ car)
 
(define cdr$
  (lambda ($) (force (cdr $))))

(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) 
              (let ((n- (sub1 n)))
                (cond
                  ((zero? n-) '())
                  (else (take$ n- (cdr$ $))))))))))

(define trib$
  (letrec ((trib$ (lambda (a)
                    (lambda (b)
                      (lambda (c)
                        (cons$ a (((trib$ b) c) (+ a b c))))))))
    (((trib$ 0) 1) 1)))

(check-equal? (take$ 7 trib$) '(0 1 1 2 4 7 13))


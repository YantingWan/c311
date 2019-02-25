#lang racket
(require rackunit)

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
    '()))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define make-mult-x2-continuation
  (lambda (v1^ k^)
    (lambda (v)
      (apply-k k^ (* v1^ v)))))

(define make-mult-x1-continuation
  (lambda (x2^ env-cps^ k^)
    (lambda (v)
      (value-of-cps x2^ env-cps^
                    (make-mult-x2-continuation v k^)))))

(define make-sub1-continuation
  (lambda (k^)
    (lambda (v)
      (apply-k k^ (sub1 v)))))

(define make-zero-continuation
  (lambda (k^)
    (lambda (v)
      (apply-k k^ (zero? v)))))

(define make-if-continuation
  (lambda (conseq^ alt^ env-cps^ k^)
     (lambda (v)
       (if v
           (value-of-cps conseq^ env-cps^ k^)
           (value-of-cps alt^ env-cps^ k^)))))

(define make-throw-v-continuation
  (lambda (k-rator^)
    (lambda (v)
      (apply-k k-rator^ v))))

(define make-throw-k-continuation
  (lambda (v-exp^ env-cps^)
    (lambda (v)
      (value-of-cps v-exp^ env-cps^ (make-throw-v-continuation v)))))

(define make-let-continuation
  (lambda (body^ env-cps^ k^)
    (lambda (v)
      (value-of-cps body^ (extend-env v env-cps^) k^))))

(define make-rand-continuation
  (lambda (clos^ k^)
    (lambda (v)
      (apply-closure clos^ v k^))))

(define make-rator-continuation
  (lambda (rand^ env-cps^ k^)
    (lambda (v)
      (value-of-cps rand^ env-cps^ (make-rand-continuation v k^)))))
    
(define apply-env
  (lambda (env-cps y k)
    (match env-cps
      ['() (error 'value-of "unbound identifier")]
      [`(env ,a^ ,env-cps^) (if (zero? y) (apply-k k a^) (apply-env env-cps^ (sub1 y) k))]
      )))

(define apply-closure
  (lambda (clos a k)
    (match clos
      [`(clos ,body ,env-cps) (value-of-cps body (extend-env a env-cps) k)]
      )))

(define apply-k
   (λ (k v)
     (k v)))

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
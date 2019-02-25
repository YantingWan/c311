#lang racket
(require rackunit)

(define value-of-cps
  (lambda (expr env-cps k^)
    (match expr
      [`(const ,expr) (apply-k k^ expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env-cps (lambda (v1)
                                                  (value-of-cps x2 env-cps (lambda (v2)
                                                                             (apply-k k^ (* v1 v2))))))]
      [`(sub1 ,x) (value-of-cps x env-cps (lambda (val)
                                            (apply-k k^ (sub1 val))))]
      [`(zero ,x) (value-of-cps x env-cps (lambda (val)
                                            (apply-k k^ (zero? val))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env-cps
                                              (lambda (bool)
                                                (if bool
                                                    (value-of-cps conseq env-cps k^)
                                                    (value-of-cps alt env-cps k^))))]
      [`(letcc ,body) (value-of-cps body (extend-env k^ env-cps) k^)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env-cps
                                            (lambda (k-rator)
                                              (value-of-cps v-exp env-cps
                                                            (lambda (val)
                                                              (apply-k k-rator val)))))]
      [`(let ,e ,body) (value-of-cps e env-cps (lambda (e-val)
                                                  (value-of-cps body
                                                               (extend-env e-val env-cps) k^)))]
                  
      [`(var ,expr) (apply-env env-cps expr k^)]
      [`(lambda ,body) (apply-k k^ (make-closure body env-cps))]
      [`(app ,rator ,rand) (value-of-cps rator env-cps (lambda (op)
                                         (value-of-cps rand env-cps (lambda (a)
                                                                      (apply-closure op a k^)))))])))
 
(define empty-env
  (lambda ()
    '()))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define apply-env
  (lambda (env-cps y k^)
    (match env-cps
      ['() (error 'value-of "unbound identifier")]
      [`(env ,a^ ,env-cps^) (if (zero? y) (apply-k k^ a^) (apply-env env-cps^ (sub1 y) k^))]
      )))

(define apply-closure
  (lambda (clos a k^)
    (clos a k^)))

(define apply-k
   (λ (k^ v)
     (k^ v)))

(define extend-env
  (lambda (a^ env-cps^)
    `(env ,a^ ,env-cps^)))

(define make-closure
  (lambda (body env-cps)
    (lambda (a k^)
        (value-of-cps body (extend-env a env-cps) k^))))

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
#lang racket
;Step 2
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
      [`(letcc ,body) (value-of-cps body (lambda (y k^^) (if (zero? y) (k^^ k^)
                                                               (apply-env env-cps (sub1 y) k^^))) k^)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env-cps
                                            (lambda (k-rator)
                                              (value-of-cps v-exp env-cps
                                                            (lambda (val)
                                                              (apply-k k-rator val)))))]
      [`(let ,e ,body) (value-of-cps e env-cps (lambda (e-val)
                                                  (value-of-cps body
                                                                (lambda (y k^) (if (zero? y)
                                                                                   (apply-k k^ e-val)
                                                                                   (apply-env env-cps (sub1 y) k^))) k^)))]
                  
      [`(var ,expr) (apply-env env-cps expr k^)]
      [`(lambda ,body) (apply-k k^ (lambda (a k^)
                         (value-of-cps body (lambda (y k^) (if (zero? y) (apply-k k^ a) (apply-env env-cps (sub1 y) k^))) k^)))]
      [`(app ,rator ,rand) (value-of-cps rator env-cps (lambda (op)
                                         (value-of-cps rand env-cps (lambda (a)
                                                                      (apply-closure op a k^)))))])))
 
(define empty-env
  (lambda ()
    (lambda (y k)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define apply-env
  (lambda (env-cps expr k^)
    (env-cps expr k^)))

(define apply-closure
  (lambda (clos val k^)
    (clos val k^)))

(define apply-k
   (λ (k^ v)
     (k^ v))) 
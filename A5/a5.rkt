#lang racket
(require rackunit)

(define empty-env
  (lambda ()
    (lambda (y)
      (error 'var "unbound ~v" y))))

(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eqv? y x)
          a
          (apply-env env y)))))

(define apply-env
  (lambda (env y)
    (env y)))

(define apply-closure
  (lambda (clos x)
    (clos x)))

(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(null? ,e1)
       (null? (val-of-cbv e1 env))]
      [`(add1 ,e1)
       (add1 (val-of-cbv e1 env))]
      [`(cons ,a ,b)
       (cons (val-of-cbv a env) (val-of-cbv b env))]
      [`(car ,ls)
       (car (val-of-cbv ls env))]
      [`(cdr ,ls)
       (cdr (val-of-cbv ls env))]
      [`(cons^ ,a ,b)
       (cons (lambda () (val-of-cbv a env)) (lambda () (val-of-cbv b env)))]
      [`(car^ ,ls)
       ((car (val-of-cbv ls env)))]
      [`(cdr^ ,ls)
       ((cdr (val-of-cbv ls env)))]
      [`(quote ,v) v]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(+ ,n1 ,n2) (+ (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(set! ,x ,e1)
       #:when (symbol? x)
       (set-box! (apply-env env x)
                 (val-of-cbv e1 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(lambda (,x) ,body) (make-closure-cbv x body env)]
      [`(let ((,x ,exp)) ,body) (val-of-cbv `((lambda (,x) ,body) ,exp) env)]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (box (val-of-cbv rand env)))])))

(define make-closure-cbv
  (lambda (x body env)
    (lambda (a) (val-of-cbv body (extend-env x a env)))))

(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(+ ,n1 ,n2) (+ (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr conseq env)
                                  (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(set! ,x ,e1)
       #:when (symbol? x)
       (set-box! (apply-env env x)
                 (val-of-cbr e1 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(let ((,x ,exp)) ,body) (val-of-cbr `((lambda (,x) ,body) ,exp) env)]
      [`(lambda (,x) ,body) (make-closure-cbr x body env)]
      [`(,rator ,x)
       #:when (symbol? x)
       (apply-closure (val-of-cbr rator env) (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env)
                                      (box (val-of-cbr rand env)))])))

(define make-closure-cbr
  (lambda (x body env)
    (lambda (a) (val-of-cbr body (extend-env x a env)))))

(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(+ ,n1 ,n2) (+ (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(set! ,x ,e1)
       #:when (symbol? x)
       (set-box! (apply-env env x)
                 (val-of-cbname e1 env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))]
      [`(let ((,x ,exp)) ,body) (val-of-cbname `((lambda (,x) ,body) ,exp) env)]
      [`(lambda (,x) ,body) (make-closure-cbname  x body env)]
      [`(,rator ,x)
       #:when (symbol? x)
       (apply-closure (val-of-cbname rator env) (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env)
                                      (box (lambda () (val-of-cbname rand env))))])))

(define make-closure-cbname
  (lambda (x body env)
    (lambda (a) (val-of-cbname body (extend-env x a env)))))

(define lookNstore
  (lambda (b)
    (let ((val ((unbox b))))
      (set-box! b (lambda () val))
      val)))

(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(+ ,n1 ,n2) (+ (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                  (val-of-cbneed conseq env)
                                  (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(set! ,x ,e1)
       #:when (symbol? x)
       (set-box! (apply-env env x)
                 (val-of-cbneed e1 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`,y #:when (symbol? y) (lookNstore (apply-env env y))]
      [`(let ((,x ,exp)) ,body) (val-of-cbneed `((lambda (,x) ,body) ,exp) env)]
      [`(lambda (,x) ,body) (make-closure-cbneed  x body env)]
      [`(,rator ,x)
       #:when (symbol? x)
       (apply-closure (val-of-cbneed rator env) (apply-env env x))]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env)
                                      (box (lambda () (val-of-cbneed rand env))))])))
(define make-closure-cbneed
  (lambda (x body env)
    (lambda (a) (val-of-cbneed body (extend-env x a env)))))

;--------test cases--------------------
(check-equal? (val-of-cbr
   '((lambda (x) (begin2 (set! x #t)
                         (if x 3 5))) #f)
   (empty-env)) 3)

(check-equal?  (val-of-cbr
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env)) 4)

(check-equal? (val-of-cbv
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env)) 3)

(check-equal? (val-of-cbr
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))
  44)

(check-equal? (val-of-cbv
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env)) 55)

(check-equal? (val-of-cbr
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env)) 44)

(check-equal? (val-of-cbv
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env)) 33)

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))

(check-equal? (val-of-cbname random-sieve (empty-env)) #f)
(check-equal? (val-of-cbneed random-sieve (empty-env)) #t)
(check-equal? (val-of-cbname
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env)) 100)
(check-equal? (val-of-cbneed
   '((lambda (z) 100)
     ((lambda (x) (x x)) (lambda (x) (x x))))
   (empty-env)) 100)

;Brainteaser
;Add some code in val-of-cbv
(define cons-test
  '(let ((fix (lambda (f)
                 ((lambda (x) (f (lambda (v) ((x x) v))))
                  (lambda (x) (f (lambda (v) ((x x) v))))))))
        (let ((map (fix (lambda (map)
                          (lambda (f)
                            (lambda (l)
                               (if (null? l)
                                   '()
                                   (cons^ (f (car^ l))
                                          ((map f) (cdr^ l))))))))))
          (let ((take (fix (lambda (take)
                             (lambda (l)
                               (lambda (n)
                                 (if (zero? n)
                                     '()
                                      (cons (car^ l) 
                                            ((take (cdr^ l)) (sub1 n))))))))))
            ((take ((fix (lambda (m)
                           (lambda (i)
                             (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5)))))

(check-equal? (val-of-cbv cons-test (empty-env)) '(1 2 3 4 5))

;my own test case
(define cons-test-fibs
  '(let ((fibs (lambda (fibs)
                (lambda (m)
                  (lambda (n)
                    (cons^ m (((fibs fibs) n) (+ m n))))))))
    (let ((take (lambda (take)
                 (lambda (n)
                   (lambda (ls)
                     (if (zero? n)
                         '()
                         (cons (car^ ls) (((take take) (sub1 n)) (cdr^ ls)))))))))
      (((take take) 10) (((fibs fibs) 0) 1)))))

(check-equal? (val-of-cbv cons-test-fibs (empty-env))
             '(0 1 1 2 3 5 8 13 21 34))
  
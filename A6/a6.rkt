#lang racket
(require rackunit)
(require racket/trace)

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

;1.
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n)
                                   (lambda (bd)
                                     (k (+ (car n) (* 2 bd)))))])))

(check-equal? (binary-to-decimal-cps '() (empty-k)) 0)
(check-equal? (binary-to-decimal-cps '(1) (empty-k)) 1)
(check-equal? (binary-to-decimal-cps '(0 1) (empty-k)) 2)
(check-equal? (binary-to-decimal-cps '(1 1 0 1) (empty-k)) 11)

;2
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls)
                       (lambda (td)
                         (k (* (car ls) td))))])))

(check-equal? (times-cps '(1 2 3 4 5) (empty-k)) 120)
(check-equal? (times-cps '(2 2 3 4 5) (empty-k)) 240)
(check-equal? (times-cps '(1 2 3 0 3) (empty-k)) 0)

;3
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps (cdr ls)
                       (lambda (td)
                         (k (* (car ls) td))))])))

(check-equal? (times-cps-shortcut '(1 2 3 4 5) (empty-k)) 120)
(check-equal? (times-cps-shortcut '(2 2 3 4 5) (empty-k)) 240)
(check-equal? (times-cps-shortcut '(1 2 3 0 3) (empty-k)) 0)

;4
(define plus-cps
  (lambda (m k)
    (k (lambda (n k)
      (k (+ m n))))))

(check-equal? ((plus-cps 2 (empty-k)) 3 (empty-k)) 5)
(check-equal? ((plus-cps ((plus-cps 2 (empty-k)) 3 (empty-k)) (empty-k)) 5 (empty-k))
              10)

;5
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9*-cps (car ls)
                          (lambda (car-val)
                            (if (equal? (car ls) car-val)
                                (remv-first-9*-cps (cdr ls)
                                                   (lambda (cdr-val)
                                                     (k (cons car-val cdr-val))))
                                (remv-first-9*-cps (car ls)
                                                   (lambda (car-val)
                                                     (k (cons car-val (cdr ls))))))))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls)
                           (lambda (cdr-val)
                             (k (cons (car ls) cdr-val))))])))

(check-equal? (remv-first-9*-cps '((1 2 (3) 9)) (empty-k)) '((1 2 (3))))
(check-equal? (remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k)) '((9 (9 (9)))))
(check-equal? (remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k)) '((((() 9) 9) 9) 9))

;6
(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls)
                            (lambda (cons-car)
                              (cons-cell-count-cps (cdr ls)
                                                   (lambda (cons-cdr)
                                                     (k (+ (add1 cons-car) cons-cdr))))))]
      [else (k 0)])))

(check-equal? (cons-cell-count-cps '(() ()) (empty-k)) 2)
(check-equal? (cons-cell-count-cps '(1 (2 (3))) (empty-k))
              5)

(check-equal? (cons-cell-count-cps '(1 2 3) (empty-k))
              3)

;7:find-cps
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr
          (find-cps (cdr pr) s (lambda (v) (k v)))
          (k u)))))

(check-equal? (find-cps 5 '((6 . a) (7 . b) (5 . c)) (empty-k))
              'c)
(check-equal? (find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
              'a)
(check-equal? (find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (empty-k))
              6)


;8: ackerman function
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

(define ack-cps
  (lambda (m n k)
    (cond
      ((zero? m) (k (add1 n)))
      ((zero? n) (ack-cps (sub1 m) 1
                          (lambda (v) (k v))))
      (else (ack-cps m (sub1 n)
                     (lambda (v-n)
                       (ack-cps (sub1 m) v-n
                                (lambda (v)
                                  (k v)))))))))

(check-equal? (ack-cps 1 1 (empty-k)) (ack 1 1))
(check-equal? (ack-cps 2 2 (empty-k)) (ack 2 2))
(check-equal? (ack-cps 3 2 (empty-k)) (ack 3 2))

;9: fib-cps
(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k)
       (fib-cps fib-cps n (lambda (v) (k v))))
     (lambda (fib-cps n k)
       (cond
	 [(zero? n) (k 0)]
	 [(zero? (sub1 n)) (k 1)]
	 [else (fib-cps fib-cps (sub1 n)
                        (lambda (a1)
                          (fib-cps fib-cps (sub1 (sub1 n))
                                   (lambda (a2)
                                     (k (+ a1 a2))))))]))
     (lambda (v) (k v)))))

(check-equal? (fib-cps 5 (empty-k)) 5)
(check-equal? (fib-cps 10 (empty-k)) 55)
(check-equal? (fib-cps 15 (empty-k)) 610)

;10
(define unfold-cps
  (lambda (p-cps f-cps g-cps seed k)
    ((lambda (h-cps k)
       (h-cps h-cps (lambda (rator)
                      (rator seed '() (lambda (v) (k v))))))
     (lambda (h-cps k)
       (k (lambda (seed ans k)
	 (p-cps seed (lambda (b)
                       (if b
                           (k ans)
                           (g-cps seed (lambda (gseed)
                                         (f-cps seed (lambda (fseed)
                                                       (h-cps h-cps (lambda (rator)
                                                                      (rator gseed (cons fseed ans)
                                                                             (lambda (v) (k v)))))
                                                              
                                         ))))
                ))))))
     (lambda (v) (k v)))
    ))

(define null?-cps
    (lambda (ls k)
      (k (null? ls))))

(define car-cps
    (lambda (pr k)
      (k (car pr))))

(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

(check-equal? (unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))
              '(e d c b a))

;11
(define empty-s
  (lambda ()
    '()))

(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s (lambda (result)
                                  (k result))))
      ((pair? u)
       (if (pair? v)
           (find-cps (car u) s
                     (lambda (arg1)
                       (find-cps (car v) s
                                 (lambda (arg2)
                                   (unify-cps arg1 arg2 s
                                              (lambda (s)
                                                (if s
                                                    (find-cps (cdr u) s
                                                              (lambda (arg1)
                                                                (find-cps (cdr v) s
                                                                          (lambda (arg2)
                                                                            (unify-cps arg1 arg2 s
                                                                                       (lambda (v)
                                                                                         (k v)))))))
                                                    #f)))))))
           #f))
      (else #f))))

(check-equal? (unify-cps 'x 5 (empty-s) (empty-k)) '((5 . x)))
(check-equal? (unify-cps 'x 5 (unify-cps 'y 6 (empty-s) (empty-k)) (empty-k))
              '((5 . x) (6 . y)))
(check-equal? (unify-cps '(x y) '(5 6) (empty-s) (empty-k)) '((6 . y) (5 . x)))
(check-equal? (unify-cps 'x 5 (unify-cps 'x 6 (empty-s) (empty-k)) (empty-k))
              '((5 . x) (6 . x)))
(check-equal? (unify-cps '(1 2 3) '(x 1 2) (empty-s) (empty-k))
              '((3 . x) (2 . x) (1 . x)))
(check-equal? (unify-cps '(x x) '(5 6) (empty-s) (empty-k))
              '((6 . x) (5 . x)))
(check-equal? (unify-cps 'x 'y (empty-s) (empty-k))
              #f)

;12: M-cps
(define M-cps
  (lambda (f-cps k)
    (k (lambda (ls k)
         (cond
           ((null? ls) (k '()))
           (else (f-cps (car ls)
                        (lambda (arg1)
                          (M-cps f-cps (lambda (rator)
                                         (rator (cdr ls)
                                                (lambda (arg2)
                                                  (k (cons arg1 arg2))))))))))))))

(check-equal? ((M-cps (lambda (n k) (k (* n n))) (empty-k)) '(1 2 3 4 5) (empty-k))
              '(1 4 9 16 25))
(check-equal? ((M-cps (lambda (n k) (k (sub1 n))) (empty-k)) '(1 2 3 4 5) (empty-k))
              '(0 1 2 3 4))

;13: use-of-M-cps
(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5) (empty-k)))

(check-equal? use-of-M-cps '(2 3 4 5 6))

;Brainteaser
;14: strange-cps
(define strange-cps
  (lambda (x k)
    ((lambda (g-cps k) (k (lambda (x k) (g-cps g-cps (lambda (v) (k v))))))
     (lambda (g-cps k) (k (lambda (x k) (g-cps g-cps (lambda (v) (k v))))))
     (lambda (v) (k v)))))

(define use-of-strange-cps
  (let ([strange^-cps (((strange-cps 5 (empty-k)) 6 (empty-k)) 7 (empty-k))])
     (((strange^-cps 8 (empty-k)) 9 (empty-k)) 10 (empty-k))))
                                        
;16: why-cps
(define why-cps
  (lambda (f-cps k)
    ((lambda (g-cps k)
       (f-cps (lambda (x k) (g-cps g-cps (lambda (rator)
                                           (rator x k))))
              (lambda (v) (k v))))
       (lambda (g-cps k)
         (f-cps (lambda (x k) (g-cps g-cps (lambda (rator)
                                           (rator x k))))
                (lambda (v) (k v))))
     
     (lambda (v) (k v)))))

(define almost-length-cps
    (lambda (f-cps k)
      (k (lambda (ls k)
        (if (null? ls)
            (k 0)
            (f-cps (cdr ls) (lambda (result)
                              (k (add1 result)))))))))

(check-equal? ((why-cps almost-length-cps (empty-k)) '(a b c d e) (empty-k))
              5)
(check-equal? ((why-cps almost-length-cps (empty-k)) '(a b c) (empty-k))
              3)

;Just Dessert: why-cps-cps
(define why-cps-cps
  (lambda (f-cps-cps k-cps k)
    ((lambda (g-cps-cps k-cps k)
       (f-cps-cps (lambda (x k-cps k) (g-cps-cps g-cps-cps (lambda (rator-cps k)
                                           (rator-cps x k-cps (lambda (v) (k v))))
                                                 (lambda (v) (k v))))
              (lambda (v k) (k-cps v (lambda (val) (k val))))
              (lambda (v) (k v))))

     (lambda (g-cps-cps k-cps k)
       (f-cps-cps (lambda (x k-cps k) (g-cps-cps g-cps-cps (lambda (rator-cps k)
                                           (rator-cps x k-cps (lambda (v) (k v))))
                                                 (lambda (v) (k v))))
              (lambda (v k) (k-cps v (lambda (val) (k val))))
              (lambda (v) (k v))))

     (lambda (v k) (k-cps v (lambda (val) (k val))))

     (lambda (v) (k v)))))

(define almost-length-cps-cps
    (lambda (f-cps-cps k-cps k)
      (k-cps (lambda (ls k-cps k)
        (if (null? ls)
            (k-cps 0 (lambda (v) (k v)))
            (f-cps-cps (cdr ls) (lambda (result k)
                              (k-cps (add1 result) (lambda (v) (k v))))
                       (lambda (v) (k v)))))
             (lambda (v) (k v)))))

(check-equal? ((why-cps-cps almost-length-cps-cps (lambda (v k) (k v)) (empty-k))
               '(a b c d e)
                (lambda (v k) (k v))
                (empty-k))
              5)

(check-equal? ((why-cps-cps almost-length-cps-cps (lambda (v k) (k v)) (empty-k))
               '(a b c)
                (lambda (v k) (k v))
                (empty-k))
              3)                  
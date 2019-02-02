#lang racket

;1: countdown
(define (countdown n)
  (cond ((< n 0) null)
        (else (cons n (countdown (sub1 n))))))

;2
(define insertR
  (λ (s1 s2 ls)
    (cond
      ((null? ls) null)
      (else (cond ((eqv? s1 (car ls)) (cons s1 (cons s2 (insertR s1 s2 (cdr ls)))))
                  (else (cons (car ls) (insertR s1 s2 (cdr ls)))))))))

;3
(define remv-1st
  (λ (s ls)
    (cond
      ((null? ls) null)
      (else (cond ((eqv? s (car ls)) (cdr ls))
                  (else (cons (car ls) (remv-1st s (cdr ls)))))))))

;4
(define list-index-ofv?
  (λ (s ls)
    (cond
      ((null? ls) (error "bad data"))
      (else (cond ((eqv? (car ls) s) 0)
                  (else (add1 (list-index-ofv? s (cdr ls))))))
      )))

;5
(define filter
  (λ (pred? ls)
    (cond
      ((null? ls) null)
      (else (cond ((pred? (car ls)) (cons (car ls) (filter pred? (cdr ls))))
                  (else (filter pred? (cdr ls)))
                  ))
      )))

;6
(define zip
  (λ (l1 l2)
    (cond
      ((or (null? l1) (null? l2)) null)
      (else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2))))
      )))

;7
(define map
  (λ (proc ls)
    (cond
      ((null? ls) null)
      (else (cons (proc (car ls)) (map proc (cdr ls))))
      )))

;8
(define append
  (λ (l1 l2)
    (cond
      ((null? l1) l2)
      (else (cons (car l1) (append (cdr l1) l2)))
      )))

;9
(define reverse
  (λ (ls)
    (cond
      ((null? ls) null)
      (else (append (reverse (cdr ls)) (list (car ls))))
      )))

;10
(define fact
  (λ (n)
    (cond
      ((zero? n) 1)
      (else (* n (fact (sub1 n))))
      )))

;11
(define memv
  (λ (s ls)
    (cond
      ((null? ls) #f)
      (else (cond
              ((eqv? s (car ls)) ls)
              (else (memv s (cdr ls)))))
      )))

;12
(define fib
  (λ (n)
    (cond
      ((zero? n) 0)
      ((= n 1) 1)
      (else (+ (fib (sub1 n)) (fib (- n 2))))
      )))

;13
;((w x) y (z))
;((w . (x . ())) . (y . ((z . ()))))

;14
(define binary->natural
  (λ (ls)
    (cond
      ((null? ls) 0)
      (else (+ (car ls) (* 2 (binary->natural (cdr ls)))))
      )))

;15
(define minus
  (λ (n1 n2)
    (cond
      ((zero? n2) n1)
      (else (sub1 (minus n1 (sub1 n2)))))
    ))

;16
(define div
  (λ (n d)
    (cond
      ((zero? d) (error "bad data"))
      ((zero? n) 0)
      (else (add1 (div (- n d) d)))
      )))

;17
(define append-map
  (λ (proc ls)
    (cond
      ((null? ls) null)
      (else (append (proc (car ls)) (append-map proc (cdr ls)))))))

;18
(define set-difference
  (λ (ls1 ls2)
    (cond
      ((null? ls2) ls1)
      (else (filter (lambda (x) (not (eqv? x (car ls2))))
                                  (set-difference ls1 (cdr ls2)))))))

;Brainteasers
;19
(define powerset
  (λ (ls)
    (cond
      ((null? ls) '(()))
      (else (append (map (lambda (subset)
                           (cons (car ls) subset)) (powerset (cdr ls)))
                    (powerset (cdr ls))))
      )))





#lang racket
(require rackunit)

(define apply-k-k #f)
(define apply-k-v #f)

(define ack-m #f)
(define ack-n #f)
(define ack-k #f)

(define depth-ls #f)
(define depth-k #f)

(define fact-n #f)
(define fact-k #f)

(define pascal-n #f)
(define pascal-m #f)
(define pascal-a #f)
(define pascal-k #f)

(define apply-k
  (lambda () ;k v
    (match apply-k-k
      [`(empty-k) apply-k-v]
      [`(ack-k ,m^ ,k^) (begin
                          (set! ack-k k^)
                          (set! ack-m (sub1 m^))
                          (set! ack-n apply-k-v)
                          (ack))]
      [`(depth-inner-k ,l^ ,k^) (begin
                                  (set! l^ (add1 l^))
                                  (if (< l^ apply-k-v)
                                      (begin
                                        (set! apply-k-k k^)
                                        (apply-k))
                                      (begin
                                        (set! apply-k-k k^)
                                        (set! apply-k-v l^)
                                        (apply-k))))]
      [`(depth-outer-k ,ls^ ,k^) (begin
                                   (set! depth-k (make-depth-inner-k apply-k-v k^))
                                   (set! depth-ls (cdr ls^))
                                   (depth))]
      [`(fact-k ,n^ ,k^) (begin
                           (set! apply-k-k k^)
                           (set! apply-k-v (* n^ apply-k-v))
                           (apply-k))]
      [`(pascal-inner-k ,a^ ,k^) (begin
                                   (set! apply-k-k k^)
                                   (set! apply-k-v (cons a^ apply-k-v))
                                   (apply-k))])))
      #|[`(pascal-outer-k ,m^ ,a^ ,k^) (v (add1 m^) a^ (make-pascal-inner-k a^ k^))]
      [`(pascal-body-k ,k^) (v 1 0 k^)])))|#
      
(define ack
  (lambda ()
    (cond
      [(zero? ack-m) (begin
                       (set! apply-k-k ack-k)
                       (set! apply-k-v (add1 ack-n))
                       (apply-k))]
      [(zero? ack-n) (begin
                       (set! ack-m (sub1 ack-m))
                       (set! ack-n 1)
                       (ack))]
      [else (begin
              (set! ack-k (make-ack-k ack-m ack-k))
              (set! ack-n (sub1 ack-n))
              (ack))])))

(define make-ack-k
  (lambda (m^ k^)
    `(ack-k ,m^ ,k^)))

(define depth
  (lambda () ;ls k
    (cond
      [(null? depth-ls) (begin
                          (set! apply-k-k depth-k)
                          (set! apply-k-v 1)
                          (apply-k))]
      [(pair? (car depth-ls))
       (begin
         (set! depth-k (make-depth-outer-k depth-ls depth-k))
         (set! depth-ls (car depth-ls))
         (depth))]
      [else (begin
              (set! depth-ls (cdr depth-ls))
              (depth))])))

(define make-depth-inner-k
  (lambda (l^ k^)
    `(depth-inner-k ,l^ ,k^)))

(define make-depth-outer-k
  (lambda (ls^ k^)
    `(depth-outer-k ,ls^ ,k^)))

(define fact
  (lambda () ;n k
    (cond
      [(zero? fact-n) (begin
                        (set! apply-k-k fact-k)
                        (set! apply-k-v 1)
                        (apply-k))]
      [else (begin
              (set! fact-k (make-fact-k fact-n fact-k))
              (set! fact-n (sub1 fact-n))
              (fact))])))

(define make-fact-k
  (lambda (n^ k^)
    `(fact-k ,n^ ,k^)))


(define pascal
  (lambda ();n m a k
    (cond
      ((> pascal-m pascal-n) (begin
                               (set! apply-k-k pascal-k)
                               (set! apply-k-v '())
                               (apply-k)))
      (else (begin
              (set! pascal-a (+ pascal-a pascal-m))
              (set! pascal-k (make-pascal-inner-k pascal-a pascal-k))
              (set! pascal-m (add1 pascal-m))
              (pascal))))))

(define make-pascal-inner-k
  (lambda (a^ k^)
    `(pascal-inner-k ,a^ ,k^)))
    
(define make-pascal-outer-k
  (lambda (m^ a^ k^)
    `(pascal-outer-k ,m^ ,a^ ,k^)))

(define make-pascal-body-k
  (lambda (k^)
    `(pascal-body-k ,k^)))

(define empty-k
  (lambda ()
    '(empty-k)))

(define ack-reg-driver
  (lambda (m n)
    (begin
      (set! ack-m m)
      (set! ack-n n)
      (set! ack-k (empty-k))
      (ack))))

(define depth-reg-driver
  (lambda (ls)
    (begin
      (set! depth-ls ls)
      (set! depth-k (empty-k))
      (depth))))

(define fact-reg-driver
  (lambda (n)
    (begin
      (set! fact-n n)
      (set! fact-k (empty-k))
      (fact))))

(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! pascal-n n)
      (set! pascal-m 1)
      (set! pascal-a 0)
      (set! pascal-k (empty-k))
      (pascal))))

(check-equal? (ack-reg-driver 2 2) 7)
(check-equal? (ack-reg-driver 3 2) 29)
(check-equal? (depth-reg-driver '(1 (2 (3 (4))))) 4)
(check-equal? (depth-reg-driver '(1 (2 (3 (4 (5 (6))))))) 6)
(check-equal? (fact-reg-driver 5) 120)
(check-equal? (pascal-reg-driver 10) '(1 3 6 10 15 21 28 36 45 55))

;brain-teaser
;I did both registerize and trampoline
(define fib
  (lambda (n k)
    (define fib-tramp-n #f)
    (define fib-tramp-k #f)
    (define apply-fib-k-k #f)
    (define apply-fib-k-v #f)

    (define fib-tramp
      (lambda ()
        (cond
          [(and (not (negative? fib-tramp-n)) (< fib-tramp-n 2))
           (begin
             (set! apply-fib-k-k fib-tramp-k)
             (set! apply-fib-k-v fib-tramp-n)
             apply-fib-k)]
          [else (begin
                  (set! fib-tramp-k (make-fib-outer-k fib-tramp-n fib-tramp-k))
                  (set! fib-tramp-n (sub1 fib-tramp-n))
                  fib-tramp)])))

    (define apply-fib-k
      (lambda ()
        (match apply-fib-k-k
          [`(empty-k ,jumpout) (jumpout apply-fib-k-v)]
          [`(fib-inner-k ,a2 ,k^) (begin
                                    (set! apply-fib-k-k k^)
                                    (set! apply-fib-k-v (+ apply-fib-k-v a2))
                                    apply-fib-k)]
          [`(fib-outer-k ,n ,k^) (begin
                                   (set! fib-tramp-k (make-fib-inner-k apply-fib-k-v k^))
                                   (set! fib-tramp-n (sub1 (sub1 n)))
                                   fib-tramp)])))

    (define make-fib-inner-k
      (lambda (a2 k^)
        `(fib-inner-k ,a2 ,k^)))
    
    (define make-fib-outer-k
      (lambda (n k^)
        `(fib-outer-k ,n ,k^)))

    (begin
      (set! fib-tramp-k k)
      (set! fib-tramp-n n)
      fib-tramp)))

(define ramp-empty-k
  (lambda (jumpout)
    `(empty-k ,jumpout)))

(define rampoline
  (lambda (th1 th2 th3)
    (rampoline (th1) (th2) (th3))))

(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (let/cc jumpout
      (rampoline
        (lambda ()
          (fib n1 (ramp-empty-k jumpout)))
        (lambda ()
          (fib n2 (ramp-empty-k jumpout)))
        (lambda ()
          (fib n3 (ramp-empty-k jumpout)))))))

(check-equal? (fib-ramp-driver 6 -1 -1) 8)
(check-equal? (fib-ramp-driver -1 6 -1) 8)
(check-equal? (fib-ramp-driver -1 -1 6) 8)

(define trib
  (lambda (n k)
    (lambda ()
      (cond
        ((< n 3) (apply-trib-k k 1))
        (else
         (trib (sub1 n)
               (lambda (a1)
                 (trib (- n 2)
                       (lambda (a2)
                         (trib (- n 3)
                               (lambda (a3)
                                 (apply-trib-k k (+ a1 a2 a3)))))))))))))
(define apply-trib-k
  (lambda (k v)
    (k v)))

(define bi-tramp
  (lambda (maybe-th1 maybe-th2 result)
    (cond
      ((empty? result) (cond
                         ((and (number? maybe-th1) (number? maybe-th2)) (list maybe-th1 maybe-th2))
                         ((number? maybe-th1) (bi-tramp #t (maybe-th2) (cons maybe-th1 '())))
                         ((number? maybe-th2) (bi-tramp (maybe-th1) #t (cons maybe-th2 '())))
                         (else (bi-tramp (maybe-th1) (maybe-th2) '()))))
      ((empty? (cdr result)) (cond
                               ((number? maybe-th1) (append result (list maybe-th1)))
                               ((number? maybe-th2) (append result (list maybe-th2)))
                               (else (if (procedure? maybe-th1)
                                         (bi-tramp (maybe-th1) #t result)
                                         (bi-tramp #t (maybe-th2) result))))))))

(define id-k
  (lambda ()
    (lambda (v) v)))

(define bi-tramp-driver
  (lambda (n1 n2)
    (bi-tramp (lambda ()
                (trib n1 (id-k)))
              (lambda ()
                (trib n2 (id-k)))
              '())))

(check-equal? (bi-tramp-driver 3 4) '(3 5))
(check-equal? (bi-tramp-driver 4 3) '(3 5))
(check-equal? (bi-tramp-driver 6 6) '(17 17))
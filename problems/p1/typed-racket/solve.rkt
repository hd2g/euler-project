#lang typed/racket

(: sum (-> (Listof Integer) Integer))
(define (sum ns)
  (foldl + 0 ns))

(: pred (-> Integer Boolean))
(define pred
  (lambda (i)
    (or (zero? (modulo i 3))
        (zero? (modulo i 5)))))

(: solve (-> Integer Integer))
(define (solve n)
  (sum
    (filter 
      pred
      (range 1 n))))

(define main
  (lambda ()
    (assert (solve 10)
            (lambda ([n : Integer]) : Boolean
              (= n 23)))
    (assert (solve 1000)
            (lambda ([n : Integer]) : Boolean
              (= n 233168)))))

(main)

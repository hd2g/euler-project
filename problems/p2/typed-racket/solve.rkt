#lang racket

(define (fibs n)
  (let go ((n n) (a 0) (b 1) (acc '()))
    (if (zero? n)
      (reverse acc)
      (go (- n 1) b (+ a b) (cons b acc)))))

(define (solve m)
  (foldl + 0 (filter (lambda (x) (zero? (modulo x 2))) (fibs (* 2 m)))))

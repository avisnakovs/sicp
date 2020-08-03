#lang racket

(define (fast-expt b n)
  (define (iter-expt b a count)
    (cond [(= count 0) a]
          [(even? count) (iter-expt (square b) a (/ count 2))]
          [else (iter-expt b (* a b) (- count 1))]))
  (iter-expt b 1 n))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

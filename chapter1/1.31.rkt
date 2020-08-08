#lang racket

(define (product term a next b)
  (if (> a b)
      1.0
      (* (term a) (product term (next a) next b))))

(define (iter-product term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (* acc (term a)))))
  (iter a 1.0))

#lang racket

(provide prime?)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (if (> (square test-divisor) n)
      n
      (find-divisor n (next test-divisor))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square x) (* x x))

(define (next x)
  (if (= x 2)
      3
      (+ 2 x)))
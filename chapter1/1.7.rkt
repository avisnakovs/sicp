#lang racket

(define (sqrt-iter guess prev x)
  (if (good-enough? prev guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? prev guess)
  (< (abs (- prev guess)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

#lang racket

(define (cube-iter guess prev x)
  (if (good-enough? prev guess)
      guess
      (cube-iter (improve guess x) guess x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? prev guess)
  (< (abs (- prev guess)) 0.001))

(define (square x)
  (* x x))

(define (cubert x)
  (cube-iter 1.0 0.0 x))

#lang racket

(provide fixed-point)
(provide average)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (begin (display "x=") (display guess) (display "; ") (display "f(x)=") (display next) (newline))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2.0))

(define (golden)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
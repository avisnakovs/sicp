#lang racket

(define (integral f a b n)
  (let ([habn (h a b n)])
    (define (simpson k)
      (* (coefficient k) (f (+ a (* k habn)))))
    (* (/ habn 3) (sum simpson 0 inc n) 1.0)))
               
(define (h a b n) (/ (- b a) n))
               
(define (inc x) (+ 1 x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (coefficient k)
  (cond ((= k 0) 1)
        ((even? k) 4)
        (else 2)))

(define (even? x)
  (= (remainder x 2) 0))

(define (cube x) (* x x x))
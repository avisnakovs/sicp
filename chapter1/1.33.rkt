#lang racket

(require "miller-rabin.rkt")

(define (accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (let ([terma (term a)])
            (if (filter terma)
                (combiner terma (accumulate filter combiner null-value term (next a) next b))
                (accumulate filter combiner null-value term (next a) next b)))))
 
(define (sum-primes a b)
  (accumulate prime? + 0 identity a inc b))

(define (inc x) (+ x 1))

(define (gcd m n) 
  (cond ((< m n) (gcd n m)) 
        ((= n 0) m) 
        (else (gcd n (remainder m n))))) 

(define (relative-prime? m n) 
 (= (gcd m n) 1)) 

(define (product-of-relative-primes n)
  (define (filter x)
    (relative-prime? x n))
  (accumulate filter * 1 identity 1 inc n))

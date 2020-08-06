#lang racket

(provide fast-prime?)
(provide try-fermat)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x) (* x x))

(define (try-fermat n a)
    (= (expmod a n n) a))

(define (fermat-test n)
  (try-fermat n (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
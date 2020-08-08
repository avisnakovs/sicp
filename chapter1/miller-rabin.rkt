#lang racket

(define (mr-prime? n times)
  (cond ((= times 0) true)
        ((mr-test n) (mr-prime? n (- times 1)))
        (else false)))

(define (mr-test n)
  (try-mr n (+ 1 (random (- n 1)))))

(define (try-mr n a)
    (= (expmod a (- n 1) n) 1))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let [rem (remainder (square (expmod base (/ exp 2) m)) m)]
           (if (= 1 rem) 0 rem)))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x) (* x x))

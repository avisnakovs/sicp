#lang racket

(require "little-fermat.rkt")

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 8)
      (begin (display n) (report-prime (- (current-inexact-milliseconds) start-time)))
      #f))

(define (report-prime elapsed-time)
  (display " --- ")
  (display elapsed-time)
  (newline))

(define (search-for-primes low up n)
  (define (search low up n)
    (cond [(= n 0) #f]
          [(and (< low up) (not (timed-prime-test low))) (search-for-primes (+ low 2) up n)]
          [(< low up) (search-for-primes (+ low 2) up (- n 1))]
          [else #f]))
  (if (even? low) (search (+ low 1) up n) (search low up n)))



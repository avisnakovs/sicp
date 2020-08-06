#lang racket

(require "little-fermat.rkt")
(require "1.21.rkt")

(define (carmichael n)
  (define (check n count)
    (cond [(= count n) #t]
          [(try-fermat n count) (check n (+ count 1))]
          [else #f]))
  (and (check n 1) (not (prime? n))))

(define (find-carmichael low up)
  (cond [(= low up) #t]
        [(carmichael low) (begin (display low) (newline) (find-carmichael (+ low 1) up))]
        [else (find-carmichael (+ low 1) up)]))


#lang racket

(define (f x)
  (cond ((< x 3) x)
        (else (+ (f (- x 1)) (* 2 (f (- x 2))) (* 3 (f (- x 3)))))))

(define (fi x)
  (if (< x 3)
      x
      (f-iter 2 1 0 (- x 2))))

(define (f-iter a b c count)
  (if (= count 0)
      a
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  
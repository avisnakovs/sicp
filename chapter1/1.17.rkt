#lang racket


(define (* a b)
  (define (mult-iter a b acc)
    (cond [(= b 0) acc]
          [(even? b) (mult-iter (double a) (halve b) acc)]
          [else (mult-iter a (- b 1) (+ a acc))]))
  (mult-iter a b 0))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

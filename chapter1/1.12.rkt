#lang racket

(define (pascal-sum n)
  (define (sum count row acc)
    (if (= count 0)
        acc
        (sum (- count 1) (next-row row) (+ acc (row-sum row)))))
  (sum n '(1) 0))

(define (row-sum row)
  (define (sum row acc)
    (if (null? row)
        acc
        (sum (cdr row) (+ (car row) acc))))
  (sum row 0))

(define (next-row previous)
  (define (next row)
    (if (pair? row)
        (cons (next-elem row) (next (cdr row)))
        row))
  (cons 1 (next previous)))

(define (next-elem row)
  (cond [(null? row) null]
        [(null? (cdr row)) (car row)]
        [else (+ (car row) (car (cdr row)))]))
  
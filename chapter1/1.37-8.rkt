#lang racket

(define (cont-frac n d k) 
  (define (frac-rec i) 
    (/ (n i) 
       (+ (d i) 
          (if (= i k) 
              0 
              (frac-rec (+ i 1)))))) 
  (frac-rec 1))

(define (euler k)
  (+ 2.0 (cont-frac
   (lambda(i) 1.0)
   ds
   k)))

(define (ds i)
  (define (find i)
    (let ([count (/ i 3)]
          [rem (remainder i 3)])
      (if (= rem 0)
          (* 2 count)
          1)))
  (find (+ i 1)))
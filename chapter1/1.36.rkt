#lang racket

(require "1.35.rkt")

(define (thousand)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (thousand-av)
  (fixed-point (lambda (x) (average (log 1000) (log x))) 2.0))


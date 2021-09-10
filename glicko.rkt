#lang racket

(provide glicko-expected)

(define q (/ (log 10) 400))

(define (G y)
  (/ (sqrt (+ 1 (* 3 (expt (* q y (/ pi)) 2))))))

(define (glicko-expected ra rda rb rdb)
  (/ (+ 1 (expt 10 (* -1/400 (G rdb) (- ra rb))))))

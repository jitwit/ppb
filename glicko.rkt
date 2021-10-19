#lang racket

(provide glicko-expected
         elo-expected
         eval->probability)

(define q (/ (log 10) 400))

(define (G y)
  (/ (sqrt (+ 1 (* 3 (expt (* q y (/ pi)) 2))))))

(define (glicko-expected ra rda rb rdb)
  (/ (+ 1 (expt 10 (* -1/400 (G rdb) (- ra rb))))))

(define (elo-expected ra rb)
  (/ (+ 1 (expt 10 (* -1/400 (- ra rb))))))

;; win probability based on evaluation:
;; https://www.chessprogramming.org/Pawn_Advantage,_Win_Percentage,_and_Elo
(define (eval->probability cp) ;; cp = centipawn
  (/ (+ 1 (expt 10 (* -1/400 cp)))))

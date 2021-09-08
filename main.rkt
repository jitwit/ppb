#lang racket

(require "chess-com.rkt"
         "glicko.rkt"
         "outils.rkt")

;; hashtables of players rating, should have rating/rd
(define (expected-score player-a player-b)
  (glicko-expected (lookup player-a 'rating)
                   (lookup player-a 'rd)
                   (lookup player-b 'rating)
                   (lookup player-b 'rd)))

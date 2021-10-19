#lang racket

(require net/http-easy
         racket/format
         "outils.rkt"
         "glicko.rkt")

(provide player-ultra-bullet
         player-bullet
         player-blitz
         player-rapid
         player-classical)

(define game-categories
  '("ultraBullet"
    "bullet"
    "blitz"
    "rapid"
    "classical"
    "correspondence"
    "chess960"
    "crazyhouse"
    "antichess"
    "atomic"
    "horde"
    "kingOfTheHill"
    "racingKings"
    "threeCheck"))

(define (player-rating-history who)
  (response-json
   (get (string-append "https://lichess.org/api/user/" who "/rating-history"))))

(define (get-player-stats who game-type)
  (response-json
   (get (string-append "https://lichess.org/api/user/" who "/perf/" game-type))))

(define (player-stats who game-type)
  (define json
    (get-player-stats who game-type))
  (define current-rating
    (lookup json 'perf 'glicko))
  (define best-rating
    (lookup json 'stat 'highest))
  (define counts
    (lookup json 'stat 'count))
  (list (lookup current-rating 'rating)
        (lookup current-rating 'deviation)
        (lookup best-rating 'int)
        (lookup counts 'win)
        (lookup counts 'draw)
        (lookup counts 'loss)))

(define (player-ultra-bullet who)
  (player-stats who "ultraBullet"))

(define (player-bullet who)
  (player-stats who "bullet"))

(define (player-blitz who)
  (player-stats who "blitz"))

(define (player-rapid who)
  (player-stats who "rapid"))

(define (player-classical who)
  (player-stats who "classical"))

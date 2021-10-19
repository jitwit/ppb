#lang racket

(require net/http-easy
         racket/format
         "outils.rkt"
         "glicko.rkt")

(provide player-profile
         player-stats
         player-blitz-rating
         player-recent-games
         recent-games-between
         expected-score)

(define (player-profile who)
  (response-json
   (get (string-append "https://api.chess.com/pub/player/" who))))

(define (player-stats who)
  (response-json
   (get (string-append "https://api.chess.com/pub/player/" who "/stats"))))

(define (player-blitz-rating who)
  (let ((stats (player-stats who)))
    (lookup stats 'chess_blitz 'last)))

(define (player-rapid-rating who)
  (let ((stats (player-stats who)))
    (lookup stats 'chess_rapid 'last)))

(define (player-recent-games who)
  (define now (seconds->date (current-seconds)))
  (define req
     (string-append "https://api.chess.com/pub/player/"
                      who
                      "/games/"
                      (number->string (date-year now))
                      "/"
                      (~a (number->string (date-month now))
                          #:align 'right
                          #:min-width 2
                          #:left-pad-string "0")))
  (lookup (response-json (get req)) 'games))

(define (game-is-between? game player-a player-b)
  (define black (lookup game 'black 'username))
  (define white (lookup game 'white 'username))
  (or (and (equal? player-a black)
           (equal? player-b white))
      (and (equal? player-a white)
           (equal? player-b black))))

(define (recent-games-between player-a player-b)
  (define games
    (player-recent-games player-a))
  (filter (lambda (game)
            (game-is-between? game player-a player-b))
          games))

;; hashtables of players rating, should have rating/rd
(define (expected-score player-a player-b)
  (glicko-expected (lookup player-a 'rating)
                   (lookup player-a 'rd)
                   (lookup player-b 'rating)
                   (lookup player-b 'rd)))

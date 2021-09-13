#lang racket

(require "chess-com.rkt"
         "glicko.rkt"
         "outils.rkt"
         "irc.rkt"

         net/http-easy
         racket/async-channel)

;; hashtables of players rating, should have rating/rd
(define (expected-score player-a player-b)
  (glicko-expected (lookup player-a 'rating)
                   (lookup player-a 'rd)
                   (lookup player-b 'rating)
                   (lookup player-b 'rd)))

(define *oauth-token*
  (symbol->string
   (with-input-from-file "token.txt"
     read)))

(define *username*
  (symbol->string
   (with-input-from-file "user.txt"
     read)))

(define C (void))
(define I (void))

(define (boot)
  (define-values (c ready)
    (irc-connect "irc.chat.twitch.tv"
                 6697
                 *username*
                 *username*
                 *username*
                 #:ssl 'auto
                 #:password (string-append "oauth:" *oauth-token*)))
  (sync ready)
  (set! C c)
  (set! I (irc-connection-incoming c))
  (display "connected to twitch yo") (newline)
  (irc-send-command C "CAP REQ" ":twitch.tv/tags")
  (irc-send-command C "CAP REQ" ":twitch.tv/membership")
  (irc-join-channel C (string-append "#" *username*)))

(define (respond-to-message where what)
  (match (string-split what)
    (`("!chesscom" ,who)
     (define profile
       (player-profile who))
     (define message
       (if (zero? (hash-ref profile 'code 1))
           (string-append "no such user: " who)
           (string-append "https://www.chess.com/play/" who)))
     (irc-send-message C
                       where
                       message))
    (_
     (write (list where what)) (newline))))

(define (gogo)
  (let loop ()
    (define message (async-channel-get I))
    (match message
      ((irc-message _ _ "PRIVMSG" `(,where ,what)  _)
       (thread
        (lambda ()
          (respond-to-message where what))))
      (_
       (write message)
       (newline)))
    (loop)))

#lang racket

(require "chess-com.rkt"
         "glicko.rkt"
         "outils.rkt"
         "irc.rkt"

         net/http-easy
         racket/async-channel
         db)

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
(define DB
  (sqlite3-connect #:database 'memory))

(define (irc-delete-message connection message)
  (match message
    ((irc-message tags pref cmd `(,chan ,msg) full-msg)
     (irc-send-message connection chan
                       (format "/delete ~a"
                               (cdr (assoc 'id tags)))))))

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
  (irc-send-command C "CAP REQ" ":twitch.tv/tags")
  (irc-join-channel C (string-append "#" *username*)))

(define (boot-db)
  (query-exec DB
              "CREATE TABLE IF NOT EXISTS chess_names (twitch_name TEXT NOT NULL, chess_com_name TEXT)"))

(define (lookup-username who)
  (query-maybe-row DB
                   "SELECT * FROM chess_names where twitch_name == $1" who))

(define (remember-username who what)
  (query-exec DB "insert into chess_names values ($1, $2)" who what))

(define (respond-to-message message where what)
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
    (`("!mynameis" ,who)
     (define profile
       (player-profile who))
     (define msg
       (if (zero? (hash-ref profile 'code 1))
           (string-append "no such user: " who)
           (string-append "username saved! " who)))
     (unless (zero? (hash-ref profile 'code 1))
       (remember-username (cdr (assoc 'display-name
                                      (irc-message-tags message)))
                          who))
     (irc-send-message C
                       where
                       msg))
    (`("!whoami")
     (define profile
       (lookup-username
        (cdr (assoc 'display-name
                    (irc-message-tags message)))))
     (irc-send-message C
                       where
                       (format "~a" profile)))
    (_
     (write (list where what)) (newline))))

(define (gogo)
  (let loop ()
    (define message (async-channel-get (irc-connection-incoming C)))
    (match message
      ((irc-message _ _ "PRIVMSG" `(,where ,what)  _)
       (thread
        (lambda ()
          (respond-to-message message where what))))
      (_
       (write message)
       (newline)))
    (loop)))

(define (gogogo)
  (let loop ()
    (define message (async-channel-get (irc-connection-incoming C)))
    (write message)
    (newline)
    (match message
      ((irc-message _ _ "PRIVMSG" `(,where ,what)  _)
       (when (string-contains? what "blaha")
         (irc-delete-message C message)))
      (_ (void)))
    (loop)))

(define (main)
  (boot-db)
  (boot)
  (gogo))

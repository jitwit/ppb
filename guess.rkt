#lang racket

(require net/http-easy
         racket/format)

(provide move-list->moves
         guess-the-piggy)

;; why t.f. ...? see
;; https://github.com/andyruwruw/chess-web-api/issues/10
;; https://github.com/andyruwruw/chess-web-api/issues/11
(define encoding
  (map cons
       (string->list
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!?")
       (range 64)))

(define files "abcdefgh")

(define promotion-encoding
  '("~{}" ;q
    "^()" ;k
    "_[]" ;r
    "#@$" ;b
    ))

(define promotion-chars
  (string->list (apply string-append promotion-encoding)))

(define pgn
  (thunk
   (response-json
    (get
     "https://www.chess.com/callback/live/game/28268957011"))))

(define (square-> char)
  (quotient/remainder (cdr (assv char encoding))
                      8))

(define (coords->square r c)
  (string-append (string (string-ref files c))
                 (number->string (+ 1 r))))

(define (from-square char)
  (define-values (r c)
    (square-> char))
  (coords->square r c))

(define (byte->move mv)
  (define to (string-ref mv 1))
  (cond ((memv to promotion-chars)
         (define-values (r c) (square-> (string-ref mv 0)))
         (define rt (if (= r 6) 7 0))
         (string-append (coords->square r c)
                        (match to
                          (#\~ (string-append (coords->square rt c) "q"))
                          (#\{ (string-append (coords->square rt (- c 1)) "q"))
                          (#\} (string-append (coords->square rt (+ c 1)) "q"))
                          (#\_ (string-append (coords->square rt c) "r"))
                          (#\[ (string-append (coords->square rt (- c 1)) "r"))
                          (#\] (string-append (coords->square rt (+ c 1)) "r"))
                          (#\^ (string-append (coords->square rt c) "n"))
                          (#\( (string-append (coords->square rt (- c 1)) "n"))
                          (#\) (string-append (coords->square rt (+ c 1)) "n"))
                          (#\# (string-append (coords->square rt c) "b"))
                          (#\@ (string-append (coords->square rt (- c 1)) "b"))
                          (#\$ (string-append (coords->square rt (+ c 1)) "b"))
                          (_ (error "parsing move: oops")))))
        (else
         (string-append (from-square (string-ref mv 0))
                        (from-square (string-ref mv 1))))))

(define (string->moves str)
  (map (lambda (j)
         (substring str (* j 2) (+ (* j 2) 2)))
       (range (/ (string-length str) 2))))

(define (move-list->moves str)
  (map byte->move (string->moves str)))

(define (pgn-key/val key val)
  (if (and val (not (hash? val)))
      (format "[~a \"~a\"]~%" key val)
      ""))

(define (lookup table . keys)
  (if (or (null? keys) (hash-empty? table))
      table
      (apply lookup
             (hash-ref table (car keys) (make-hash))
             (cdr keys))))

(define (print-headers game who nick)
  (define headers (lookup game 'game 'pgnHeaders))
  (define dr/dw (lookup game 'game 'ratingChangeWhite))
  (define dr/db (lookup game 'game 'ratingChangeBlack))
  (define white (lookup headers 'White))
  (define black (lookup headers 'Black))
  (display (pgn-key/val 'Event (lookup headers 'Event)))
  (display (pgn-key/val 'Site (lookup headers 'Site)))
  (display (pgn-key/val 'Date (lookup headers 'Date)))
  (display (pgn-key/val 'White (if (equal? white who)
                                   nick
                                   white)))
  (display (pgn-key/val 'Black (if (equal? black who)
                                   nick
                                   black)))
  (display (pgn-key/val 'Result (lookup headers 'Result)))
  ;; maybe don't show elo?
  ;; (display (pgn-key/val 'WhiteElo (lookup headers 'WhiteElo)))
  ;; (display (pgn-key/val 'BlackElo (lookup headers 'BlackElo)))
  (display (pgn-key/val 'WhiteRatingDiff dr/dw))
  (display (pgn-key/val 'BlackRatingDiff dr/db))
  (display (pgn-key/val 'TimeControl (lookup headers 'TimeControl)))
  (display (pgn-key/val 'ECO (lookup headers 'ECO))))

(define (print-time t)
  (display
   (format "{[%clk ~a]}"
           (chess.com-time->string t))))

(define (print-moves game)
  (define times
    (string-split (lookup game 'game 'moveTimestamps) ","))
  (define moves
    (move-list->moves (lookup game 'game 'moveList)))
  (for-each (lambda (m t)
              (display m)
              (display " ")
              (print-time t)
              (display " "))
            (lan->san moves)
            (take times (length moves))))

(define (print-game game who nick)
  (print-headers game who nick)
  (newline)
  (print-moves game)
  (display (lookup game 'game 'pgnHeaders 'Result)))

(define (chess.com-game->pgn game)
  (define times (lookup game 'game 'moveTimestamps))
  (define moves (lookup game 'game 'moveList))
  (define headers (lookup game 'game 'pgnHeaders))
  (map (compose chess.com-time->string string->number)
       (string-split times ",")))

(define (chess.com-time->string n)
  (let ((n (string->number n)))
    (format "~a:~a:~a.~a"
            (floor (/ n 10 60 60))
            (~a (floor (/ n 10 60))
                #:align 'right
                #:min-width 2
                #:left-pad-string "0")
            (~a (modulo (floor (/ n 10)) 60)
                #:align 'right
                #:min-width 2
                #:left-pad-string "0")
            (modulo n 10))))

(define (lan->san moves)
  (string-split
   (with-output-to-string
     (lambda ()
       (system (format "python3 moves.py ~a" (string-join moves)))))))

(define (print-answer-key pigs ids)
  (display (pgn-key/val 'Event "Guess the Piggy: KEY"))
  (display "{") (newline)
  (for-each (lambda (p id j)
              (display (format "    piggy numéro ~a: ~a~%"
                               (+ 1 j)
                               p))
              (display (format "    https://www.chess.com/game/live/~a~%" id)))
            pigs ids (range (length pigs)))
  (display "}")
  (newline)
  (newline))

(define (guess-the-piggy pigs game-ids output-file)
  (let* ((n (length pigs))
         (ordering (shuffle (range n)))
         (pigs (map (curry list-ref pigs) ordering))
         (game-ids (map (curry list-ref game-ids) ordering)))
    ;; todo add key in comments?
    (with-output-to-file output-file
      (lambda ()
        (for-each (lambda (pig id j)
                    (define game
                      (response-json
                       (get (format "https://www.chess.com/callback/live/game/~a"
                                    id))))
                    (print-game game pig (format "piggy numéro ~a" (+ 1 j)))
                    (newline) (newline) (newline))
                  pigs
                  game-ids
                  (range n))
        (print-answer-key pigs game-ids)))))



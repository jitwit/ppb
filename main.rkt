#lang racket

(require racket/async-channel
         (except-in srfi/1 delete)

         "irc.rkt"
         "outils.rkt")

(define pieces.names
  '((p . "black pawn")
    (n . "black knight")
    (b . "black bishop")
    (r . "black rook")
    (q . "black queen")
    (k . "black king")
    (P . "white pawn")
    (N . "white knight")
    (B . "white bishop")
    (R . "white rook")
    (Q . "white queen")
    (K . "white king")
    (p2 . "black pawn2")
    (b2 . "black bishop2")
    (r2 . "black rook2")
    (q2 . "black queen2")
    (k2 . "black king2")
    (P2 . "white pawn2")
    (B2 . "white bishop2")
    (R2 . "white rook2")
    (Q2 . "white queen2")
    (K2 . "white king2")))

(define pieces.figures
  '((p . "♟")
    (n . "♞")
    (b . "♝")
    (r . "♜")
    (q . "♛")
    (k . "♚")
    (P . "♙")
    (N . "♘")
    (B . "♗")
    (R . "♖")
    (Q . "♕")
    (K . "♔")
    (p2 . "♟2")
    (n2 . "♞2")
    (b2 . "♝2")
    (r2 . "♜2")
    (q2 . "♛2")
    (k2 . "♚2")
    (P2 . "♙2")
    (N2 . "♘2")
    (B2 . "♗2")
    (R2 . "♖2")
    (Q2 . "♕2")
    (K2 . "♔2")))

(define prioritized-pieces
  '(p n b r q k P N B R Q K))

(define (remove-at name)
  (if (and (non-empty-string? name) (eqv? #\@ (string-ref name 0)))
      (substring name 1)
      name))

(define names.pieces
  (map swap pieces.names))

(define pieces
  (map car pieces.names))

(define (piece->string piece)
  (let ((name (assq piece pieces.names)))
    (and name (cdr name))))

;; table mapping pieces to usernames, the core state.
(define participants
  (make-hash))

;; sorted list of pieces that have been assinged
(define (assigned-pieces)
  (filter lookup-piece pieces))

;; sorted list of assignements
(define (marbles-lineup)
  (filter-map (lambda (x)
                (let ((pp (lookup-piece x)))
                  (and pp (cons x pp))))
              pieces))

;; list of pieces that haven't been assigned
(define (available-pieces)
  (filter (compose not lookup-piece) pieces))

;; add an entry to the participants table unless there are conflicts
(define (add-participant who piece)
  (cond ((member who (hash-values participants))
         'already-assigned)
        ((not piece) ;; was #f from randomly getting a piece
         'marbles-full)
        ((not (lookup-piece piece))
         (hash-set! participants piece who) 'ok)
        (else 'piece-taken)))

;; find out what piece someone specific has
(define (participant-piece who)
  (let ((there (member who (hash-values participants))))
    (and there
         (list-ref (hash-keys participants)
                   (- (hash-count participants) (length there))))))

;; find out who has a given piece
(define (lookup-piece piece)
  (hash-ref participants piece #f))

;; remove assignment of given person
(define (remove-participant who)
  (let ((piece (participant-piece who)))
    (and piece
         (hash-remove! participants piece))))

(define (reset-marbles)
  (set! participants (make-hash)))

;; randomly return one of the available pieces or #f if none are left
(define (random-piece)
  (define available (available-pieces))
  (define first-set (lset-intersection eq? available prioritized-pieces))
  (let* ((ps (if (null? first-set) available first-set))
         (n (length ps)))
    (and (> n 0)
         (list-ref ps (random n)))))

;; configuration to log in to twitch
(define *oauth-token*
  (symbol->string
   (with-input-from-file "token.txt"
     read)))

(define *username*
  (symbol->string
   (with-input-from-file "user.txt"
     read)))

;; network connection to twitch irc network
(define twitch-connection
  (make-parameter #f))

;; semaphore to protect state among threads
(define irl-semaphore
  (make-semaphore 1))

(define (is-moderator? message)
  (equal? "1" (cdr (assq 'mod (irc-message-tags message)))))

(define (is-room-owner? message)
  (equal? (cdr (assq 'room-id (irc-message-tags message)))
          (cdr (assq 'user-id (irc-message-tags message)))))

;; allow destructive actions only by moderators or room owner (because
;; for whatever reason they don't get mod . "1" tag?)
(define (grant-permission? message)
  (or (is-moderator? message)
      (is-room-owner? message)))

;; list of commands
;  ?watch (doesn't want to join...?) , (?freeze prevent further joins)
(define commands
  '(("?play" . "?play -- join marbles")
    ("?leave" . "?leave -- quit marbles")
    ("?kick" . "?kick <user> -- (mod only) boot <user> from the game")
    ("?who" . "?who <piece> -- owner of <piece>, where <piece> is either in fen or in full (e.g. n for black knight, R2 for white rook2)")
    ("?what" . "?what [person] -- what piece you have or optionally ask what <person>'s piece is")
    ("?pieces" . "?pieces -- list of assigned pieces")
    ("?pieces-free" . "?pieces-free -- list of pieces not yet assigned")
    ("?lineup" . "?lineup -- pieces & people")
    ("?reset" . "?reset -- (mod only) reset irl marbles")
    ("?help" . "?help <command> -- information about <command>")
    ("?commands" . "?commands -- list of available commands. type ?help <command> to get information about <command>")))

;; take arguments to ?who command and figure out piece
(define (arguments->piece args)
  (match args
    (`(,fen-char)
     (string->symbol fen-char))
    (`(,color ,english-name)
     (define name.piece
       (assoc (string-join (map string-downcase (list color english-name)))
              names.pieces))
     (and name.piece (cdr name.piece)))
    (`(,color ,english-name "2")
     (define name.piece
       (assoc (string-join (map string-downcase
                                (list color
                                      (string-append english-name "2"))))
              names.pieces))
     (and name.piece (cdr name.piece)))
    (_ #f)))

;; handle commands and return response text
(define (response-message message)
  (match message
    ((irc-message tags pref "PRIVMSG" `(,where ,what) message-whole)
     (define who
       (cdr (assq 'display-name (irc-message-tags message))))
     (match (string-split what)
       ('("?play")
        (let* ((piece (random-piece))
               (result (add-participant who piece)))
          (match result
            ('ok
             (format "~a you have the ~a"
                     who
                     (piece->string piece)))
            ('already-assigned
             (format "~a you already have a piece"
                     who))
            ('marbles-full
             (format "~a marbbies is full"
                     who)))))
       ('("?leave")
        (remove-participant who)
        (format "~a left the game" who))
       (`("?kick" ,pisser)
        (let ((pisser (remove-at pisser)))
          (cond ((or (is-moderator? message)
                     (is-room-owner? message)
                     (equal? pisser who))
                 (remove-participant pisser)
                 (format "~a left the game" pisser))
                (else
                 (format "~a only moderators or ~a can kick ~a"
                         who pisser pisser)))))
       ('("?what")
        (let ((piece (participant-piece who)))
          (if piece
              (format "~a you have the ~a"
                      who
                      (and piece
                           (piece->string piece)))
              (format "~a you are not in the current irl marbbies" who))))
       (`("?what" ,pisser)
        (let* ((pisser (remove-at pisser))
               (piece (participant-piece pisser)))
          (if piece
              (format "~a has the ~a"
                      pisser
                      (and piece
                           (piece->string piece)))
              (format "~a is not in the current irl marbbies" pisser))))
       ('("?who")
        (format "~a expecting a piece, e.g. \"?who black bishop\""))
       (`("?who" . ,args)
        (let* ((piece (arguments->piece args))
               (pig (lookup-piece piece)))
          (cond ((not (member piece pieces))
                 (format "~a, got \"~a\", expecting one of: ~a"
                         who
                         (string-join args)
                         (string-join (append (map symbol->string pieces)
                                              (map cdr pieces.names))
                                      ", ")))
                (else
                 (if pig
                     (format "~a ~a has the ~a"
                             who
                             pig
                             (piece->string piece))
                     (format "~a the ~a isn't taken"
                             who
                             (piece->string piece)))))))
       (`("?force" ,who)
        (when (or (is-moderator? message)
                  (is-room-owner? message))
          (let* ((piece (random-piece))
                 (result (add-participant who piece)))
            (match result
              ('marbles-full
               (format "~a irl marbbies is full" who))
              (_ #f)))))
       ('("?pieces")
        (format "~a ~a"
                who
                (string-join (map piece->string (assigned-pieces)) ", ")))
       ('("?pieces-free")
        (format "~a remaining pieces: ~a"
                who
                (string-join (map piece->string (available-pieces)) ", ")))
       ('("?lineup")
        (format "~a the current lineup: ~a"
                who
                (string-join (map (lambda (p.w)
                                    (format "~a - ~a"
                                            (piece->string (car p.w))
                                            (cdr p.w)))
                                  (marbles-lineup))
                             " | ")))
       ('("?reset")
        (cond ((or (is-moderator? message)
                   (is-room-owner? message))
               (reset-marbles)
               (format "irl marbles has been reset by ~a" who))
              (else
               (format "~a the command \"?reset\" is only available to moderators"
                       who))))
       ('("?commands")
        (format "~a the commands are: ~a"
                who
                (string-join (map car commands) ", ")))
       (`("?help" ,command)
        (define msg (assoc command commands))
        (if msg
            (format "~a ~a" who (cdr msg))
            (format "~a command not found: \"~a\". use \"?commands\" to see possible commands"
                    who command)))
       (`("?help")
        (format "~a try \"?help <command>\" or \"?commands\"" who))
       ('("!clawee") "what a shit app")
       ('("?mona") "spenny11Mona spenny11Mona spenny11Mona spenny11Mona spenny11Mona")
       (_ #f))) ;; unrecognized command/not applicable
    (_ #f))) ;; other types of messages

(define (message-tag tag message)
  (assoc tag (irc-message-tags message)))

;; ;; "Want to become famous? Buy followers and viewers on bigfollows .com"
(define (greeting-message message)
  (match message
    ((irc-message tags _ "PRIVMSG" `(,where ,what)  _)
     (define who
       (cdr (assq 'display-name (irc-message-tags message))))
     (define first
       (cdr (assq 'first-msg (irc-message-tags message))))
     (and (equal? first "1")
          (cond ((string-contains? what "bigfollows")
                 "yes baby, give me fame and fortune")
                (else (format "how do you do, ~a?" who)))))
    (_ #f)))

;; respond to applicable messages
(define (respond-to-message message)
  (write message) (newline)
  (match message
    ;; respond to commands
    ((irc-message _ _ "PRIVMSG" `(,where ,what)  _)
     (define response
       ;; these are potential commands, so use the semaphore
       (call-with-semaphore irl-semaphore
                            (thunk (response-message message))))
     (define greeting
       (greeting-message message))
     (when response
       (irc-send-message (twitch-connection) where response))
     (when greeting
       (irc-send-message (twitch-connection) where greeting)))
    ;; give shoutouts
    ((irc-message tags _ "USERNOTICE" `(,where) _)
     (match (assoc 'msg-id tags)
       ('(msg-id . "raid")
        (define response (format "!so ~a" (cdr (assoc 'display-name tags))))
        (sleep 2) ;; so things don't seem too quick?
        (irc-send-message (twitch-connection) where response))
       (_ #f)))
    (_ (void))))

;; connect to twitch and grab connection in twitch-connection parameter
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
  (twitch-connection c)
  (irc-send-command c "CAP REQ" ":twitch.tv/commands")
  (irc-send-command c "CAP REQ" ":twitch.tv/tags")
  (irc-join-channel c (string-append "#" *username*))
  (irc-join-channel c "#spennythompson"))

;; main loop
(define (gogo)
  (let loop ()
    (define message
      (async-channel-get (irc-connection-incoming (twitch-connection)))) 
    (thread (thunk (respond-to-message message)))
    (loop)))

(define (run-it retry-interval)
  (with-handlers ((exn:fail:network:errno?
                   (lambda (err)
                     (match err
                       ((exn:fail:network:errno msg cont `(,errno . ,idk))
                        (write msg)
                        (newline)
                        (when (< retry-interval 10)
                          (sleep retry-interval)
                          (run-it (* retry-interval 2))))
                       (_ (write "idk\n"))))))
    
    (boot)
    (gogo)))

(define (main)
  (run-it 1))

;; (main)
;; #(struct:irc-message #f "tmi.twitch.tv" "RECONNECT\r" () ":tmi.twitch.tv RECONNECT\r")

; error reading from stream port
;   system error: Connection reset by peer; errno=104

; tcp-connect: host not found
;   hostname: irc.chat.twitch.tv
;   port number: 6697
;   system error: Name or service not known; gai_err=-2

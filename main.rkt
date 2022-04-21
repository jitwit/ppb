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

(define names.pieces
  (map swap pieces.names))

(define game-queue '())

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

;; list of pieces that haven't been assigned
(define (available-pieces)
  (filter (compose not lookup-piece) pieces))

;; sorted list of assignements
(define (marbles-lineup)
  (filter-map (lambda (x)
                (let ((pp (lookup-piece x)))
                  (and pp (cons x pp))))
              pieces))

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

(define (enqueue who)
  (cond ((member who game-queue)
         'already-there)
        (else
         (set! game-queue
               `(,@game-queue ,who))
         'ok)))

(define (dequeue who)
  (set! game-queue
        (filter (lambda (x)
                  (not (equal? x who)))
                game-queue)))

(define (popqueue)
  (match game-queue
    (`(,a . ,bc)
     (set! game-queue bc)
     a)
    (_ #f)))

(define (reset-queue)
  (set! game-queue '()))

;; randomly return one of the available pieces or #f if none are left
(define (random-piece)
  (define available (available-pieces))
  (define first-set (lset-intersection eq? available prioritized-pieces))
  (let* ((ps (if (null? first-set) available first-set))
         (n (length ps)))
    (and (> n 0)
         (list-ref ps (random n)))))

(define (reshuffle-pieces)
  (define-values (s1 s2)
    (partition (lambda (x)
                 (member (car x) prioritized-pieces))
               (marbles-lineup)))
  (define s3
    (lset-difference eq? prioritized-pieces (map car s1)))
  (cond ((or (null? s2) (null? s3)) 'nothing-to-do)
        (else
         (set! participants
               (make-hash (append s1
                                  (map cons s3 (map cdr (take s2 (length s3))))
                                  (drop s2 (length s3)))))
         'reshuffled-lineup)))

(define (test-reshuffle)
  (for-each (lambda (x)
              (add-participant x (random-piece)))
            '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"))
  (write (marbles-lineup)) (newline)
  (remove-participant "b")
  (write (reshuffle-pieces)) (newline)
  (remove-participant "m")
  (write (reshuffle-pieces)) (newline)
  (write (marbles-lineup)) (newline))

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
(define twitch-connection #f)

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
(define commands
  '(("?play" . "?play -- join marbles")
    ("?join" . "?join -- join the game queue")
    ("?next" . "?next -- advance the game queue")
    ("?leave" . "?leave -- quit marbles")
    ("?list" . "?list -- see the game queue")
    ("?clear" . "?clear -- reset the game queue")
    ("?quit" . "?quit -- quit the game queue")
    ("?kick" . "?kick <user> -- (mod only) boot <user> from the game")
    ("?who" . "?who <piece> -- owner of <piece>, where <piece> is either in fen or in full (e.g. n for black knight, R2 for white rook2)")
    ("?what" . "?what [person] -- what piece you have or optionally ask what [person]'s piece is")
    ("?reshuffle" . "?reshuffle -- (mod only) randomly assign pieces from set 1 to people with pieces from set 2 if possible.")
    ("?pieces" . "?pieces -- list of assigned pieces")
    ("?pieces-free" . "?pieces-free -- list of pieces not yet assigned")
    ("?lineup" . "?lineup -- pieces & people")
    ("?reset" . "?reset -- (mod only) reset irl marbles")
    ("?help" . "?help <command> -- information about <command>")
    ("?commands" . "?commands -- list of available commands. type ?help <command> to get information about <command>")
    ("?adopt" . "?adopt <elo-1> <elo-2> <n> -- the probability that <elo-1> can adopt <elo-2> in a match of length <n>")))

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
       ('("?next")
        (cond ((grant-permission? message)
               (let ((next (popqueue)))
                 (if next
                     (format "next up: ~a" next)
                     "game queue is empty")))
              (else
               "command only available to mods")))
       ('("?shill")
        "https://www.chess.com/membership?ref_id=14665398")
       ('("?join")
        (match (enqueue who)
          ('ok
           (format "~a joined the queue" who))
          ('already-there
           (format "~a you're already in the queue" who))))
       ('("?clear")
        (cond ((grant-permission? message)
               (reset-queue)
               "game queue was reset")
              (else
               "command only available to mods")))
       (`("?join!" ,who)
        (and (grant-permission?)
             (enqueue who)
             (format "~a joined the queue" who)))
       ('("?list")
        (format "play with streamer queue -- ~a" (string-join game-queue ", ")))
       ('("?quit")
        (dequeue who)
        (format "~a left the queue" who))
       (`("?adopt" ,elo1 ,elo2 ,match-length)
        (let* ((elo1 (string->number elo1))
               (elo2 (string->number elo2))
               (match-length (string->number match-length)))
          (and elo1 elo2 match-length
               (integer? match-length)
               ;; (<= 0 match-length 10000)
               (format "adoption probability: ~a"
                       (adoption elo1 elo2 match-length)))))
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
       ('("?reshuffle")
        (match (reshuffle-pieces)
          ('nothing-to-do "no reshuffling was necessary")
          ('reshuffled-lineup "lineup was reshuffled")))
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
                     (format "~a has the ~a"
                             pig
                             (piece->string piece))
                     (format "the ~a isn't taken"
                             (piece->string piece)))))))
       (`("?force" ,who)
        (when (grant-permission? message)
          (let* ((piece (random-piece))
                 (result (add-participant (remove-at who) piece)))
            (match result
              ('marbles-full
               (format "~a irl marbbies is full" who))
              (_ #f)))))
       ('("?pieces")
        (format "~a"
                (string-join (map piece->string (assigned-pieces)) ", ")))
       ('("?pieces-free")
        (format "remaining pieces: ~a"
                (string-join (map piece->string (available-pieces)) ", ")))
       ('("?lineup")
        (format "the current lineup -- ~a"
                (string-join (map (lambda (p.w)
                                    (format "~a : ~a"
                                            (cdr p.w)
                                            (piece->string (car p.w))))
                                  (marbles-lineup))
                             " | ")))
       ('("?reset")
        (cond ((grant-permission? message)
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
       ;; (`("!rohan")
       ;;  "i might have a rich dad but he makes me earn everything i have. dont get anything for free. everything has terms and conditions. i have worked and done internships. i do go to school, dont do drugs, get good grades, have student council roles")
       ('("l") "l")
       ('("!clawee") "what a shit app")
       ('("?clawee") "what a shit app")
       ('("?mona")
        (string-join (make-list (+ 5 (random 10)) "spenny11Mona")))
       ('("?gorey")
        (string-join (make-list (+ 5 (random 10)) "spenny11Gorey")))
       ('("?no-more")
        (string-join (make-list (+ 5 (random 10)) "spenny11NoMosquito")))
       ('("?pronouns") "it/it")
       ('("piss!play") "i adore piss!play")
       ;; ('("!followers")
       ;;  "Just followed to say I hate it when channels bribe followers by making you follow just to chat. For small channels it kills interactivity. It never works. It's for popular channels that need chat control, not channels with 10 viewers. Just followed to post this. Unfollowing and blocking. I hope I never get even a whiff of this channel again. Bye.") 
       (`("Hey" "@piss_pig_bot" . ,args)
        (format "i'm doing ok! hopefully things are going well for you, ~a?" who))
       ;;       (`("?user" ,arg) (format "/user ~a" arg))
       (_ #f))) ;; unrecognized command/not applicable
    (_ #f))) ;; other types of messages

(define (message-tag tag message)
  (assoc tag (irc-message-tags message)))

;; "Want to become famous? Buy followers and viewers on bigfollows .com"
;; Twitch Viewbot Program - do up to 1000 online on any stream! 49$, lifetime. Discord - Viewbot#0001

(define (greeting-message message)
  (match message
    ((irc-message tags _ "PRIVMSG" `(,where ,what)  _)
     (define who
       (cdr (assq 'display-name (irc-message-tags message))))
     (define first
       (cdr (assq 'first-msg (irc-message-tags message))))
     (and (equal? first "1")
          (cond ((and (string-contains? what "followers")
                      (string-contains? what "primes"))
                 (thread
                  (lambda ()
                    (sleep 10)
                    (irc-send-message twitch-connection where
                                      (format "/ban ~a" who))))
                 "yes baby, give me fame and fortune")
                ((string-contains? what "Twitch Viewbot Program")
                 (format "/ban ~a" who))
                (else (format "how do you do, ~a?" who)))))
    (_ #f)))

(define (adoption x y m)
  (with-output-to-string
    (lambda ()
      (system
       (format "jconsole adoption.ijs -js \"exit 0 [ echo ~a (~a adopt) ~a\""
               x m y)))))

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
       (irc-send-message twitch-connection where response))
     (when greeting
       (irc-send-message twitch-connection where greeting)))
    ;; give shoutouts
    ((irc-message tags _ "USERNOTICE" `(,where) _)
     (match (assoc 'msg-id tags)
       ('(msg-id . "raid")
        (define response (format "!so ~a"
                                 (cdr (assoc 'display-name tags))))
        (sleep 2) ;; so things don't seem too quick?
        (irc-send-message twitch-connection where response))
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
  (set! twitch-connection c)
  (irc-send-command c "CAP REQ" ":twitch.tv/commands")
  (irc-send-command c "CAP REQ" ":twitch.tv/tags")
  (irc-join-channel c (string-append "#" *username*))
  (irc-join-channel c "#spennythompson")
  (irc-join-channel c "#gorey_hole"))

;; main loop
(define (gogo)
  (let loop ()
    (define message
      (async-channel-get (irc-connection-incoming twitch-connection)))
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
                       (_ (write "idk\n")))))
                  ((const #t)
                   (lambda (err)
                     (write err)
                     (newline)
                     (when (< retry-interval 10)
                       (sleep retry-interval)
                       (run-it (* retry-interval 2))))))
    (boot)
    (gogo)))

(define (main)
  (run-it 1))

(main)




;; #(struct:irc-message #f "tmi.twitch.tv" "RECONNECT\r" () ":tmi.twitch.tv RECONNECT\r")

; error reading from stream port
;   system error: Connection reset by peer; errno=104

; tcp-connect: host not found
;   hostname: irc.chat.twitch.tv
;   port number: 6697
;   system error: Name or service not known; gai_err=-2

;; (first-msg . "1")
;; #(struct:irc-message ((badges . "glhf-pledge/1") (client-nonce . "0b0ed6c2867b10d3b4f77397497b51cc") (color . "#0000FF") (display-name . "deveshwarrocks1234") (first-msg . "1") (id . "272c36ab-092b-467d-9635-7709bb3b0f54") (mod . "0") (room-id . "500504795") (subscriber . "0") (tmi-sent-ts . "1636430956064") (turbo . "0") (user-id . "265456604")) "deveshwarrocks1234!deveshwarrocks1234@deveshwarrocks1234.tmi.twitch.tv" "PRIVMSG" ("#spennythompson" "!viewerbattles\r") "@badge-info=;badges=glhf-pledge/1;client-nonce=0b0ed6c2867b10d3b4f77397497b51cc;color=#0000FF;display-name=deveshwarrocks1234;emotes=;first-msg=1;flags=;id=272c36ab-092b-467d-9635-7709bb3b0f54;mod=0;room-id=500504795;subscriber=0;tmi-sent-ts=1636430956064;turbo=0;user-id=265456604;user-type= :deveshwarrocks1234!deveshwarrocks1234@deveshwarrocks1234.tmi.twitch.tv PRIVMSG #spennythompson :!viewerbattles\r")

;; #(struct:irc-message ((badge-info . "founder/7") (badges . "founder/0,sub-gifter/25") (color . "#008000") (display-name . "latinum_blonde") (id . "96a7794e-9202-4255-9502-2aa896931992") (login . "latinum_blonde") (mod . "0") (msg-id . "bitsbadgetier") (msg-param-threshold . "10000") (room-id . "500504795") (subscriber . "1") (system-msg . "bits\\sbadge\\stier\\snotification") (tmi-sent-ts . "1636521654972") (user-id . "570223165")) "tmi.twitch.tv" "USERNOTICE" ("#spennythompson\r") "@badge-info=founder/7;badges=founder/0,sub-gifter/25;color=#008000;display-name=latinum_blonde;emotes=;flags=;id=96a7794e-9202-4255-9502-2aa896931992;login=latinum_blonde;mod=0;msg-id=bitsbadgetier;msg-param-threshold=10000;room-id=500504795;subscriber=1;system-msg=bits\\sbadge\\stier\\snotification;tmi-sent-ts=1636521654972;user-id=570223165;user-type= :tmi.twitch.tv USERNOTICE #spennythompson\r")


;; #(struct:irc-message #f "tmi.twitch.tv" "RECONNECT\r" () ":tmi.twitch.tv RECONNECT\r")

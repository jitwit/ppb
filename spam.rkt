#lang racket

(require racket/async-channel
         (except-in srfi/1 delete)
         net/http-easy
         "irc.rkt"
         "outils.rkt")

;; configuration to log in to twitch
(define *oauth-token*
  (file->string "ringo-token.txt"))

(define *username*
  "jingo_ringo")

;; network connection to twitch irc network
(define twitch-connection #f)

(define location #f)

;; semaphore to protect state among threads
(define irl-semaphore
  (make-semaphore 1))

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
  (irc-join-channel c (string-append "#" *username*)))

(define (leave-channel channel)
  (irc-part-channel twitch-connection (string-append "#" channel)))

(define (join-channel channel)
  (when (string? location)
    (leave-channel location))
  (set! location channel)
  (irc-join-channel twitch-connection
                    (string-append "#" channel)))

(define (send-message message)
  (irc-send-message twitch-connection (string-append "#" location) message))

(define peepin      "domina105RINGOPEEPINFROMBLIND")
(define bunny       "mcknzRingoBunny")
(define cat-jam     "badche2CatJAM")
(define side-eye    "badche2RingoSideEye")
(define ringo-bus   "domina105RINGOBUS")
(define ringo-cute  "domina105RINGOCUTE")
(define ringo-rip   "domina105RESTINPEACERINGO")
(define golden-boi  "domina105RINGOGOLDENBOI")
(define rolly-boi   "tinxRINGOROLLYBOI")
(define uwu         "domina105RINGOUWUWUWUUWUWUW")
(define irl-ezwin   "badche2RINGOLIFEIMITATESART")
(define ezwin       "domina105RINGOEZWINGETREKT")
(define cooked      "domina105COOKED")
(define ahoy        "domina105CRABAHOY")
(define crab-sad    "domina105SAD")
(define menacing    "domina105MENACING")
(define luci-dance  "tinxDANCE")
(define ringo-luci  "tinxRINGOLUCI")
(define feels-bad   "tinxFeelsBadMan")
(define ringo-bunny "mcknzRingoBunny")
(define rare-boi    "domina105RARELIMITEDRINGBUNNY")
(define golden-card "domina105GOLDENCARD")
(define tot-pls     "domina105TOTPLS")
(define work-call   "domina105WORKCALL")
(define angi        "tinxANGI")

(define (quick-fire message n)
  (for ((i (iota n)))
    (send-message message)))

(define (triangle-fire message n)
  (define dh 1)
  (define h 1)
  (for ((i (iota n)))
    (send-message (string-join
                   (map (lambda (x) message)
                        (iota h))))
    (cond ((= h 1) (set! dh 1))
          ((= h 4) (set! dh -1)))
    (set! h (+ h dh))))

(define (fire message n dt)
  (for ((i (iota n)))
    (send-message message)
    (sleep dt)))

(define (repeated s)
  (define n (quotient 500 (+ 1 (string-length s))))
  (string-join (make-list n s)))

(define (squared boundary . centers)
  (define boundary-message (string-join (make-list 3 boundary)))
  (for-each (lambda (center-piece)
              (send-message boundary-message)
              (send-message (string-join (list boundary center-piece boundary))))
            centers)
  (send-message boundary-message))

(define (L-inf x y)
  (apply max (map abs (map - x y))))

(define (supersquare . layers)
  (define N (- (* 2 (length layers)) 1))
  (define C (make-list 2 (- (length layers) 1)))
  (when (<= 0 (car C) 5)
    (for ((x (range N)))
      (let ((msg ""))
        (for ((y (range N)))
          (set! msg (string-append msg
                                   " "
                                   (list-ref layers
                                             (L-inf (list x y) C)))))
        (send-message msg)))))

(define (attack!)
  (let ((msg (repeated ezwin)))
    (fire msg 100 0.2)))

(define (go)
  (boot)
  (join-channel "DOMINANTCRAB")
;; (join-channel "spennythompson")
;;  (join-channel "TinaPalooza")
  )

;; (go)



;; Unicode Character “⠀” (U+2800) --- how diesiraeswe gets whitespace
;; braille empty or something

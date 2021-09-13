#lang racket/base

(provide irc-get-connection
         irc-connection-incoming
         irc-send-command
         irc-send-message
         irc-send-notice
         irc-join-channel
         irc-part-channel
         irc-connect
         irc-set-nick
         irc-set-user-info
         irc-quit
         irc-connection?
         (struct-out irc-message))

;; ---------------------------------------------------------------------------------------------------

(require racket/async-channel
         racket/list
         racket/match
         racket/string
         racket/tcp
         openssl)

(module+ test
  (require rackunit))

(struct irc-connection (in-port out-port in-channel handlers))
(struct irc-message (tags prefix command parameters content) #:transparent)

(define irc-connection-incoming irc-connection-in-channel)

(define (irc-get-connection host port
                            #:return-eof [return-eof #f]
                            #:ssl [ssl #f])
  (define-values (in out) (match ssl
                            [#f (tcp-connect host port)]
                            [#t (ssl-connect host port)]
                            [_ (ssl-connect host port ssl)]))
  (file-stream-buffer-mode out 'line)
  (define in-channel (make-async-channel))
  (define handlers (make-hash))
  (define connection (irc-connection in out in-channel handlers))
  (add-handler connection send-to-user)
  (add-handler connection handle-ping)

  (thread (lambda ()
            (let loop ()
              (sync in)
              (define line (if (port-closed? in) eof (read-line in)))
              (cond
               [(eof-object? line)
                (when return-eof
                  (async-channel-put in-channel line))]
               [else
                (define message (parse-message line))
                (when message
                  ;; convert to list here so that we can remove hash table elements during the loop
                  (for ([kv (hash->list (irc-connection-handlers connection))])
                    ((cdr kv) message connection (car kv))))
                (loop)]))))
  connection)

(define (irc-send-command connection command . parameters)
  (fprintf (irc-connection-out-port connection)
           "~a ~a\r\n"
           command
           (string-join parameters)))

(define (add-handler connection callback)
  (hash-set! (irc-connection-handlers connection) (gensym) callback))

(define (remove-handler connection handler-id)
  (hash-remove! (irc-connection-handlers connection) handler-id))

(define (send-to-user message connection handler-key)
  (async-channel-put (irc-connection-in-channel connection) message))

(define (handle-ping message connection handler-key)
  (match message
    [(irc-message _ _ "PING" params _)
     (irc-send-command connection "PONG" (string-append ":" (first params)))]
    [_ (void)]))

(define (irc-set-nick connection nick)
  (irc-send-command connection "NICK" nick))

(define (irc-set-user-info connection username real-name)
  (irc-send-command connection
                    "USER"
                    username
                    "0"
                    "*"
                    (string-append ":" real-name)))


;; Connects to an IRC server, returning the connection and an event that will be ready for
;; synchronization when the server is ready for more commands
(define (irc-connect server port nick username real-name
                     #:return-eof [return-eof #f]
                     #:ssl [ssl #f]
                     #:password [password #f])
  (define connection (irc-get-connection server port #:return-eof return-eof #:ssl ssl))
  (define sema (make-semaphore))
  (add-handler connection (listen-for-connect sema))
  (when password
    (irc-send-command connection "PASS" password))
  (irc-set-nick connection nick)
  (irc-set-user-info connection username real-name)
  (values connection sema))

;; defined inside private file for some reason?
(define RPL_WELCOME 1)

(define ((listen-for-connect sema) message connection handler-id)
  (match message
    [(irc-message _ _ RPL_WELCOME _ _)
     (semaphore-post sema)
     (remove-handler connection handler-id)]
    [_ (void)]))

(define (irc-join-channel connection channel)
  (irc-send-command connection "JOIN" channel))

(define (irc-part-channel connection channel)
  (irc-send-command connection "PART" channel))

(define (irc-send-message connection target message)
  (irc-send-command connection
		    "PRIVMSG"
		    target
		    (string-append ":" message)))

(define (irc-send-notice connection target message)
  (irc-send-command connection
                    "NOTICE"
                    target
                    (string-append ":" message)))

(define (irc-quit connection [quit-message ""])
  (if (string=? quit-message "")
      (irc-send-command connection "QUIT")
      (irc-send-command connection "QUIT" quit-message))
  (close-output-port (irc-connection-out-port connection))
  (close-input-port (irc-connection-in-port connection)))

;; v3 messages:
;; <message>       ::= ['@' <tags> <SPACE>] [':' <prefix> <SPACE> ] <command> <params> <crlf>
;; <tags>          ::= <tag> [';' <tag>]*
;; <tag>           ::= <key> ['=' <escaped_value>]
;; <key>           ::= [ <client_prefix> ] [ <vendor> '/' ] <key_name>
;; <client_prefix> ::= '+'
;; <key_name>      ::= <non-empty sequence of ascii letters, digits, hyphens ('-')>
;; <escaped_value> ::= <sequence of zero or more utf8 characters except NUL, CR, LF, semicolon (`;`) and SPACE>
;; <vendor>        ::= <host>

;; original messages:
;; <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
;; <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
;; <command>  ::= <letter> { <letter> } | <number> <number> <number>
;; <SPACE>    ::= ' ' { ' ' }
;; <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]

;; <middle>   ::= <Any *non-empty* sequence of octets not including SPACE
;;                or NUL or CR or LF, the first of which may not be ':'>
;; <trailing> ::= <Any, possibly *empty*, sequence of octets not including
;;                  NUL or CR or LF>

;; <crlf>     ::= CR LF

;; esacping quirks: unless one of the following (all others) then \ does nothing... and is dropped... ok...
;; ; (semicolon)	\: (backslash and colon)
;; SPACE	\s
;; \	\\
;; CR	\r
;; LF	\n
;; all others	the character itself

;; Given the string of an IRC message, returns an irc-message that has been parsed as far as possible,
;; or #f if the input was unparsable
(define (parse-message message)
  (define chunks (string-split message " " #:trim? #f))
  (let-values (((tags chunks) (parse-tags chunks)))
    (let-values (((pref chunks) (parse-prefix chunks)))
      (match chunks
        (`(,cmd ,params ...)
         (irc-message tags pref cmd (parse-params params) message))
        (_ #f)))))

(define (message-prefix-section? chunk)
  (string-prefix? chunk ":"))
(define (parse-prefix chunks)
  (match chunks
    (`(,(? message-prefix-section? pref) ,chunks ...)
     (values (substring pref 1) chunks))
    (_ (values #f chunks))))

(define (message-tag-section? chunk)
  (string-prefix? chunk "@"))
(define (parse-tags-chunk chunk)
  (filter-map (lambda (k/v)
                (match (string-split k/v "=")
                  (`(,k ,v) (cons (string->symbol k) v))
                  (_ #f)))
              (string-split (substring chunk 1) ";")))
(define (parse-tags chunks)
  (match chunks
    (`(,(? message-tag-section? tags) ,chunks ...)
     (values (parse-tags-chunk tags) chunks))
    (_ (values #f chunks))))

(define (clean-trailing trail)
  (if (string-prefix? trail ":") (substring trail 1) trail))
(define (parse-params chunks)
  (define ix-trail
    (min 14 (or (index-where chunks message-prefix-section?)
                (length chunks))))
  (define trail
    (clean-trailing (string-join (drop chunks ix-trail))))
  (if (non-empty-string? trail)
      `(,@(take chunks ix-trail) ,trail)
      chunks))

;; Run these via ``raco test main.rkt''
(module+ test
  (define (message-equal? m1 m2)
    (and (equal? (irc-message-prefix m1) (irc-message-prefix m2))
         (equal? (irc-message-command m1) (irc-message-command m2))
         (equal? (irc-message-parameters m1) (irc-message-parameters m2))))

  (define-check (check-parse input expected-prefix expected-command expected-args)
    (let ([actual (parse-message input)]
          [expected (irc-message #f expected-prefix
                                 expected-command
                                 expected-args
                                 input)])
      (with-check-info*
       (list (make-check-actual actual)
             (make-check-expected expected))
       (lambda ()
         (when (not
                (message-equal?
                 actual
                 expected))
           (fail-check))))))

  (check-parse ":my-prefix my-command arg1 arg2 arg3"
               "my-prefix"
               "my-command"
               (list "arg1" "arg2" "arg3"))
  (check-parse ":my-prefix my-command arg1 arg2 arg3 :4  5 6 7 8 9 0 1 2 3 4 5 6"
               "my-prefix"
               "my-command"
               (list "arg1" "arg2" "arg3" "4  5 6 7 8 9 0 1 2 3 4 5 6"))
  (check-parse ":my-prefix my-command arg1 arg2 arg3 4 5 6 7 8 9 0 1 2 3 4 5"
               "my-prefix"
               "my-command"
               (list "arg1" "arg2" "arg3" "4" "5" "6" "7" "8" "9" "0" "1" "2" "3" "4" "5"))
  (check-parse "my-command arg1 arg2 arg3 4 5 6 7 8 9 0 1 2 3 4 5 6"
               #f
               "my-command"
               (list "arg1" "arg2" "arg3" "4" "5" "6" "7" "8" "9" "0" "1" "2" "3" "4" "5 6"))

  (check-parse "my-command arg1 arg2 arg3 4 :5 6 7 8 9 0 1 2 3 4 5 6"
               #f
               "my-command"
               (list "arg1" "arg2" "arg3" "4" "5 6 7 8 9 0 1 2 3 4 5 6")))

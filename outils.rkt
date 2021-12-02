#lang racket

(provide lookup
         swap
         remove-at)

;; recurse into hashtable, returning empty table if key not present
(define (lookup table . keys)
  (if (or (null? keys) (hash-empty? table))
      table
      (apply lookup
             (hash-ref table (car keys) (make-hash))
             (cdr keys))))

(define (swap a.b)
  (cons (cdr a.b) (car a.b)))

(define (remove-at name)
  (if (and (non-empty-string? name) (eqv? #\@ (string-ref name 0)))
      (substring name 1)
      name))

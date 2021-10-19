#lang racket

(provide lookup
         swap)

;; recurse into hashtable, returning empty table if key not present
(define (lookup table . keys)
  (if (or (null? keys) (hash-empty? table))
      table
      (apply lookup
             (hash-ref table (car keys) (make-hash))
             (cdr keys))))

(define (swap a.b)
  (cons (cdr a.b) (car a.b)))

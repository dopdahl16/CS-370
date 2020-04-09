;Exam 1, Question 4
;Author: Daniel Opdahl
;Date: 3-29-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;Returns a unary predicate that takes an arbitrarily-complex list of symbols and returns #t if target occurs anywhere inside that list and returns #f otherwise.
;target must be a symbol, and los must be an arbitrarily-complex list of symbols
;O(length of los)

(define searcher
  (lambda (target)
    (lambda (los)
      (cond
        ((null? los) #f)
        ((eq? (car los) target) #t)
        ((list? (car los)) (or ((searcher target) (car los)) ((searcher target) (cdr los))))
        (else ((searcher target) (cdr los)))))))
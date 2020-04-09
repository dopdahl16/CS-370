;Exam 1, Question 3
;Author: Daniel Opdahl
;Date: 3-29-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;Returns a list of the position numbers with respect to a zero-origin, left-to-right numbering of all the symbols in los+ at which the target occurs.
;los+ must be an arbitrarily-complex list of symbols, and target must be a symbol
;O(fringe-positions-aux target (squish los+) 0 '()) -> O(length of los+ ^2)

(define fringe-positions
  (lambda (target los+)
    (fringe-positions-aux target (squish los+) 0 '())))



;Helper function for fringe-positions. Does the leg work for the functionality of fringe-positions. Adds the parameters index and return-lis.
;los+ must be an arbitrarily-complex list of symbols, target must be a symbol, index must be a number, and return-lis must be a list
;O(length of los+)

(define fringe-positions-aux
  (lambda (target los+ index return-lis)
    (cond
      ((null? los+) return-lis)
      ((eq? target (car los+)) (fringe-positions-aux target (cdr los+) (+ 1 index) (append return-lis (list index))))
      (else (fringe-positions-aux target (cdr los+) (+ 1 index) return-lis)))))



;Helper function for fringe-positions. Removes nesting from los+, returning a list of depth 1.
;los+ must be an arbitrarily-complex list of symbols
;O(length of los+)

(define squish
  (lambda (los+)
    (cond
      ((null? los+) '())
      ((list? (car los+)) (append (squish (car los+)) (squish (cdr los+))))
      (else (cons (car los+) (squish (cdr los+)))))))
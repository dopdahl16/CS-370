;Exam 1
;Author: Daniel Opdahl
;Date: 3-29-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;Returns a list containing the same elements as lis, but nested deeper and deeper as you go from right to left.
;lis must be a list
;O(length of lis)

(define deepen-right
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((eq? (length lis) 1) (cons (car lis) '()))
      (else (cons (car lis) (cons (deepen-right (cdr lis)) '()))))))



;Removes any and all null lists from lis+, ultimately leaving the result with no null lists whatsoever, noting that the removal of more-nested nulls can create less-nested nulls.
;lis+ must be an arbitrarily-complex list
;O(length of lis+)

(define without-nulls
  (lambda (lis+)
    (cond
      ((null? lis+) '())
      ((atom? (car lis+)) (cons (car lis+) (without-nulls (cdr lis+))))
      ((empty? (car lis+)) (without-nulls (cdr lis+)))
      (else (cons (without-nulls (car lis+)) (without-nulls (cdr lis+)))))))



;Helper function for without-nulls. Unary predicate that determines if lis+ is empty (if it contains no atoms) or not, returning true if it contains no atoms and false if it does.
;lis+ must be an arbitrarily-complex list
;O(length of lis+ * depth of lis+)

(define empty?
  (lambda (lis+)
    (cond
      ((null? lis+) #t)
      ((atom? (car lis+)) #f)
      (else (and (empty? (car lis+)) (empty? (cdr lis+)))))))



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



;Returns the convolution of the elements of lisa with the elements of lisb; that is, if lisa is (x1 x2 ... xn) and lisb is (y1 y2 ... yn), convolution returns the list ((x1 yn) (x2 yn-1) ... (xn y1))
;lisa and lisb must be lists of symbols that are the same length, and stage-length must be the list '(0) in order for this procedure to work as intended
;O(length of lisa + length of lisb)

(define convolution
  (lambda (lisa lisb stage-list)
    (cond
      ((eq? (car stage-list) 0)
       (cond
         ((null? lisb) '())
         ((null? lisa) '())
         (else (convolution lisa (append (convolution lisa (cdr lisb) '(1)) (cons (cons (car lisb) '()) '())) '(2)))))
      ((eq? (car stage-list) 1)
       (cond
         ((null? lisb) '())
         ((null? lisa) '())
         (else (append (convolution lisa (cdr lisb) '(1)) (cons (cons (car lisb) '()) '())))))
      ((eq? (car stage-list) 2)
       (cond
         ((null? lisb) '())
         (else (cons (cons (car lisa) (car lisb)) (convolution (cdr lisa) (cdr lisb) '(2)))))))))
;Assignment 3
;Author: Daniel Opdahl
;Date: 3-4-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;Returns a unary procedure that when applied to a value
;element1 returns a unary procedure that when applied to a
;value element2 returns the value of binaryProc applied to
;element1 and element2.
;binaryProc must be a binary procedure
;O(binaryProc)

(define curried-binary
  (lambda (binaryProc)
    (lambda (element1)
      (lambda (element2)
        (binaryProc element1 element2)))))



;Returns a binary procedure that when applied to a value x and
;a value y returns the value of the original uncurried version
;of curriedBinaryProc applied to x and y.
;curriedBinaryProc must be a bianry procedure that has been
;curried
;O(curriedBinaryProc)

(define uncurried-binary
  (lambda (curriedBinaryProc)
      (lambda (element1 element2)
        ((curriedBinaryProc element1) element2))))
    


;Returns the list in which all runs of two or more adjacent
;elements of lis for which related? returns a non-#f value
;have been grouped into lists.
;lis must be a list and related? must be a binary predicate
;O(length of lis)

(define adjacent-related-grouped
  (lambda (related? lis)
    (cond
      ((null? lis) '())
      ((null? (cdr lis)) (cons lis '()))
      ((related? (car lis) (car (cdr lis)))
       (cons
        (cons (car lis)
              (car (adjacent-related-grouped related?
                                             (cdr lis))))
        (cdr (adjacent-related-grouped related? (cdr lis)))))
      (else (cons (cons (car lis) '())
                  (adjacent-related-grouped related? (cdr lis)))))))



;Returns a unary procedure p that “reduces” a list using
;binaryProc, treating binaryProc as if it’s right-associative.
;binaryProc must be a binary procedure, unaryProc must be a
;unary procedure, and zeroaryProc must be a procedure with
;zero parameters, lat must be a list.
;O(length of lat)

(define rreducer
  (lambda (binaryProc unaryProc zeroaryProc)
    (lambda (lat)
      (cond
        ((eq? (length lat) 0) (zeroaryProc))
        ((eq? (length lat) 1) (unaryProc (car lat)))
        (else (binaryProc (car lat)
              ((rreducer binaryProc unaryProc zeroaryProc)
               (cdr lat))))))))



;Returns a unary procedure p that “reduces” a list using
;binaryProc, treating binaryProc as if it’s left-associative.
;binaryProc must be a binary procedure, unaryProc must be a
;unary procedure, and zeroaryProc must be a procedure with
;zero parameters, lat must be a list.
;O(length of lat)

(define lreducer
  (lambda (binaryProc unaryProc zeroaryProc)
    (lambda (lat)
      (cond
        ((eq? (length lat) 0) (zeroaryProc))
        ((eq? (length lat) 1) (unaryProc (car lat)))
        (else ((lreducer binaryProc unaryProc zeroaryProc)
               (cons (binaryProc (car lat) (car (cdr lat)))
                     (cdr (cdr lat)))))))))



;Succeeds with two values: a list like the arbitrarily-complex
;list of symbols los+, but with every other occurrence (as
;read from left to right, starting with the leftmost
;occurrence) of the symbol old replaced by an occurrence of
;the symbol new and a Boolean value that’s #f if the last
;occurrence of old wasn’t replaced by new but #t if the last
;occurrence of old was replaced by new.
;Success procedures must be binary procedures, los+ must be a
;list of symbols 
;O(subst-every-other-helper)

(define subst-every-other-sf
  (lambda (old new los+ succeed)
    (subst-every-other-helper old new los+ 1 succeed)))



;Helper function for subst-every-other-sf. If succeed calls
;for returning the the result, returns a list like the
;arbitrarily-complex list of symbols los+, but with every other
;occurrence (as read from left to right, starting with the
;leftmost occurrence) of the symbol old replaced by an
;occurrence of the symbol new, irrespective of the (sub)list
;containing old, and irrespective of nesting or depth of the
;list. If bit is 1, then the next instance of old that is
;encountered will be replaced with new. If bit is 0, then the
;next value of old that is encounted will not be replaced with
;new. If succeed calls for returning the replaced, returns a
;Boolean value that’s #f if the last occurrence of old wasn’t
;replaced by new but #t if the last occurrence of old was
;replaced by new.
;los+ is an arbitrarily-complicated list of symbols, new can be
;any valid symbol, old can be any symbol (but in the case that
;old is not a symbol found in los+, the los+ passed in to
;subst-every-other will be the same los+ returned, i.e., the
;procedure will not do anything to the los+. Additionally, bit
;must be either a 0 or a 1 at all times, and if the procedure
;is to replace every other instance of old with new starting
;with the first occurance of old, the value of bit with which
;subst-every-other is initially called must be 1.
;O(length of los+)

(define subst-every-other-helper
  (lambda (old new los+ bit succeed)
    (cond
      ((null? los+) (succeed '() #f))
      ((not (atom? (car los+)))
       (succeed (cons
                 (subst-every-other-helper old new
                                           (car los+)
                                            bit succeed)
             (subst-every-other-helper old new
                                       (cdr los+)
                                        bit succeed))
                (odd? (count old 0 los+))))
      ((and (eq? (car los+) old) (not (zero? bit)))
       (succeed (cons new
             (subst-every-other-helper old new
                                       (cdr los+) 0 succeed))
                (odd? (count old 0 los+))))
      ((and (eq? (car los+) old) (zero? bit))
       (succeed (cons old
             (subst-every-other-helper old new
                                       (cdr los+) 1 succeed))
                (odd? (count old 0 los+))))
      (else
       (succeed (cons (car los+)
             (subst-every-other-helper old new
                                       (cdr los+) bit succeed))
                (odd? (count old 0 los+)))))))



;Support function for subst-every-other-helper. Returns the
;number of occurances of old in los+ starting counting at the
;value of tot.
;old must be a symbol, tot must be an int, los+ must be a list
;of symbols
;O(length of los+)

(define count
  (lambda (old tot los+)
    (cond
      ((null? los+) '())
      ((and (eq? (cdr los+) '()) (eq? (car los+) old)) (+ tot 1))
      ((and (eq? (cdr los+) '()) (not (eq? (car los+) old))) tot)
      ((eq? (car los+) old) (count old (+ tot 1) (cdr los+)))
      (else (count old tot (cdr los+))))))
;Assignment 2
;Author: Daniel Opdahl
;Date: 2-25-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;Returns #t iff x and y have the same structure at all levels and returns #f otherwise. x and y have the same structure iff wherever x contains an atom, y also contains an atom (any old atom: it need not be the same atom that appeared in x), and wherever x contains a list, y also contains a list.
;x and y can be any arbitrary value that is defined in Scheme
;If x and y are not the same structure, they will not be the same length, so same-structure? will be O(length of x or y, whichever is shorter). If they are the same structure, they will be the same length, so same-structure? will be O(length of x or y, does not matter since they are the same length) 

(define same-structure?
  (lambda (x y)
    (cond
      ((and (atom? x) (not(atom? y))) false)
      ((and (not(atom? x)) (atom? y)) false)
      ((and (null? x) (not(null? y))) false)
      ((and (not(null? x)) (null? y)) false)
      ((and (null? x) (null? y)) true)
      ((and (atom? x) (atom? y)) true)
      (else (and (same-structure? (car x) (car y)) (same-structure? (cdr x) (cdr y)))))))



;Returns a list like lis+, but where any/all sublists of lis+ contain only the first n elements from lis+. If lis+ (or a sublist) contains more than n elements, a list containing just the first n elements from lis+ (or that sublist) followed by the symbol ... is returned
;lis+ is an arbitrarily-complicated list, and n is a non-negative integer
;O(length of lis+)

(define elide-length
  (lambda (lis+ n)
    (elide-length-helper lis+ n n)))



;Helper functino for elide-length. Returns a list like lis+, but where any/all sublists of lis+ contain only the first n elements from lis+. If lis+ (or a sublist) contains more than n elements, a list containing just the first n elements from lis+ (or that sublist) followed by the symbol ... is returned. The extra parameter len is added as a way to keep track of the original value of n that was passed in to elide-length, so that the correct number of elements in the list is kept.
;lis+ is an arbitrarily-complicated list, n is a non-negative integer, and len is a non-negative integer
;O(length of lis+)

(define elide-length-helper
  (lambda (lis+ n len)
    (cond
      ((null? lis+) '())
      ((zero? n) '(...))
      ((list? (car lis+)) (cons (elide-length-helper (car lis+) len len) (elide-length-helper (cdr lis+) (- n 1) len)))
      (else (cons (car lis+) (elide-length-helper (cdr lis+) (- n 1) len))))))



;Returns a list in which one can’t see inside any lists occurring at or below depth n, those elements having been replaced by the symbol &. If depth 0 signifies that the passed in list cannot be looked into at all, then all elements inside n+1 parentheses are replaced by &.
;lis+ is an arbitrarily-complicated list, and n is a non-negative integer
;O(length of lis+)

(define elide-depth
  (lambda (lis+ n)
    (cond
      ((zero? n) '&)
      ((null? lis+) '())
      ((atom? (car lis+)) (cons (car lis+) (elide-depth (cdr lis+) n)))
      (else (cons (elide-depth (car lis+) (- n 1)) (elide-depth (cdr lis+) n))))))



;Returns the list of all possible permutations of the atoms in loa. This procedure is not complete, but is functional.
;loa is a list of atoms
;O(length of loa)

(define perms
  (lambda (loa)
    (cond
      ((null? loa) '())
      ((null? (car loa)) (car loa))
      ((null? (cdr (cdr loa))) (cons (cons (car loa) (cdr loa)) (cons (cons (car (cdr loa)) (cons (car loa) '())) '())))
      (else (list (perms (cons (car loa) (cons (car (cdr loa)) '()))) (perms (cdr loa)))))))



;Returns a list like the arbitrarily-complex list of symbols los+, but with every other occurrence (as read from left to right, starting with the leftmost occurrence) of the symbol old replaced by an occurrence of the symbol new, irrespective of the (sub)list containing old, and irrespective of nesting or depth of the list.
;los+ is an arbitrarily-complicated list of symbols, new can be any valid symbol, old can be any symbol (but in the case that old is not a symbol found in los+, the los+ passed in to subst-every-other will be the same los+ returned, i.e., the procedure will not do anything to the los+.  
;O(length of los+)

(define subst-every-other
  (lambda (old new los+)
    (subst-every-other-helper old new los+ 1)))



;Helper function for subst-every-other. Returns a list like the arbitrarily-complex list of symbols los+, but with every other occurrence (as read from left to right, starting with the leftmost occurrence) of the symbol old replaced by an occurrence of the symbol new, irrespective of the (sub)list containing old, and irrespective of nesting or depth of the list. If bit is 1, then the next instance of old that is encountered will be replaced with new. If bit is 0, then the next value of old that is encounted will not be replaced with new.
;los+ is an arbitrarily-complicated list of symbols, new can be any valid symbol, old can be any symbol (but in the case that old is not a symbol found in los+, the los+ passed in to subst-every-other will be the same los+ returned, i.e., the procedure will not do anything to the los+. Additionally, bit must be either a 0 or a 1 at all times, and if the procedure is to replace every other instance of old with new starting with the first occurance of old, the value of bit with which subst-every-other is initially called must be 1.
;O(length of los+)

(define subst-every-other-helper
  (lambda (old new los+ bit)
    (cond
      ((null? los+) '())
      ((not (atom? (car los+))) (cons (subst-every-other-helper old new (car los+) bit) (subst-every-other-helper old new (cdr los+) bit)))
      ((and (eq? (car los+) old) (not (zero? bit))) (cons new (subst-every-other-helper old new (cdr los+) 0)))
      ((and (eq? (car los+) old) (zero? bit)) (cons old (subst-every-other-helper old new (cdr los+) 1)))
      (else (cons (car los+) (subst-every-other-helper old new (cdr los+) bit))))))
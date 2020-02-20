;Assignment 1
;Author: Daniel Opdahl
;Date: 2-20-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;Assume that lisa and lisb are lists of the same length or that lisa is exactly one element longer than lisb (and the procedure deals with either situation). Returns the list that results from “zipping” together lisa and lisb.
;lisa and lisb must be lists of the same length or lisa must be no more than one element longer
;O(length of lisa + length of lisb)

(define zip
  (lambda (lisa lisb)
    (cond
      ((null? lisa) '())
      ((null? lisb) lisa)
      ((null? (cdr lisa)) (cons (car lisa) lisb))
      (else (cons (car lisa) (zip lisb (cdr lisa)))))))



;Assume that lis is a list. Returns the pair of lists that results from “unzipping” lis (placing the last element of lis into the first element of the result, if lis has an odd number of elements).
;lis must be a list
;O(length of lis)

(define unzip
  (lambda (lis)
    (cond
      ((null? lis) '(()()))
      (else (cons (every-other lis) (cons (every-other (cdr lis)) '()))))))



;Auxiliary procedure for unzip procedure. Takes a list and returns a list of every other element in that list
;lis must be a list
;O(length of lis)

(define every-other
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((null? (cdr lis)) (cons (car lis) '()))
      ((null? (cdr (cdr lis))) (cons (car lis) '()))
      (else (cons (car lis) (every-other (cdr (cdr lis))))))))


;Assume that lis is a list. Returns the list of all tails of lis, in order from longest to shortest.
;lis must be a list
;O(length of lis)

(define all-tails
  (lambda (lis)
    (cond
      ((null? lis) '(()))
      (else (cons lis (all-tails (cdr lis)))))))



;Assume that lis is a list. Returns the list of all heads of lis, in order from shortest to longest.
;lis must be a list
;O(length of los ^2)                    MAKE SURE YOU ASK ABOUT THIS - I AM NOT SURE ABOUT THIS COMPLEXITY

(define all-heads
  (lambda (lis)
    (cond
      ((null? lis) '(()))
      (else (snoc lis (all-heads (rdc lis)))))))



;Assume los is a list of symbols. Returns the list in which all runs of two or more adjacent duplicate symbols have been eliminated and replaced by a single occurrence of that symbol.
;los must be a list of symbols
;O(length of los)

(define without-adjacent-duplicates
  (lambda (los)
    (cond
      ((null? los) '())
      ((null? (cdr los)) los)
      ((eq? (car los) (car (cdr los))) (without-adjacent-duplicates (cdr los))) 
      (else (cons (car los) (without-adjacent-duplicates (cdr los)))))))



;Assume los is a list of symbols. Returns the list in which all runs of adjacent equal symbols have been grouped into lists.
;los must be a list of symbols
;O(length of los ^2)                    MAKE SURE YOU ASK ABOUT THIS - I AM NOT SURE ABOUT THIS COMPLEXITY

(define adjacent-equals-grouped
  (lambda (los)
    (cond
      ((null? los) '())
      ((null? (cdr los)) (cons los '()))
      ((eq? (car los) (car (cdr los))) (cons (cons (car los) (car (adjacent-equals-grouped (cdr los)))) (cdr (adjacent-equals-grouped (cdr los)))))
      (else (cons (cons (car los) '()) (adjacent-equals-grouped (cdr los)))))))
      


;;;;;;;;;;Example procedures defined in class;;;;;;;;;

(define rac
  (lambda (lis)
    (if
     (null? (cdr lis))
     (car lis)
     (rac (cdr lis)))))


(define rdc
  (lambda (lis)
    (if
     (null? (cdr lis))
     '()
     (cons (car lis) (rdc (cdr lis))))))


(define snoc
  (lambda (x lis)
    (if
     (null? lis)
     (cons x '())
     (cons (car lis) (snoc x (cdr lis))))))
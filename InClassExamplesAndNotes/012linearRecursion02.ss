#lang scheme

(require (lib "tls.ss" "Luther"))


;;;;;;;;
;;;;;;;; Examples of Simple (Linear) Recursion
;;;;;;;;


;;;;
;;;; Palindrome Predicates
;;;;


;;
;; This version takes time O(1 + (length of los) ^ 2)
;;


(define palindrome?
  (lambda (los)
    (cond
      ((null? los) #t)
      ((null? (cdr los)) #t)
      ((not (eq? (car los) (rac los))) #f)
      (else (palindrome? (cdr (rdc los)))))))


;;
;; An alternate version combining two cond-clauses using an or-expression
;;
;; This version also takes time O(1 + (length of los) ^ 2)
;;


(define palindrome-2?
  (lambda (los)
    (cond
      ((or (null? los) (null? (cdr los))) #t)
      ((not (eq? (car los) (rac los))) #f)
      (else (palindrome-2? (cdr (rdc los)))))))


;;
;; An alternate version using no if's or cond's
;;
;; This version also takes time O(1 + (length of los) ^ 2)
;;


(define palindrome-3?
  (lambda (los)
    (or
     (null? los)
     (null? (cdr los))
     (and
      (eq? (car los) (rac los))
      (palindrome-3? (cdr (rdc los)))))))


;;
;; An alternate version using the linear-time reversal procedure
;; and a linear-time comparison predicate to perform the test
;; in time O(1 + (length of los))
;;


(define palindrome-4?
  (lambda (los)
    (equal-length-lists-same? los (rev-linear los))))


(define equal-length-lists-same?
  (lambda (los1 los2)
    (cond
      ((null? los1) #t)
      ((not (eq? (car los1) (car los2))) #f)
      (else (equal-length-lists-same? (cdr los1) (cdr los2))))))


;;;;
;;;; Equal-List-Length Predicates
;;;;


;;
;; This version takes time O(1 + (length of lisa) + (length of lisb))
;;


(define same-length?
  (lambda (lisa lisb)
    (cond
      ((null? lisa) (null? lisb))
      ((null? lisb) #f)
      (else (same-length? (cdr lisa) (cdr lisb))))))


;;
;; An alternate version using an and-expression and an
;; or-expression
;;
;; This version also takes time O(1 + (length of lisa) + (length of lisb))
;;


(define same-length-2?
  (lambda (lisa lisb)
    (cond
      ((and (null? lisa) (null? lisb)) #t)
      ((or (null? lisa) (null? lisb)) #f)
      (else (same-length-2? (cdr lisa) (cdr lisb))))))


;;
;; An alternate version using an if
;;
;; This version also takes time O(1 + (length of lisa) + (length of lisb))
;;


(define same-length-3?
  (lambda (lisa lisb)
    (if
     (or (null? lisa) (null? lisb))
     (and (null? lisa) (null? lisb))
     (same-length-3? (cdr lisa) (cdr lisb)))))


;;
;; An alternate version using no if's or cond's
;;
;; This version also takes time O(1 + (length of lisa) + (length of lisb))
;;


(define same-length-4?
  (lambda (lisa lisb)
    (or
     (and (null? lisa) (null? lisb))
     (and
      (not (null? lisa))
      (not (null? lisb))
      (same-length-4? (cdr lisa) (cdr lisb))))))

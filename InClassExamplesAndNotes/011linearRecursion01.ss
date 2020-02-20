#lang scheme

(require (lib "tls.ss" "Luther"))


;;;;;;;;
;;;;;;;; Examples of Simple (Linear) Recursion
;;;;;;;;


;;;;
;;;; "Right-End" List Operations
;;;;
;;;; All take time O(1 + (length of lis))
;;;;


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


;;;;
;;;; List-Concatenation Operations
;;;;


;;
;; This version takes time O(1 + (length of lisa)).
;;


(define concatenate
  (lambda (lisa lisb)
    (if
     (null? lisa)
     lisb
     (cons (car lisa) (concatenate (cdr lisa) lisb)))))


;;
;; An alternate version using the right-end operations
;; This version takes time O(1 + (length of lisa) * (length of lisb) + (length of lisb) ^ 2).
;;


(define concatenate-2
  (lambda (lisa lisb)
    (if
     (null? lisb)
     lisa
     (snoc (rac lisb) (concatenate-2 lisa (rdc lisb))))))


;;
;; An alternate version that recurses by reducing the size
;;     of lisa while increasing the size of lisb
;; This version takes time O(1 + (length of lisa) ^ 2).
;;


(define concatenate-3
  (lambda (lisa lisb)
    (if
     (null? lisa)
     lisb
     (concatenate-3 (rdc lisa) (cons (rac lisa) lisb)))))


;;
;; An alternate version that recurses by reducing the size
;;     of lisb while increasing the size of lisa
;; This version takes time O(1 + (length of lisa) * (length of lisb) + (length of lisb) ^ 2).
;;


(define concatenate-4
  (lambda (lisa lisb)
    (if
     (null? lisb)
     lisa
     (concatenate-4 (snoc (car lisb) lisa) (cdr lisb)))))


;;;;
;;;; List-Reversal Operations
;;;;


;;
;; This version takes time O((length of lis) ^ 2).
;;


(define rev
  (lambda (lis)
    (if
     (null? lis)
     '()
     (snoc (car lis) (rev (cdr lis))))))


;;
;; This version takes time O((length of lis) ^ 2).
;;


(define rev-2
  (lambda (lis)
    (if
     (null? lis)
     '()
     (cons (rac lis) (rev-2 (rdc lis))))))


;;
;; An alternate version using an accumulator parameter to perform the reversal
;; in time O(length of lis).
;;


(define rev-linear
  (lambda (lis)
    (rev-linear-aux lis '())))


(define rev-linear-aux
  (lambda (lis result)
    (if
     (null? lis)
     result
     (rev-linear-aux (cdr lis) (cons (car lis) result)))))

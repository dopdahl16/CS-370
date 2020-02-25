;Assignment 2
;Author: Daniel Opdahl
;Date: 2-25-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;
;
;

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



;
;
;

(define elide-length
  (lambda (lis+ n)
    (cond
      ((null? lis+) '())
      ((zero? n) '(...))
      ((null? (cdr lis+)) (cons (car lis+) '()))
      (else (cons (get-first-n-elements (car lis+) n) (elide-length (cdr lis+) (- n 1))))
      )))



;
;
;

(define get-first-n-elements
  (lambda (lis n)
    (cond
      ((eq? n 1) (cons (car lis) '()))
      (else (cons (car lis) (get-first-n-elements (cdr lis) (- n 1)))))))



;
;
;

(define elide-depth
  (lambda (lis+ n)
    (cond
      ((zero? n) '&)
      ((null? lis+) '()))))



;
;
;

(define perms
  (lambda (loa)
    (cond
      )))



;
;
;

(define subst-every-other
  (lambda (old new los+)
    (cond
      ((null? los+) '())
      ((eq? (car los+) old) (cons new (cdr los+)))
      (else (cons (subst-every-other old new (car los+)) (subst-every-other old new (cdr los+))))
      )))


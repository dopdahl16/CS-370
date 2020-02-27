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
      (else (cons (get-first-n-elements (car lis+) n) (elide-length (cdr lis+) (- n 1)))))))



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
      ((and (zero? n) (null? lis+)) '&)
      ((zero? n) (cons '& (elide-depth (cdr lis+) n)))
      ((null? lis+) '())
      ((null? (cdr lis+)) (car lis+))
      (else (cons (elide-depth (car lis+) (- n 1)) (elide-depth (cdr lis+) (- n 1)))))))


(elide-depth '(() (a) ((a)) (((a)))) 1)
;
;
;

;(define perms
;  (lambda (loa)
;    (cond
;      ((null? loa) '())
;      ((null? (car loa)) (car loa))
;      ((null? (cdr (cdr loa))) (cons (cons (car loa) (cdr loa)) (cons (cons (car (cdr loa)) (cons (car loa) '())) '())))
;      (else (insert-into-every-space (car loa) (perms (cdr loa)))))))
;
;
;
;(define insert-into-every-space
;  (lambda (char lol)
;    (cond
;      ((null? lol) '()) 
;      (cons 




(define subst-every-other
  (lambda (old new los+)
    (subst-every-other-helper old new los+ 0)))

(define subst-every-other-helper
  (lambda (old new los+ bit)
    (cond
      ((null? los+) '())
      ((not (atom? (car los+))) (cons (subst-every-other-helper old new (car los+) bit) (subst-every-other-helper old new (cdr los+) bit)))
      ((and (eq? (car los+) old) (not (zero? bit))) (cons old (subst-every-other-helper old new (cdr los+) 0)))
      ((and (eq? (car los+) old) (zero? bit)) (cons new (subst-every-other-helper old new (cdr los+) 1)))
      (else (cons (car los+) (subst-every-other-helper old new (cdr los+) bit))))))
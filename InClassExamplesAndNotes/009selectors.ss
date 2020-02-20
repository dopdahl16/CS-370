#lang scheme

(require (lib "tls.ss" "Luther"))


(define 1st
  (lambda (lis)
    (car lis)))


;
; Alternatively
;
;
;(define 1st car)


(define 2nd
  (lambda (lis)
    (car (cdr lis))))


;
; Alternatively
;
;
;(define 2nd
;  (lambda (lis)
;    (1st (cdr lis))))


;
; Alternatively
;
;
;(define 2nd cadr)


(define 3rd
  (lambda (lis)
    (car (cdr (cdr lis)))))


;
; Alternatively
;
;
;(define 3rd
;  (lambda (lis)
;    (1st (cdr (cdr lis)))))


;
; Alternatively
;
;
;(define 3rd
;  (lambda (lis)
;    (2nd (cdr lis))))


;
; Alternatively
;
;
;(define 3rd caddr)


(define 1st-of-1st
  (lambda (lol)
    (car (car lol))))


;
; Alternatively
;
;
;(define 1st-of-1st
;  (lambda (lol)
;    (1st (1st lis))))


;
; Alternatively
;
;
;(define 1st-of-1st caar)


(define 2nd-of-2nd
  (lambda (lol)
    (car (cdr (car (cdr lol))))))


;
; Alternatively
;
;
;(define 2nd-of-2nd
;  (lambda (lol)
;    (2nd (2nd lol))))


;
; Alternatively
;
;
;(define 2nd-of-2nd cadadr)

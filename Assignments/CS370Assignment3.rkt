;Assignment 3
;Author: Daniel Opdahl
;Date: 2-25-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;
;
;Done

(define curried-binary
  (lambda (binaryProc)
    (lambda (element1)
      (lambda (element2)
        (binaryProc element1 element2)))))



;
;
;

(define uncurried-binary
  (lambda (curriedBinaryProc)
    '()))
    


;
;
;Done

(define adjacent-related-grouped
  (lambda (related? lis)
    (cond
      ((null? lis) '())
      ((null? (cdr lis)) (cons lis '()))
      ((related? (car lis) (car (cdr lis))) (cons (cons (car lis) (car (adjacent-related-grouped related? (cdr lis)))) (cdr (adjacent-related-grouped related? (cdr lis)))))
      (else (cons (cons (car lis) '()) (adjacent-related-grouped related? (cdr lis)))))))



;
;
;

(define rreducer
  (lambda (binaryProc unaryProc zeroaryProc)
    '()))



;
;
;

(define lreducer
  (lambda (binaryProc unaryProc zeroaryProc)
    '()))



;
;
;

(define subst-every-other-sf
  (lambda (old new los+ succeed)
    '()))
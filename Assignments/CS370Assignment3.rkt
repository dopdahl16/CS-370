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
;Done

(define uncurried-binary
  (lambda (curriedBinaryProc)
      (lambda (element1 element2)
        ((curriedBinaryProc element1) element2))))
    


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
;Done

(define rreducer
  (lambda (binaryProc unaryProc zeroaryProc)
    (lambda (lat)
      (cond
        ((eq? (length lat) 0) (zeroaryProc))
        ((eq? (length lat) 1) (unaryProc (car lat)))
        (else (binaryProc (car lat) ((rreducer binaryProc unaryProc zeroaryProc) (cdr lat))))))))



;
;
;

(define lreducer
  (lambda (binaryProc unaryProc zeroaryProc)
    (lambda (lat)
      (cond
        ((eq? (length lat) 0) (zeroaryProc))
        ((eq? (length lat) 1) (unaryProc (car lat)))
        (else (binaryProc ((lreducer binaryProc unaryProc zeroaryProc) (cdr lat)) (car lat)))))))



;
;
;

(define subst-every-other-sf
  (lambda (old new los+ succeed)
    '()))
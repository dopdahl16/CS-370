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
;Done

(define lreducer
  (lambda (binaryProc unaryProc zeroaryProc)
    (lambda (lat)
      (cond
        ((eq? (length lat) 0) (zeroaryProc))
        ((eq? (length lat) 1) (unaryProc (car lat)))
        (else ((lreducer binaryProc unaryProc zeroaryProc) (cons (binaryProc (car lat) (car (cdr lat))) (cdr (cdr lat)))))))))


;
;
;

(define subst-every-other-sf
  (lambda (old new los+ succeed)
    (subst-every-other-helper old new los+ 1 succeed)))



;
;
;

(define subst-every-other-helper
  (lambda (old new los+ bit succeed)
    (cond
      ((null? los+) (succeed '() #f))
      ((not (atom? (car los+)))
       (succeed (cons (subst-every-other-helper old new (car los+) bit succeed)
             (subst-every-other-helper old new (cdr los+) bit succeed)) (odd? (count old 0 los+))))
      ((and (eq? (car los+) old) (not (zero? bit)))
       (succeed (cons new
             (subst-every-other-helper old new (cdr los+) 0 succeed)) (odd? (count old 0 los+))))
      ((and (eq? (car los+) old) (zero? bit))
       (succeed (cons old
             (subst-every-other-helper old new (cdr los+) 1 succeed)) (odd? (count old 0 los+))))
      (else
       (succeed (cons (car los+)
             (subst-every-other-helper old new (cdr los+) bit succeed)) (odd? (count old 0 los+)))))))



(define count
  (lambda (old tot los+)
    (cond
      ((null? los+) '())
      ((and (eq? (cdr los+) '()) (eq? (car los+) old)) (+ tot 1))
      ((and (eq? (cdr los+) '()) (not (eq? (car los+) old))) tot)
      ((eq? (car los+) old) (count old (+ tot 1) (cdr los+)))
      (else (count old tot (cdr los+))))))








(subst-every-other-sf 'a 'x '(a (b (a (c (a (d ())))))) (lambda (result
replaced) result))
(subst-every-other-sf 'a 'x '(a (b (a (c (a (d ())))))) (lambda (result
replaced) replaced))
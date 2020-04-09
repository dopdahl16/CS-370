;Exam 1, Question 5
;Author: Daniel Opdahl
;Date: 3-29-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;Returns the convolution of the elements of lisa with the elements of lisb; that is, if lisa is (x1 x2 ... xn) and lisb is (y1 y2 ... yn), convolution returns the list ((x1 yn) (x2 yn-1) ... (xn y1))
;lisa and lisb must be lists of symbols that are the same length, and stage-length must be the list '(0) in order for this procedure to work as intended
;O(length of lisa + length of lisb)

(define convolution
  (lambda (lisa lisb stage-list)
    (cond
      ((eq? (car stage-list) 0)
       (cond
         ((null? lisb) '())
         ((null? lisa) '())
         (else (convolution lisa (append (convolution lisa (cdr lisb) '(1)) (cons (cons (car lisb) '()) '())) '(2)))))
      ((eq? (car stage-list) 1)
       (cond
         ((null? lisb) '())
         ((null? lisa) '())
         (else (append (convolution lisa (cdr lisb) '(1)) (cons (cons (car lisb) '()) '())))))
      ((eq? (car stage-list) 2)
       (cond
         ((null? lisb) '())
         (else (cons (cons (car lisa) (car lisb)) (convolution (cdr lisa) (cdr lisb) '(2)))))))))
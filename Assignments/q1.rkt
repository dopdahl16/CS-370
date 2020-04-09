;Exam 1, Question 1
;Author: Daniel Opdahl
;Date: 3-29-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;Returns a list containing the same elements as lis, but nested deeper and deeper as you go from right to left.
;lis must be a list
;O(length of lis)

(define deepen-right
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((eq? (length lis) 1) (cons (car lis) '()))
      (else (cons (car lis) (cons (deepen-right (cdr lis)) '()))))))
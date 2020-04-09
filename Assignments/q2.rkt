;Exam 1, Question 2
;Author: Daniel Opdahl
;Date: 3-29-2020

#lang scheme

(require (lib "tls.ss" "Luther"))



;Removes any and all null lists from lis+, ultimately leaving the result with no null lists whatsoever, noting that the removal of more-nested nulls can create less-nested nulls.
;lis+ must be an arbitrarily-complex list
;O(length of lis+)

(define without-nulls
  (lambda (lis+)
    (cond
      ((null? lis+) '())
      ((atom? (car lis+)) (cons (car lis+) (without-nulls (cdr lis+))))
      ((empty? (car lis+)) (without-nulls (cdr lis+)))
      (else (cons (without-nulls (car lis+)) (without-nulls (cdr lis+)))))))



;Helper function for without-nulls. Unary predicate that determines if lis+ is empty (if it contains no atoms) or not, returning true if it contains no atoms and false if it does.
;lis+ must be an arbitrarily-complex list
;O(length of lis+ * depth of lis+)

(define empty?
  (lambda (lis+)
    (cond
      ((null? lis+) #t)
      ((atom? (car lis+)) #f)
      (else (and (empty? (car lis+)) (empty? (cdr lis+)))))))
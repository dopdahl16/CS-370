#lang scheme

(require (lib "tls.ss" "Luther"))


(define rev
  (lambda (lis)
    (if
     (null? lis)
     '()
     (snoc (car lis) (rev (cdr lis))))))


(define rev*
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((not (atom? (car lis)))
       (snoc (rev* (car lis)) (rev* (cdr lis))))
      (else
       (snoc (car lis) (rev* (cdr lis)))))))


(define count-atoms
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((atom? (car lis)) (+ 1 (count-atoms (cdr lis))))
      (else (count-atoms (cdr lis))))))


(define count-atoms*
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((atom? (car lis)) (+ 1 (count-atoms* (cdr lis))))
      (else (+ (count-atoms* (car lis)) (count-atoms* (cdr lis)))))))


(define crush
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((atom? (car lis)) (cons (car lis) (crush (cdr lis))))
      (else (append (crush (car lis)) (crush (cdr lis)))))))


(define depth
  (lambda (x)
    (cond
      ((atom? x) 0)
      ((null? x) 1)
      (else (max (+ 1 (depth (car x))) (depth (cdr x)))))))


;
; Support Code From Previous Lectures
;


(define snoc
  (lambda (x lis)
    (if
     (null? lis)
     (cons x '())
     (cons (car lis) (snoc x (cdr lis))))))

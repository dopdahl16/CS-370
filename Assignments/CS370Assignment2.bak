#lang scheme

(require (lib "tls.ss" "Luther"))

(define same-structure
  (lambda (x y)
    (cond
      ((and (atom? x) (not(atom? y))) false)
      ((and (not(atom? x)) (atom? y)) false)
      ((and (null? x) (not(null? y))) false)
      ((and (not(null? x)) (null? y)) false)
      ((and (null? x) (null? y)) true)
      ((and (atom? x) (atom? y)) true)
      (else (same-structure (cdr x) (cdr y))))))

(same-structure '(a (b (c (d)))) '((1) 2))
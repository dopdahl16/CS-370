#lang scheme

(require (lib "tls.ss" "Luther"))


(define fact
  (lambda (n)
    (if
     (zero? n)
     1
     (* n (fact (- n 1))))))


;
; A version of fact that uses an accumulator parameter
; (The use of the accumulator doesn't really improve things:
; it's simply provided as an example.)
;


(define fact-accum
  (lambda (n result)
    (if
     (zero? n)
     result
     (fact-accum (- n 1) (* result n)))))


(define choose
  (lambda (n m)
    (cond
      ((> m n) 0)
      ((zero? m) 1)
      (else
       (+
        (choose (- n 1) (- m 1))
        (choose (- n 1) m))))))

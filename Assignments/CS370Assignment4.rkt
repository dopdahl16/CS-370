#lang scheme

(require (lib "tls.ss" "Luther"))

(define do1
  (lambda ()
    (void)
    (display 'x)
    (display 'y)
    (display 'z)))

;running this line of code
(do1)

;produces the same output as this line of code
(begin (display 'x) (display 'y) (display 'z))




(define do2
  (lambda ()
    (void)))

;running this line of code
(do2)

;produces the same output as this line of code
(begin)
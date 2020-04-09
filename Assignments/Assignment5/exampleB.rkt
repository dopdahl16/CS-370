(scheme-value
 '((lambda (x)
     ((lambda (x p)
        (p))
      'a
      (lambda ()
        (cons x (cons (static x) (cons (dynamic x) '()))))))
   'b))
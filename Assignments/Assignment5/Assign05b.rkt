;Assignment 5b
;Author: Daniel Opdahl
;Date: 4-9-2020

#lang scheme


;
; A Scheme Meta-Circular Interpreter, Version 0.15b
;

(define scheme-value
  (lambda (expr)
    (expr-value
     expr
     '((car . *car*) (cdr . *cdr*) (cons . *cons*) (eq? . *eq?*) (symbol? . *symbol?*) (apply . *apply*) (map . *map*) (pair? . *pair?*))
     '((car . *car*) (cdr . *cdr*) (cons . *cons*) (eq? . *eq?*) (symbol? . *symbol?*) (apply . *apply*) (map . *map*) (pair? . *pair?*)))))

(define expr-value
  (lambda (expr env-static env-dynamic)
    (cond
      ((eq? expr #t) #t)
      ((eq? expr #f) #f)
      ((eq? expr '()) '())
      ((symbol? expr) (sym-value expr env-static))
      ((eq? (car expr) 'static) (sym-value (cadr expr) env-static))
      ((eq? (car expr) 'dynamic) (sym-value (cadr expr) env-dynamic))
      ((eq? (car expr) 'quote) (cadr expr))
      ((eq? (car expr) 'cond) (cond-value (cdr expr) env-static))
      ((eq? (car expr) 'lambda) (cons '*closure* (cons (cadr expr) (cons (caddr expr) (cons env-static '())))))
      (else (app-value (expr-value (car expr) env-static env-dynamic) (map (lambda (rand) (expr-value rand env-static env-dynamic)) (cdr expr)) env-static env-dynamic)))))

(define sym-value
  (lambda (sym env)
    (cond
      ((eq? sym (caar env)) (cdar env))
      (else (sym-value sym (cdr env))))))

(define cond-value
  (lambda (clauses env)
    (cond
      ((eq? (caar clauses) 'else) (expr-value (cadar clauses) env))
      ((eq? (expr-value (caar clauses) env) #f) (cond-value (cdr clauses) env))
      (else (expr-value (cadar clauses) env)))))

(define app-value
  (lambda (rator rand-list env-static env-dynamic)
    (cond
      ((symbol? rator)
       (cond
         ((eq? rator '*car*) (caar rand-list))
         ((eq? rator '*cdr*) (cdar rand-list))
         ((eq? rator '*cons*) (cons (car rand-list) (cadr rand-list)))
         ((eq? rator '*eq?*) (eq? (car rand-list) (cadr rand-list)))
         ((eq? rator '*symbol?*) (symbol? (car rand-list)))
         ((eq? rator '*apply*) (app-value (car rand-list) (cadr rand-list) env-static env-dynamic))
         ((eq? rator '*map*) (map (lambda (rand) (app-value (car rand-list) (cons rand '()) env-static env-dynamic)) (cadr rand-list)))
         ((eq? rator '*pair?*) (pair? (car rand-list)))))
      ((eq? (car rator) '*closure*) (expr-value (caddr rator) (augmented-env (cadr rator) rand-list (cadddr rator)) (cons env-dynamic (augmented-env (cadr rator) rand-list (cadddr rator))))))))

(define augmented-env
  (lambda (sym-list rand-list env)
    (cond
      ((eq? sym-list '()) env)
      ((symbol? sym-list) (cons (cons sym-list rand-list) env))
      (else (cons (cons (car sym-list) (car rand-list)) (augmented-env (cdr sym-list) (cdr rand-list) env))))))



(scheme-value
 '((lambda (x)
     ((lambda (x p)
        (p))
      'a
      (lambda ()
        (cons x (cons (static x) (cons (dynamic x) '()))))))
   'b))
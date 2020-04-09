#lang scheme

(require (lib "tls.ss" "Luther"))



([lambda (moot) ([lambda (moot) (display 'c)] {display 'b})] {display 'a})

(begin (display 'a) (display 'b) (display 'c))


((lambda (x) (+ x x)) 4)
[λ (dummy) ([λ (dummy) (display 'c)] {display 'b})]
'()
([lambda (moot) (display 'b)] (display 'a))
'()
[λ (dummy) (display 'c)]

'()
(begin cons (begin (begin (display 'a))) '())
'()
([lambda (moot) (display (cons 'a '()))] (display 'a))

(begin (begin (begin )))






'aaaaaaaaaaaa
([lambda (moot) ([lambda (moot) (display 'z)] {display 'y})] {display 'x})

'bbbbbbbbbb

(begin (display 'a) (display (list (begin 'x 'y 'z))))

((lambda (moot) (display (list (begin 'x 'y 'z)))) (display 'a))
((lambda (moot) (display (list ([lambda (moot) ([lambda (moot) 'z] 'y)] 'x)
))) (display 'a))
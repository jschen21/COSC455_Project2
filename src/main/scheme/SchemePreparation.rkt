#lang racket
;part 1
(define first (lambda (x) (car x)))
(define second (lambda (x) (cadr x)))
(define third (lambda (x) (caddr x)))
(define fourth (lambda (x) (cadddr x)))
(define fifth (lambda (x) (car(cddddr x))))
(define rest (lambda (x) (cdr x)))
(define family '(josh sara erin sandy jon))
(first family)
(second family)
(third family)
(fourth family)
(fifth family)

;part 2
(define boolList (list #t #f #t #t #t #f #f #t))
(define (truecount aList)
  (length (filter identity aList)))
(truecount boolList)

;part 3
(define sqrlist (list 1 2 3 4 5 6))
(define (squarelist alist)
  (map (lambda (x) (* x x)) alist))
(squarelist sqrlist)

;part 4
(define filtlist (list 122 12 133 98 500))
(define (hundreds? alist)
  (filter (lambda (x) (> x 100)) alist))
(hundreds? filtlist)

;part 5
(define (collatz n)
  (display n)(newline)
  (cond ((eq? n 1) (display "end")(newline))
        ((even? n) (collatz (/ n 2)))
        ((odd? n) (collatz (+(* 3 n) 1)))))
(collatz 30)

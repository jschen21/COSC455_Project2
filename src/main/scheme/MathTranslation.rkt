#lang racket
(define chinese '(ling yi er san si wu liu qi ba jiu shi))
(define english '(zero one two three four five six seven eight nine ten))

(define (go alist)
  (display "Translation: ")(showlist (convert alist))(newline)
  (display "Addition: ") (showplus (convert alist))(display " = ")(display (sum (convert alist)))(newline)
  (display "Multiplication: ")(showmulti (convert alist))(display " = ")(display (product (convert alist)))(newline))

(define (convert alist)
  (cond
    ((null? alist) '())
    ((eq? (member? (car alist) chinese) #t) (cons (index-of chinese (car alist)) (convert (cdr alist))))
    ((eq? (member? (car alist) english) #t) (cons (index-of english (car alist)) (convert (cdr alist))))
    (else (convert (cdr alist)))))
 
(define (member? x alist)
  (if (null? alist) #f
      (if (eq? x (car alist)) #t
          (member? x (cdr alist)))))

(define (sum alist)
  (if
   (null? alist) 0
   (+ (car alist) (sum (cdr alist)))))

(define (product alist)
  (if
   (null? alist) 1
   (* (car alist) (product (cdr alist)))))

(define (showplus alist)
 (display (car alist))
  (map (lambda (x) (display " + ")(display x)) (cdr alist)))

(define (showmulti alist)
 (display (car alist))
  (map (lambda (x) (display " * ")(display x)) (cdr alist)))

(define (showlist alist)
 (display (car alist))
  (map (lambda (x) (display " ")(display x)) (cdr alist)))
  

(go '(yi nine six ba))
(go '(yi josh three si))
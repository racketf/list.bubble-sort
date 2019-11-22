#lang racket

; (atmost1? lst): L -> B
; return #t if list has atmost 1 element(s).
(define atmost1?
  (lambda (lst)
    (or (null? lst) (null? (cdr lst)))))

; (swap lst): L -> L
; swaps the first two elements of list
(define swap
  (lambda (lst)
    (if (atmost1? lst)
        lst
        (cons (cadr lst) (cons (car lst) (cddr lst))))))

; (swap-by lst f): L X F -> L
; swaps the first 2 elements of list using given function
(define swap-by
  (lambda (lst f)
    (if (or (atmost1? lst) (f (car lst) (cadr lst)))
        lst
        (swap lst))))

; (bubble-once-by lst f): L X F -> L
; runs a single pass of bubble sort on list
(define bubble-once-by
  (lambda (lst f)
    (if (atmost1? lst)
        lst
        (let ([lst (swap-by lst f)])
          (cons (car lst) (bubble-once-by (cdr lst) f))))))

; (bubble-sort-by lst f): L X F -> L
; bubble sorts a list with given predicate f
(define bubble-sort-by
  (lambda (lst f)
    (if (atmost1? lst)
        lst
        (bubble-once-by (cons (car lst) (bubble-sort-by (cdr lst) f)) f))))

; (bubble-sort lst): L -> L
; bubble sorts a list in ascending order
(define bubble-sort
  (lambda (lst)
    (bubble-sort-by lst <=)))

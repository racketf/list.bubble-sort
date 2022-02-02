#lang racket

; (list.atmost1? lst): L -> B
; return #t if list has atmost 1 element(s).
(define list.atmost1?
  (lambda (lst)
    (or (null? lst) (null? (cdr lst)))))

; (list.swap lst): L -> L
; swaps the first two elements of list
(define list.swap
  (lambda (lst)
    (if (list.atmost1? lst)
        lst
        (cons (cadr lst) (cons (car lst) (cddr lst))))))

; (list.swap-by lst f): L X F -> L
; swaps the first 2 elements of list using given function
(define list.swap-by
  (lambda (lst f)
    (if (or (list.atmost1? lst) (f (car lst) (cadr lst)))
        lst
        (list.swap lst))))

; (list.bubble-once-by lst f): L X F -> L
; runs a single pass of bubble sort on list
(define list.bubble-once-by
  (lambda (lst f)
    (if (list.atmost1? lst)
        lst
        (let ([lst (list.swap-by lst f)])
          (cons (car lst) (list.bubble-once-by (cdr lst) f))))))

; (list.bubble-sort-by lst f): L X F -> L
; bubble sorts a list with given predicate f
(define list.bubble-sort-by
  (lambda (lst f)
    (if (list.atmost1? lst)
        lst
        (list.bubble-once-by (cons (car lst) (list.bubble-sort-by (cdr lst) f)) f))))

; (list.bubble-sort lst): L -> L
; bubble sorts a list in ascending order
(define list.bubble-sort
  (lambda (lst)
    (list.bubble-sort-by lst <=)))

#lang typed/racket

(provide list-flatmap)


(: rappend (∀ (a) (-> (Listof a) (Listof a) (Listof a))))
(define (rappend xs ys)
  (if (null? xs) ys
    (rappend (cdr xs) (cons (car xs) ys))))


(: list-flatmap (∀ (a b) (-> (-> a (Listof b)) (Listof a) (Listof b))))
(define (list-flatmap f xs)
  (: recur (-> (Listof a) (Listof b) (Listof b)))
  (define (recur xs accum)
    (if (null? xs) (reverse accum)
      (recur (cdr xs) (rappend (f (car xs)) accum))))
  (recur xs '()))

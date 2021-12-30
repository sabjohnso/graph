#lang typed/racket

(provide :->)

(define-syntax :->
  (syntax-rules ()
    [(:-> x) x]
    [(:-> x (f xs ...) more-forms ...)
     (:-> (f x xs ...) more-forms ...)]))

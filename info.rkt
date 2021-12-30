#lang info
(define collection "graph")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "rackunit-typed"))
(define scribblings '(("scribblings/graph.scrbl" ())))
(define pkg-desc "Data structures, algorithms and languages for graphs.")
(define version "0.0")
(define license '(MIT))

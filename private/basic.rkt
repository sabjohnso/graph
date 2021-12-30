#lang typed/racket

;; This module introduces a data structure that is the foundation
;; for directed and undirected graphs The data structure is bare and
;; is not user facing: directed and undirected graph structures encapsulate
;; this data.

(struct exn:fail:basic-graph-link exn:fail ())

(define-type Node  (Setof Integer))
(define-type Link-Table (Immutable-HashTable Integer Node))
(define-type Basic-Graph (Pairof Link-Table Integer))

(: empty-basic-graph Basic-Graph)
(define empty-basic-graph (cons (make-immutable-hash '()) 0))

(: basic-graph-link-table (-> Basic-Graph Link-Table))
(define (basic-graph-link-table graph)
  (car graph))

(: basic-graph-counter (-> Basic-Graph Integer))
(define (basic-graph-counter graph)
  (cdr graph))

(: basic-graph-empty? (-> Basic-Graph Boolean))
(define (basic-graph-empty? graph)
  (zero? (hash-count (basic-graph-link-table graph))))

(: basic-graph-node-count (-> Basic-Graph Integer))
(define (basic-graph-node-count graph)
  (hash-count (basic-graph-link-table graph)))

(: basic-graph-link-count (-> Basic-Graph Integer))
(define (basic-graph-link-count graph)
  (ann (for/sum ([node : Node (hash-values (basic-graph-link-table graph))])
         (set-count node)) Integer))

(: basic-graph-has-node? (-> Basic-Graph Integer Boolean))
(define (basic-graph-has-node? graph node-index)
  (hash-has-key? (basic-graph-link-table graph) node-index))

(: basic-graph-has-link? (-> Basic-Graph Integer Integer Boolean))
(define (basic-graph-has-link? graph source target)
  (and (basic-graph-has-node? graph source)
       (basic-graph-has-node? graph target)
       (set-member? (hash-ref (basic-graph-link-table graph) source)
                    target)))

(: basic-graph-has-undirected-link? (-> Basic-Graph Integer Integer Boolean))
(define (basic-graph-has-undirected-link? graph node1 node2)
  (and (basic-graph-has-link? graph node1 node2)
       (basic-graph-has-link? graph node2 node1)))

(define (basic-graph-make-node)
  (ann (set) Node))

(: basic-graph-add-node (-> Basic-Graph Basic-Graph))
(define (basic-graph-add-node graph)
  (match-let ([(cons table counter) graph])
    (#{cons @ Link-Table Integer}
     (hash-set table counter (basic-graph-make-node))
          (add1 counter))))

(: basic-graph-add-link (-> Basic-Graph Integer Integer  Basic-Graph))
(define (basic-graph-add-link graph source target)
  (match-let ([(cons table counter) graph])
    (if (and (hash-has-key? table source)
             (hash-has-key? table target))
        (cons
         (hash-set table source (set-add (hash-ref table source) target))
         counter)
      (raise (exn:fail:basic-graph-link
              (format "invalid source (~a) or target (~a)" source target)
              (current-continuation-marks))))))

(: basic-graph-remove-link (-> Basic-Graph Integer Integer Basic-Graph))
(define (basic-graph-remove-link graph source target)
  (if (basic-graph-has-link? graph source target)
      (match-let ([(cons link-table counter) graph])
        (cons (hash-set link-table source (set-remove (hash-ref link-table source) target))
              counter))
    graph))

(: basic-graph-remove-node (-> Basic-Graph Integer Basic-Graph))
(define (basic-graph-remove-node graph target)
  (if (basic-graph-has-node? graph target)
      (match-let ([(cons link-table counter)
                   (for/fold ([graph : Basic-Graph graph])
                       ([source : Integer (hash-keys (basic-graph-link-table graph))])
                     (basic-graph-remove-link graph source target))])
        (cons (hash-remove link-table target) counter))
    graph))


(module+ test
  ;; Note: As none of the data types or functions herein are user facing, the
  ;; tests are of implementation details and may be removed if broken.

  (require typed/rackunit "pipe.rkt")

  (check-true (basic-graph-empty? empty-basic-graph))
  (check-equal? (basic-graph-node-count empty-basic-graph) 0)
  (check-equal? (basic-graph-link-count empty-basic-graph) 0)
  (check-false (basic-graph-has-node? empty-basic-graph 0))
  (check-false (basic-graph-has-link? empty-basic-graph 0 1))
  (check-false (basic-graph-has-undirected-link? empty-basic-graph 0 1))


  (let ([graph (:-> empty-basic-graph
                    (basic-graph-add-node)
                    (basic-graph-add-node)
                    (basic-graph-add-link 0 1))])
    (check-false (basic-graph-empty? graph))
    (check-equal? (basic-graph-node-count graph) 2)
    (check-equal? (basic-graph-link-count graph) 1)
    (check-true (basic-graph-has-node? graph 0))
    (check-true (basic-graph-has-node? graph 1))
    (check-false (basic-graph-has-node? graph 2))
    (check-true (basic-graph-has-link? graph 0 1))
    (check-false (basic-graph-has-link? graph 1 0))
    (check-false (basic-graph-has-undirected-link? graph 0 1))

    (let ([graph (basic-graph-add-link graph 1 0)])
      (check-true (basic-graph-has-link? graph 1 0))
      (check-true (basic-graph-has-undirected-link? graph 0 1))
      (check-true (basic-graph-has-undirected-link? graph 1 0))))

  (let ([graph (:-> empty-basic-graph
                    (basic-graph-add-node)
                    (basic-graph-add-node)
                    (basic-graph-add-node)
                    (basic-graph-add-link 0 1)
                    (basic-graph-add-link 1 2)
                    (basic-graph-add-link 2 0))])
    (check-equal? (basic-graph-node-count graph) 3)
    (check-equal? (basic-graph-link-count graph) 3)
    (check-true (basic-graph-has-node? graph 1))
    (check-true (basic-graph-has-link? graph 0 1))
    (check-true (basic-graph-has-link? graph 1 2))

    (let ([graph (basic-graph-remove-node graph 1)])
      (check-equal? (basic-graph-node-count graph) 2)
      (check-equal? (basic-graph-link-count graph) 1)
      (check-false (basic-graph-has-node? graph 1))
      (check-false (basic-graph-has-link? graph 0 1))
      (check-false (basic-graph-has-link? graph 1 0)))))

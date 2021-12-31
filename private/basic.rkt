#lang typed/racket

;; This module introduces a data structure that is the foundation
;; for directed and undirected graphs The data structure is bare and
;; is not user facing: directed and undirected graph structures encapsulate
;; this data.
(provide
 Node Link-Table Graph
 make-graph
 empty-graph
 graph-has-node?
 graph-node-count
 graph-has-link?
 graph-link-count
 graph-complete-graph
 graph-complete?
 graph-node-neighbors)

(require "pipe.rkt" "node.rkt" "link-table.rkt")

(struct exn:fail:graph-link exn:fail () #:transparent)

(define-type Graph (Pairof Link-Table Integer))
(define-type Node-Map (Immutable-HashTable Integer Integer))

(: make-graph (-> Link-Table Node-Label Graph))
(define (make-graph link-table counter)
  (cons link-table counter))

(: empty-graph Graph)
(define empty-graph (make-graph (make-immutable-hash '()) 0))

(: graph-link-table (-> Graph Link-Table))
(define (graph-link-table graph)
  (car graph))

(: graph-counter (-> Graph Integer))
(define (graph-counter graph)
  (cdr graph))

(: graph-update-link-table (-> Graph Link-Table Graph))
(define (graph-update-link-table graph new-link-table)
  (cons new-link-table
        (graph-counter graph)))

(: graph-empty? (-> Graph Boolean))
(define (graph-empty? graph)
  (link-table-empty? (graph-link-table graph)))

(: graph-node-count (-> Graph Integer))
(define (graph-node-count graph)
  (link-table-node-count (graph-link-table graph)))

(: graph-link-count (-> Graph Integer))
(define (graph-link-count graph)
  (link-table-link-count (graph-link-table graph)))

(: graph-node (-> Graph Node-Label Node))
(define (graph-node graph label)
  (link-table-node (graph-link-table graph) label))

(: graph-node-neighbors (-> Graph Node-Label (Listof Node-Label)))
(define (graph-node-neighbors graph label)
  (node-neighbors (graph-node graph label)))

(: graph-nodes (-> Graph (Listof Node-Label)))
(define (graph-nodes graph)
  (link-table-nodes (graph-link-table graph)))

(: graph-node-degree (-> Graph Node-Label Integer))
(define (graph-node-degree graph label)
  (node-degree (graph-node graph label)))

(: graph-has-node? (-> Graph Node-Label Boolean))
(define (graph-has-node? graph label)
  (link-table-has-node? (graph-link-table graph) label))

(: graph-has-link? (-> Graph Node-Label Node-Label Boolean))
(define (graph-has-link? graph source-label target-label)
  (link-table-has-link? (graph-link-table graph) source-label target-label))

(: graph-has-undirected-link? (-> Graph Node-Label Node-Label Boolean))
(define (graph-has-undirected-link? graph label1 label2)
  (and (graph-has-link? graph label1 label2)
       (graph-has-link? graph label2 label1)))

(: graph-add-node (-> Graph Graph))
(define (graph-add-node graph)
  (cons (link-table-add-node (graph-link-table graph) (graph-counter graph))
        (add1 (graph-counter graph))))

(: graph-add-link (-> Graph Integer Integer  Graph))
(define (graph-add-link graph source target)
  (match-let ([(cons table counter) graph])
    (if (and (hash-has-key? table source)
             (hash-has-key? table target))
        (cons
         (hash-set table source (set-add (hash-ref table source) target))
         counter)
      (raise (exn:fail:graph-link
              (format "invalid source (~a) or target (~a)" source target)
              (current-continuation-marks))))))

(: graph-add-undirected-link (-> Graph Node-Label Node-Label Graph))
(define (graph-add-undirected-link graph label1 label2)
  (graph-update-link-table graph
    (link-table-add-undirected-link (graph-link-table graph)
     label1 label2)))

(: graph-remove-link (-> Graph Node-Label Node-Label Graph))
(define (graph-remove-link graph source-label target-label)
  (if (graph-has-link? graph source-label target-label)
      (graph-update-link-table graph
        (link-table-remove-link (graph-link-table graph) source-label target-label))
    graph))

(: graph-remove-node (-> Graph Node-Label Graph))
(define (graph-remove-node graph label)
  (if (graph-has-node? graph label)
      (graph-update-link-table graph (link-table-remove-node (graph-link-table graph) label))
    graph))

(: graph-isomorphism? (-> Graph Graph Node-Map Boolean))
(define (graph-isomorphism? graph1 graph2 node-map)

  (: aux (-> Graph Graph Node-Map Boolean))
  (define (aux graph1 graph2 node-map)
    (let ([link-table : Link-Table (graph-link-table graph1)])
      (for/fold ([result : Boolean #t])
          ([source : Node-Label (link-table-nodes link-table)]
           #:break (not result))
        (let ([neighbors : Node (link-table-node link-table source)])
          (for/fold ([result : Boolean #t])
              ([target : Integer neighbors]
               #:break (not result))
            (graph-has-link? graph2 (hash-ref node-map source) (hash-ref node-map target)))))))

  (and (= (graph-node-count graph1) (graph-node-count graph2))
       (= (graph-link-count graph1) (graph-link-count graph2))
       (aux graph1 graph2 node-map)))

(: graph-complete-graph (-> Integer Graph))
(define (graph-complete-graph n)
  (let ([unlinked-graph
         (for/fold ([graph : Graph empty-graph])
             ([i (in-range n)])
           (graph-add-node graph))])
    (for*/fold ([graph : Graph unlinked-graph])
        ([source : Integer (in-range n)]
         [target : Integer (in-range n)]
         #:when (not (= source target)))
      (graph-add-link graph source target))))

(: graph-complete? (-> Graph Boolean))
(define (graph-complete? graph)
  (let ([n (sub1 (graph-node-count graph))])
    (for/fold ([result : Boolean #t])
        ([node : Integer (graph-nodes graph)]
         #:break (not result))
      (= n (graph-node-degree graph node)))))

(module+ test
  ;; Note: As none of the data types or functions herein are user facing, the
  ;; tests are of implementation details and may be removed if broken.

  (require typed/rackunit)

  (check-true (graph-empty? empty-graph))
  (check-equal? (graph-node-count empty-graph) 0)
  (check-equal? (graph-link-count empty-graph) 0)
  (check-false (graph-has-node? empty-graph 0))
  (check-false (graph-has-link? empty-graph 0 1))
  (check-false (graph-has-undirected-link? empty-graph 0 1))

  (let ([graph (:-> empty-graph
                    (graph-add-node)
                    (graph-add-node)
                    (graph-add-link 0 1))])
    (check-false (graph-empty? graph))
    (check-equal? (graph-node-count graph) 2)
    (check-equal? (graph-link-count graph) 1)
    (check-true (graph-has-node? graph 0))
    (check-true (graph-has-node? graph 1))
    (check-false (graph-has-node? graph 2))
    (check-true (graph-has-link? graph 0 1))
    (check-false (graph-has-link? graph 1 0))
    (check-false (graph-has-undirected-link? graph 0 1))

    (let ([graph (graph-add-link graph 1 0)])
      (check-true (graph-has-link? graph 1 0))
      (check-true (graph-has-undirected-link? graph 0 1))
      (check-true (graph-has-undirected-link? graph 1 0))))

  (let ([graph (:-> empty-graph
                    (graph-add-node)
                    (graph-add-node)
                    (graph-add-node)
                    (graph-add-link 0 1)
                    (graph-add-link 1 2)
                    (graph-add-link 2 0))])
    (check-equal? (graph-node-count graph) 3)
    (check-equal? (graph-link-count graph) 3)
    (check-true (graph-has-node? graph 1))
    (check-true (graph-has-link? graph 0 1))
    (check-true (graph-has-link? graph 1 2))

    (let ([graph (graph-remove-node graph 1)])
      (check-equal? (graph-node-count graph) 2)
      (check-equal? (graph-link-count graph) 1)
      (check-false (graph-has-node? graph 1))
      (check-false (graph-has-link? graph 0 1))
      (check-false (graph-has-link? graph 1 0))))

  (let ([graph (:-> empty-graph
                    (graph-add-node)
                    (graph-add-node)
                    (graph-add-undirected-link 0 1))])
    (check-true (graph-has-undirected-link? graph 0 1)))

  (let ([graph (graph-complete-graph 4)])
    (check-true (graph-complete? graph))
    (check-true (graph-complete? (graph-remove-node graph 0)))
    (check-false (graph-complete? (graph-remove-link graph 0 1)))))

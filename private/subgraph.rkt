#lang typed/racket

(require "utility.rkt" "pipe.rkt" "node.rkt" "link-table.rkt" "basic.rkt")


(struct exn:fail:subgraph-node exn:fail () #:transparent)
(struct exn:fail:subgraph-link exn:fail () #:transparent)

(define-type Subgraph (Pairof Link-Table Graph))

(: subgraph-link-table (-> Subgraph Link-Table))
(define (subgraph-link-table subgraph)
  (car subgraph))

(: subgraph-parent (-> Subgraph Graph))
(define (subgraph-parent subgraph)
  (cdr subgraph))

(: subgraph-node-count (-> Subgraph Integer))
(define (subgraph-node-count subgraph)
  (link-table-node-count (subgraph-link-table subgraph)))

(: subgraph-link-count (-> Subgraph Integer))
(define (subgraph-link-count subgraph)
  (link-table-link-count (subgraph-link-table subgraph)))

(: subgraph-update-link-table (-> Subgraph Link-Table Subgraph))
(define (subgraph-update-link-table subgraph link-table)
  (cons link-table (subgraph-parent subgraph)))

(: empty-subgraph (-> Graph Subgraph))
(define (empty-subgraph graph)
  (cons (make-immutable-hash '()) graph))

(: subgraph-empty? (-> Subgraph Boolean))
(define (subgraph-empty? subgraph)
  (hash-empty? (subgraph-link-table subgraph)))

(: subgraph-has-node? (-> Subgraph Integer Boolean))
(define (subgraph-has-node? subgraph node)
  (hash-has-key? (subgraph-link-table subgraph) node))

(: subgraph-has-link? (-> Subgraph Integer Integer Boolean))
(define (subgraph-has-link? subgraph source target)
  (and (subgraph-has-node? subgraph target)
       (set-member? (hash-ref (subgraph-link-table subgraph) source)
                    target)))

(: subgraph-has-undirected-link? (-> Subgraph Integer Integer Boolean))
(define (subgraph-has-undirected-link? subgraph node1 node2)
  (and (subgraph-has-link? subgraph node1 node2)
       (subgraph-has-link? subgraph node2 node1)))

(: subgraph-node (-> Subgraph Integer Node))
(define (subgraph-node subgraph node)
  (hash-ref (subgraph-link-table subgraph) node))

(: subgraph-nodes (-> Subgraph (Listof Integer)))
(define (subgraph-nodes subgraph)
  (hash-keys (subgraph-link-table subgraph)))

(: subgraph-links (-> Subgraph (Listof (Pairof Integer Integer))))
(define (subgraph-links subgraph)
  (link-table-links (subgraph-link-table subgraph)))

(: subgraph-add-node (-> Subgraph Integer Subgraph))
(define (subgraph-add-node subgraph node)
  (if (graph-has-node? (subgraph-parent subgraph) node)
      (if (subgraph-has-node? subgraph node) subgraph
        (cons (hash-set (subgraph-link-table subgraph) node empty-node)
              (subgraph-parent subgraph)))
      (raise (exn:fail:subgraph-node
              (format "invalid subgraph node (~a)" node)
              (current-continuation-marks)))))

(: subgraph-add-link (-> Subgraph Node-Label Node-Label Subgraph))
(define (subgraph-add-link subgraph source target)
  (if (and (graph-has-link? (subgraph-parent subgraph) source target)
           (subgraph-has-node? subgraph target)
           (subgraph-has-node? subgraph source))
      (if (subgraph-has-link? subgraph source target) subgraph
        (subgraph-update-link-table subgraph
          (link-table-add-link  (subgraph-link-table subgraph) source target)))
      (raise (exn:fail:subgraph-link
              (format "invalid subgraph link (~a -> ~a)" source target)
              (current-continuation-marks)))))

(: subgraph-add-undirected-link (-> Subgraph Integer Integer Subgraph))
(define (subgraph-add-undirected-link subgraph node1 node2)
  (:-> subgraph
       (subgraph-add-link node1 node2)
       (subgraph-add-link node2 node1)))

(: subgraph-remove-link (-> Subgraph Integer Integer Subgraph))
(define (subgraph-remove-link subgraph source target)
  (if (subgraph-has-link? subgraph source target)
      (subgraph-update-link-table subgraph
        (hash-set (subgraph-link-table subgraph) source
                  (set-remove (subgraph-node subgraph source) target)))
    subgraph))

(: subgraph-remove-undirected-link (-> Subgraph Integer Integer Subgraph))
(define (subgraph-remove-undirected-link subgraph node1 node2)
  (if (subgraph-has-undirected-link? subgraph node1 node2)
      (:-> subgraph
           (subgraph-remove-link node1 node2)
           (subgraph-remove-link node2 node1))
    subgraph))

(: subgraph-remove-node (-> Subgraph Integer Subgraph))
(define (subgraph-remove-node subgraph node)
  (if (subgraph-has-node? subgraph node)
      (subgraph-update-link-table subgraph
       (link-table-remove-node
        (for/fold ([result : Link-Table (subgraph-link-table subgraph)])
            ([source (subgraph-nodes subgraph)])
          (link-table-remove-link result source node))
        node))
    subgraph))

(: subgraph-siblings? (-> Subgraph Subgraph Boolean))
(define (subgraph-siblings? subgraph1 subgraph2)
  (equal? (subgraph-parent subgraph1)
          (subgraph-parent subgraph2)))

(: subgraph-union (-> Subgraph Subgraph Subgraph))
(define (subgraph-union subgraph1 subgraph2)
  (let ([subgraph
         (for/fold ([subgraph : Subgraph subgraph1])
             ([node : Integer (subgraph-nodes subgraph2)])
           (subgraph-add-node subgraph node))])
    (for/fold ([subgraph subgraph])
        ([link (subgraph-links subgraph2)])
      (subgraph-add-link subgraph (car link) (cdr link)))))

(: subgraph-add-links (-> Subgraph Subgraph))
(define (subgraph-add-links subgraph)
  (let ([node-set : (Listof Integer) (subgraph-nodes subgraph)]
        [graph : Graph (subgraph-parent subgraph)])
    (for*/fold ([subgraph : Subgraph subgraph])
        ([source : Integer node-set]
         [target : Integer node-set])
      (if (graph-has-link? graph source target)
          (subgraph-add-link subgraph source target)
        subgraph))))

(: subgraph-remove-links (-> Subgraph Subgraph))
(define (subgraph-remove-links subgraph)
  (subgraph-update-link-table subgraph
      (for/fold ([link-table : Link-Table empty-link-table])
          ([label (subgraph-nodes subgraph)])
        (link-table-add-node link-table label))))

(: subgraph-grow (-> Subgraph Subgraph))
(define (subgraph-grow subgraph)
  (let ([graph  : Graph (subgraph-parent subgraph)]
        [labels : (Listof Node-Label) (subgraph-nodes subgraph)])
    (subgraph-add-links
     (for*/fold ([subgraph : Subgraph subgraph])
         ([source-label : Node-Label labels]
          [target-label : Node-Label (graph-node-neighbors graph source-label)])
       (subgraph-add-node subgraph target-label)))))

(: subgraph-full? (-> Subgraph Boolean))
(define (subgraph-full? subgraph)
  (let ([graph (subgraph-parent subgraph)])
    (and (= (subgraph-node-count subgraph) (graph-node-count graph))
         (= (subgraph-link-count subgraph) (graph-link-count graph)))))

(: subgraph->graph (-> Subgraph Graph))
(define (subgraph->graph subgraph)
  (make-graph (subgraph-link-table subgraph)
              (add1 (apply max (subgraph-nodes subgraph)))))

(module+ test
  (require typed/rackunit)
  (check-true (subgraph-empty? (empty-subgraph empty-graph)))

  (let ([subgraph (empty-subgraph (graph-complete-graph 4))])
    (check-true (subgraph-empty? subgraph))
    (check-equal? (subgraph-node-count subgraph) 0)
    (check-equal? (subgraph-link-count subgraph) 0))


  (let ([subgraph (:-> (graph-complete-graph 4)
                       (empty-subgraph)
                       (subgraph-add-node 0)
                       (subgraph-grow))])
    (check-true (subgraph-full? subgraph))))

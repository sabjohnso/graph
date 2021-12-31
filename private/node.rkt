#lang typed/racket

(provide
 Node-Label Node
 empty-node
 node-degree
 node-has-neighbor?
 node-neighbors
 node-add-neighbor
 node-remove-neighbor)

(define-type Node-Label Integer)
(define-type Node (Setof Node-Label))

(: empty-node Node)
(define empty-node (set))

(: node-degree (-> Node Integer))
(define (node-degree node)
  (set-count node))

(: node-has-neighbor? (-> Node Node-Label Boolean))
(define (node-has-neighbor? node label)
  (set-member? node label))

(: node-neighbors (-> Node (Listof Integer)))
(define (node-neighbors node)
  (set->list node))

(: node-add-neighbor (-> Node Node-Label Node))
(define (node-add-neighbor node label)
  (set-add node label))

(: node-remove-neighbor (-> Node Node-Label Node))
(define (node-remove-neighbor node label)
  (set-remove node label))

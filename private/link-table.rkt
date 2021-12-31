#lang typed/racket

(provide
 Link Link-Table
 make-link link-source link-target
 empty-link-table
 link-table-empty?
 link-table-node-count
 link-table-link-count
 link-table-has-node?
 link-table-has-link?
 link-table-has-undirected-link?
 link-table-node
 link-table-nodes
 link-table-add-node
 link-table-remove-node
 link-table-add-link
 link-table-add-undirected-link
 link-table-remove-link
 link-table-remove-undirected-link
 link-table-node-links
 link-table-links)

(require "pipe.rkt" "utility.rkt" "node.rkt")

(struct exn:fail:link-table-link exn:fail () #:transparent)

;;
;; ... Link
;;
(define-type Link (Pairof Node-Label Node-Label))

(: make-link (-> Integer Integer Link))
(define (make-link source target)
  (cons source target))

(: link-source (-> Link Node-Label))
(define (link-source link) (car link))

(: link-target (-> Link Node-Label))
(define (link-target link) (cdr link))


;;
;; ... Link-Table
;;
(define-type Link-Table (Immutable-HashTable Node-Label Node))

(: empty-link-table Link-Table)
(define empty-link-table (make-immutable-hash '()))

(: link-table-empty? (-> Link-Table Boolean))
(define (link-table-empty? link-table)
  (hash-empty? link-table))

(: link-table-node-count (-> Link-Table Integer))
(define (link-table-node-count link-table)
  (hash-count link-table))

(: link-table-link-count (-> Link-Table Integer))
(define (link-table-link-count link-table)
  (for/sum ([node (hash-values link-table)])
    (set-count node)))

(: link-table-has-node? (-> Link-Table Node-Label Boolean))
(define (link-table-has-node? link-table label)
  (hash-has-key? link-table label))

(: link-table-has-link? (-> Link-Table Node-Label Node-Label Boolean))
(define (link-table-has-link? link-table source-label target-label)
  (and (link-table-has-node? link-table source-label)
       (node-has-neighbor? (link-table-node link-table source-label)
                           target-label)))

(: link-table-has-undirected-link? (-> Link-Table Node-Label Node-Label Boolean))
(define (link-table-has-undirected-link? link-table label1 label2)
  (and (link-table-has-link? link-table label1 label2)
       (link-table-has-link? link-table label2 label1)))

(: link-table-node (-> Link-Table Node-Label Node))
(define (link-table-node link-table label)
  (hash-ref link-table label))

(: link-table-nodes (-> Link-Table (Listof Node-Label)))
(define (link-table-nodes link-table)
  (hash-keys link-table))

(: link-table-add-node (-> Link-Table Node-Label Link-Table))
(define (link-table-add-node link-table label)
  (if (link-table-has-node? link-table label) link-table
    (hash-set link-table label empty-node)))

(: link-table-add-undirected-link (-> Link-Table Node-Label Node-Label Link-Table))
(define (link-table-add-undirected-link link-table label1 label2)
  (:-> link-table
       (link-table-add-link label1 label2)
       (link-table-add-link label2 label1)))

(: link-table-add-link (-> Link-Table Node-Label Node-Label Link-Table))
(define (link-table-add-link link-table source-label target-label)
  (if (and (link-table-has-node? link-table source-label)
           (link-table-has-node? link-table target-label))
      (if (link-table-has-link? link-table source-label target-label) link-table
        (hash-set link-table source-label
                  (node-add-neighbor (link-table-node link-table source-label) target-label)))
    (raise (exn:fail:link-table-link
            (format "invalid link-table link (~a -> ~b)" source-label target-label)
            (current-continuation-marks)))))

(: link-table-remove-link (-> Link-Table Node-Label Node-Label Link-Table))
(define (link-table-remove-link link-table source-label target-label)
  (hash-set link-table source-label
            (set-remove (link-table-node link-table source-label) target-label)))

(: link-table-remove-undirected-link (-> Link-Table Node-Label Node-Label Link-Table))
(define (link-table-remove-undirected-link link-table label1 label2)
  (if (link-table-has-undirected-link? link-table label1 label2)
      (:-> link-table
           (link-table-remove-link label1 label2)
           (link-table-remove-link label2 label1))
    link-table))

(: link-table-remove-node (-> Link-Table Node-Label Link-Table))
(define (link-table-remove-node link-table target-label)
  (hash-remove
   (for/fold ([link-table : Link-Table link-table])
       ([source-label : Node-Label (link-table-nodes link-table)])
     (link-table-remove-link link-table source-label target-label))
               target-label))

(: link-table-node-links (-> Link-Table Integer (Listof Link)))
(define (link-table-node-links link-table source)
  (for/list ([target (link-table-node link-table source)])
    (make-link source target)))

(: link-table-links (-> Link-Table (Listof (Pairof Integer Integer))))
(define (link-table-links link-table)
  (: node-links (-> Integer (Listof Link)))
  (define (node-links node)
    (link-table-node-links link-table node))
  (list-flatmap node-links (link-table-nodes link-table)))

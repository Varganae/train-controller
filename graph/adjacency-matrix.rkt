#lang racket

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*         Labeled Graphs (Adjacency Matrix Representation)        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

;;; NOTE: this ADT does not optimize memory consumption for directed graphs!

(provide new labeled-graph? order directed? nr-of-edges
         for-each-node for-each-edge
         add-edge! delete-edge!
         adjacent?
         label label! edge-label)

(struct labeled-graph
  (d [n #:mutable] s)
  #:constructor-name make)

(define directed?   labeled-graph-d)
(define nr-of-edges labeled-graph-n)
(define (nr-of-edges! g v) (set-labeled-graph-n! g v))
(define storage     labeled-graph-s)

(define (new directed order)
  (make directed
        0
        (let ((rows (make-vector order)))
          (let fill-row ((i 1))
            (vector-set! rows (- i 1) (mcons 'no-label (make-vector order 'no-label)))
            (if (< i order)
                (fill-row (+ i 1))
                rows)))))

(define (order graph)
  (vector-length (storage graph)))

(define (for-each-node graph proc)
  (define rows (storage graph))
  (let iter-nodes ((i 0))
    (proc i (mcar (vector-ref rows i)))
    (when (< (+ i 1) (order graph))
      (iter-nodes (+ i 1))))
  graph)

(define (for-each-edge graph node proc)
  (define rows (storage graph))
  (let iter-edges ((to 0)
                   (label (vector-ref (mcdr (vector-ref rows node)) 0)))
    (unless (eq? label 'no-label)
      (proc to label))
    (when (< (+ to 1) (order graph))
      (iter-edges (+ to 1)
                  (vector-ref (mcdr (vector-ref rows node)) (+ to 1)))))
  graph)

(define (label! graph node lbl)
  (define rows (storage graph))
  (set-mcar! (vector-ref rows node) lbl)
  graph)

(define (label graph node)
  (mcar (vector-ref (storage graph) node)))

(define (add-edge! graph from to lbl)
  (define rows (storage graph))
  (when (if (directed? graph)
            (eq? (vector-ref (mcdr (vector-ref rows from)) to) 'no-label)
            (and (eq? (vector-ref (mcdr (vector-ref rows from)) to) 'no-label)
                 (eq? (vector-ref (mcdr (vector-ref rows to)) from) 'no-label)))
    (nr-of-edges! graph (+ (nr-of-edges graph) 1)))
  (vector-set! (mcdr (vector-ref rows from)) to lbl)
  (unless (directed? graph)
    (vector-set! (mcdr (vector-ref rows to)) from lbl))
  graph)

(define (delete-edge! graph from to)
  (define rows (storage graph))
  (unless (if (directed? graph)
              (eq? (vector-ref (mcdr (vector-ref rows from)) to) 'no-label)
              (and (eq? (vector-ref (mcdr (vector-ref rows from)) to) 'no-label)
                   (eq? (vector-ref (mcdr (vector-ref rows to)) from) 'no-label)))
    (nr-of-edges! graph (- (nr-of-edges graph) 1)))
  (vector-set! (mcdr (vector-ref rows from)) to 'no-label)
  (unless (directed? graph)
    (vector-set! (mcdr (vector-ref rows to)) from 'no-label))
  graph)

(define (adjacent? graph from to)
  (not (eq? (vector-ref (mcdr (vector-ref (storage graph) from)) to) 'no-label)))

(define (edge-label graph from to)
  (define val (vector-ref (mcdr (vector-ref (storage graph) from)) to))
  (if (eq? val 'no-label) #f val))

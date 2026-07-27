#lang racket

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Breadth First Traversal (version: labeled)          *-*-
;-*-*                                                                 *-*-
;-*-*                        Matthias Stevens                         *-*-
;-*-*                 2009-2018 Software Languages Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(require data/queue
         "config.rkt")

(provide bft node-nop edge-nop root-nop)

(define (bft graph
             root-discovered
             node-discovered
             edge-discovered
             edge-bumped
             . roots)
  (define visited (make-vector (order graph) #f))
  (define q (make-queue))
  (define exit '())
  (define (bft-component root root-label)
    (define (bft-iter from)
      (unless (node-discovered from (label graph from))
        (exit #t))
      (for-each-edge
       graph
       from
       (lambda (to edge-label)
         (if (vector-ref visited to)
             (unless (edge-bumped from to edge-label)
               (exit #f))
             (when (edge-discovered from to edge-label)
               (vector-set! visited to #t)
               (enqueue! q to)))))
      (unless (queue-empty? q)
        (bft-iter (dequeue! q))))
    (when (not (vector-ref visited root))
      (vector-set! visited root #t)
      (if (root-discovered root root-label)
          (bft-iter root)
          (exit #t))))
  (call-with-current-continuation
   (lambda (cont)
     (set! exit cont)
     (if (null? roots)
         (for-each-node graph bft-component)
         (for-each
          (lambda (root)
            (bft-component root (label graph root)))
          (car roots))))))

(define (root-nop root labl) #t)
(define (node-nop node labl) #t)
(define (edge-nop from to labl) #t)

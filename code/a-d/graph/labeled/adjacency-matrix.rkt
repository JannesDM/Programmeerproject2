#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*         Labeled Graphs (Adjacency Matrix Representation)        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*            Added changed version of for-each-edge               *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (labeled-graph)
 (export new labeled-graph? order directed? nr-of-edges
         for-each-node for-each-edge
         my-for-each-edge ; added
         add-edge! delete-edge!
         adjacent?
         label label! edge-label)
 (import (rnrs base)
         (srfi :9)
         (rnrs control)
         (rnrs mutable-pairs))
 
 (define-record-type labeled-graph
   (make d n s)
   labeled-graph?
   (d directed?)
   (n nr-of-edges nr-of-edges!)
   (s storage))
 
 (define (new directed order)
   (make directed
         0 ; nr-of-edges
         (let ((rows (make-vector order)))
           (let fill-row
             ((i 1))
             (vector-set! rows (- i 1) (cons 'no-label (make-vector order 'no-label)))
             (if (< i order)
                 (fill-row (+ i 1))
                 rows)))))
  
 (define (order graph)
   (vector-length (storage graph)))
 
 (define (for-each-node graph proc)
   (define rows (storage graph))
   (let iter-nodes
     ((i 0))
     (proc i (car (vector-ref rows i)))
     (if (< (+ i 1) (order graph))
         (iter-nodes (+ i 1))))
   graph)
 
 (define (for-each-edge graph node proc)
   (define rows (storage graph))
   (let iter-edges
     ((to 0)
      (label (vector-ref (cdr (vector-ref rows node)) 0)))
     (if (not (eq? label 'no-label))
         (proc to label))
     (if (< (+ to 1) (order graph))
         (iter-edges (+ to 1) (vector-ref (cdr (vector-ref rows node)) (+ to 1)))))
   graph)

 ; Edited to build a list and return it, 
 ;   the content is based on the given function
 (define (my-for-each-edge graph node proc)
   (define rows (storage graph))
   (define return-value '())
   (let iter-edges
     ((to 0)
      (label (vector-ref (cdr (vector-ref rows node)) 0)))
     (if (not (eq? label 'no-label))
         (set! return-value (append return-value (proc to label))))
     (if (< (+ to 1) (order graph))
         (iter-edges (+ to 1) (vector-ref (cdr (vector-ref rows node)) (+ to 1)))))
   return-value)
 
 (define (label! graph node label)
   (define rows (storage graph))
   (set-car! (vector-ref rows node) label)
   graph)
 
 (define (label graph node)
   (define rows (storage graph))
   (car (vector-ref rows node)))
 
 (define (add-edge! graph from to label)
   (define rows (storage graph))
   (vector-set! (cdr (vector-ref rows from)) to label)
   (when (not (directed? graph))
     (vector-set! (cdr (vector-ref rows to)) from label)
     (nr-of-edges! graph (+ 1 (nr-of-edges graph))))
   graph)
 
 (define (delete-edge! graph from to)
   (define rows (storage graph))
   (vector-set! (cdr (vector-ref rows from)) to 'no-label)
   (when (not (directed? graph))
     (vector-set! (cdr (vector-ref rows to)) from 'no-label)
     (nr-of-edges! graph (- (nr-of-edges graph) 1)))
   graph)
 
 (define (adjacent? graph from to)
   (define rows (storage graph))
   (define row (cdr (vector-ref rows from)))
   (not (eq? (vector-ref row to) 'no-label)))
 
 (define (edge-label graph from to)
   (define rows (storage graph))
   (if (eq? (vector-ref (cdr (vector-ref rows from)) to) 'no-label)
       #f
       (vector-ref (cdr (vector-ref rows from)) to))))
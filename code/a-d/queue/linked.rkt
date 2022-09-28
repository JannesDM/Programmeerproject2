#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Queues (Linked Implementation)                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (queue)
 (export new queue? serve! enqueue! peek full? empty?)
 (import (rnrs base)
         (srfi :9)
         (rnrs mutable-pairs))
 
 (define-record-type queue
   (make h r)
   queue?
   (h head head!)
   (r rear rear!))
 
 (define (new)
   (make () ()))
 
 (define (enqueue! q val)
   (define last (rear q))
   (define node (cons val '()))
   (if (null? (head q))
     (head! q node)
     (set-cdr! last node))
   (rear! q node)
   q)
 
 (define (peek q)
   (if (null? (head q))
     (error "empty queue (peek)" q)
     (car (head q))))
 
 (define (serve! q)
   (define first (head q))
   (if (null? first)
     (error "empty queue (serve!)" q))
   (head! q (cdr first))
   (if (null? (head q))
     (rear! q '()))
   (car first))
 
 (define (empty? q)
   (null? (head q)))
 
 (define (full? q)
   #f))
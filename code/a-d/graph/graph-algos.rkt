#lang racket


;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         Graph Algo's                            *-*-
;-*-*                                                                 *-*-
;-*-*              Based on:                                          *-*-
;-*-*                  Undirected BFS Applications                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

; export
(provide shortest-path
         get-edge-label
         hardware-graph
         state-connections)

; import
(require racket/mpair)
(require "../../helpers.rkt")
(require (prefix-in BFT: "traversal/bft-labeled.rkt"))
(require (prefix-in g: "labeled/adjacency-matrix.rkt"))

;; Returns 'shortest' path
;;    args from and to are indexes in the graph
; BFT
(define (shortest-path from to)
  (define g hardware-graph)
  (define paths (make-vector (g:order g) '()))
  (vector-set! paths from (g:label g from)) 
  (BFT:bft g 
       BFT:root-nop
       ; node-disc
       (lambda (node label)
         (not (equal? label (g:label g to))))
       ; edge-disc
       (lambda (from to edge-label)
         (define temp (vector-ref paths to))
         (vector-set! paths to (append temp (list (vector-ref paths from) (g:label g to)))))
       BFT:edge-nop
       (list from))
  ; return content on destination-index
  (vector-ref paths to))


; Get the label on an edge
;    edge from switch id to given track-id
(define (get-edge-label switch-id to-id)
  (g:edge-label hardware-graph
                (label->index switch-id)
                (label->index to-id)))


; Returns list of states and connected track-index
;    switch-index is an index of the graph
(define (state-connections switch-index)
  (define g hardware-graph)
  
  ; Function to execute for each edge
  ;   var direction indicates what part of the label to extract
  (define proc
    (lambda (to label)        
      ; abstractions
      (define direction (if (> to switch-index) 0 2))
      (define return-states '())
      (define label-state (string->number (string (string-ref (symbol->string label) direction))))
      
      ; append the right switch-label to the list
      (if (or (= label-state 0) (= label-state 1)
              (= label-state 2) (= label-state 3))
          (set! return-states (append return-states
                                      (mlist label-state (g:label g to))))
          (error "Incorrect label state (state-connection)" label-state))
      return-states))
  
  ; Execute function for each edge of the given switch
  (g:my-for-each-edge g switch-index proc))


; --- Hardcoded Graph -----------------------------------------------
; move to -> railway? route? ..?
(define hardware-graph
  (let ((g (g:new #f 36)))
    ; labels
    (g:label! g 0  '1-3)
    (g:label! g 1  'S-27)
    (g:label! g 2  'S-26)
    (g:label! g 3  '1-4)
    (g:label! g 4  '1-5)
    (g:label! g 5  'S-20)
    (g:label! g 6  '2-4)
    (g:label! g 7  'S-23)
    (g:label! g 8  'S-24)
    (g:label! g 9  'S-9)
    (g:label! g 10 '1-2)
    (g:label! g 11 'S-28)
    (g:label! g 12 '1-7)
    (g:label! g 13 '1-6)
    (g:label! g 14 'S-5)
    (g:label! g 15 'S-6)
    (g:label! g 16 '2-3)
    (g:label! g 17 'S-12)
    (g:label! g 18 'S-11)
    (g:label! g 19 'S-10)
    (g:label! g 20 '1-1)
    (g:label! g 21 'S-7)
    (g:label! g 22 'S-25)
    (g:label! g 23 '1-8)
    (g:label! g 24 '2-1)
    (g:label! g 25 'S-1)
    (g:label! g 26 'S-2-3)
    (g:label! g 27 '16-16) ; 'detect' 16
    (g:label! g 28 '2-2)
    (g:label! g 29 'S-8)
    (g:label! g 30 '2-5)
    (g:label! g 31 'S-4)
    (g:label! g 32 '2-6)
    (g:label! g 33 '2-7)
    (g:label! g 34 'S-16)
    (g:label! g 35 '2-8)
        
    ; Edges
    ; normal: (g:add-edge! g 0 1 '0)
    ;   index 0 and 1 = (label->index '1-3)  (label->index 'S-27)
    ;     improves readability but slower to make -> only at startup
    ;
    ; label: LowIndexValue - HighIndexValue (= bidirectional edges but 'directional' labels)
    ;       eg. 0-1: low->high: 0, high->low: 1
    ; 0, 1, 2 or 3: the state of the switch which connect to the other track
    ; detection blocks have state 0, switches 'entrance' is also 0
    (g:add-edge! g (label->index '1-3)  (label->index 'S-27) '0-1)
    (g:add-edge! g (label->index '1-3)  (label->index 'S-24) '0-2)
    (g:add-edge! g (label->index 'S-27) (label->index 'S-26) '0-2)
    (g:add-edge! g (label->index 'S-27) (label->index '1-2)  '2-0)
    (g:add-edge! g (label->index 'S-26) (label->index 'S-28) '1-2)
    (g:add-edge! g (label->index 'S-26) (label->index '1-4)  '0-0)
    (g:add-edge! g (label->index '1-4)  (label->index '1-5)  '0-0)
    (g:add-edge! g (label->index '1-5)  (label->index 'S-20) '0-1)
    (g:add-edge! g (label->index 'S-20) (label->index '2-4)  '0-0)
    (g:add-edge! g (label->index '2-4)  (label->index 'S-23) '0-0)
    (g:add-edge! g (label->index 'S-23) (label->index 'S-24) '1-0)
    (g:add-edge! g (label->index 'S-23) (label->index 'S-12) '2-1)
    (g:add-edge! g (label->index 'S-24) (label->index 'S-9)  '1-1)
    (g:add-edge! g (label->index 'S-9)  (label->index '1-2)  '0-0)
    (g:add-edge! g (label->index 'S-9)  (label->index 'S-11) '2-1)
    (g:add-edge! g (label->index 'S-28) (label->index '1-1)  '0-0)
    (g:add-edge! g (label->index 'S-28) (label->index '1-7)  '1-0)
    (g:add-edge! g (label->index '1-7)  (label->index '1-6)  '0-0)
    (g:add-edge! g (label->index '2-3)  (label->index 'S-12) '0-2)
    (g:add-edge! g (label->index 'S-12) (label->index 'S-11) '0-0)
    (g:add-edge! g (label->index 'S-11) (label->index 'S-10) '2-0)
    (g:add-edge! g (label->index 'S-10) (label->index '1-1)  '1-0)
    (g:add-edge! g (label->index 'S-7)  (label->index 'S-25) '1-1)
    (g:add-edge! g (label->index 'S-25) (label->index 'S-1)  '2-2)
    (g:add-edge! g (label->index 'S-25) (label->index '1-8)  '0-0)
    (g:add-edge! g (label->index '2-1)  (label->index 'S-1)  '0-1)
    (g:add-edge! g (label->index 'S-8)  (label->index 'S-4)  '2-0)
    (g:add-edge! g (label->index 'S-4)  (label->index '2-6)  '1-0) ;
    (g:add-edge! g (label->index 'S-4)  (label->index '2-7)  '2-0)
    (g:add-edge! g (label->index 'S-10) (label->index 'S-16) '2-1)
    (g:add-edge! g (label->index 'S-16) (label->index '2-8)  '2-0)
    (g:add-edge! g (label->index 'S-8)  (label->index '2-5)  '1-0)

    ; cross
    (g:add-edge! g (label->index '1-6)  (label->index 'S-5)  '0-1)
    (g:add-edge! g (label->index 'S-5)  (label->index 'S-6)  '0-0)
    (g:add-edge! g (label->index 'S-5)  (label->index 'S-7)  '2-0)
    (g:add-edge! g (label->index 'S-6)  (label->index '2-3)  '1-0)
    (g:add-edge! g (label->index 'S-20) (label->index 'S-6)  '2-2)
    
    ; 3-way
    (g:add-edge! g (label->index 'S-7)  (label->index 'S-2-3)  '2-1)
    (g:add-edge! g (label->index 'S-1)  (label->index 'S-2-3)  '0-0)
    (g:add-edge! g (label->index 'S-2-3)  (label->index 'S-8)  '3-0)
    (g:add-edge! g (label->index 'S-2-3)  (label->index '2-2)  '2-0)

    ; detection 16
    (g:add-edge! g (label->index '16-16)  (label->index 'S-16)  '0-0)
    
    ; return graph
    g))

;--------------------------------------------------------------------
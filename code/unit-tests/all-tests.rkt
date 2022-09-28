#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;; Unit Tests
;; --------------------------------------------

;https://docs.racket-lang.org/rackunit/api.html
(require rackunit
         racket/mpair
         "../nmbs/nmbs.rkt"
         "../infrabel.rkt"
         "../nmbs/gui.rkt"
         (prefix-in rw-i: "../railway/railway-infrabel.rkt")
         (prefix-in rw-n: "../railway/railway-nmbs.rkt")
         "../railway/train.rkt"
         "../railway/switch.rkt"
         "../railway/detection-block.rkt"
         (prefix-in h: "../helpers.rkt")
         (prefix-in rw: "../railway/railway.rkt")
         (prefix-in graph: "../a-d/graph/graph-algos.rkt")
         )

; Abstractions
(define (test-list-ids expected actual)
  (if (h:!= (length expected) (length actual))
      #f
      (let loop
        ([e-ids expected]
         [a-ids actual])
        (cond
          ((empty? e-ids) #t)
          ((not (eq? (h:head e-ids) (h:head a-ids))) #f)
          (else (loop (h:next-track e-ids) (h:next-track a-ids)))))))

(define simulation? #t)

; Begin tests
(test-begin
 ;; --- Basic Tests --------------------------------------
 ;; Train
 (define test-train (train '1 '1-1 '1-7))
 (check-eq?  (test-train 'id) '1)
 (check-true (number? (test-train 'speed)))
 (check-true (boolean? (test-train 'direction)))
 (check-eq?  (test-train 'type) 'train)
 (test-train 'speed! 100)
 (check-eq?  (test-train 'speed) 100)

 ;; Switch
 ; regular
 (define test-switch (switch 'test-1 '0 '1 '2))
 (check-eq?  (test-switch 'id) 'test-1)
 (check-true (number? (test-switch 'state)))
 (check-eq?  (test-switch 'type) 'switch)
 (test-switch 'state! 1)
 (check-eq?  (test-switch 'state) 1)
 (check-eq?  (test-switch 'track-id-0) '0)
 (check-eq?  (test-switch 'track-id-1) '1)
 (check-eq?  (test-switch 'track-id-2) '2)
 
 ;; Detection-block
 (define test-detection (detection '3))
 (check-eq?  (test-detection 'id) '3)
 (check-eq?  (test-detection 'type) 'detection-block)

 
 ;; --- Railway-Simulator Tests --------------------------------------
 ;; Railway
 ; setups raiwlay with 2 trains
 (rw:setup-railway! simulation?)

 
 ; track/train lists and tags
 (check-true (and (pair? (rw:get-trains))
                  (eq? (first (rw:get-trains)) 'trains)))
 (check-true (and (pair? (rw:get-switches))
                  (eq? (first (rw:get-switches)) 'switches)))
 (check-true (and (pair? (rw:get-detections))
                  (eq? (first (rw:get-detections)) 'detection-blocks)))
 
 ; train start positions
 (check-eq? (rw:get-prev-detection 1) (h:next-track h:START_LOCATIONS_1))
 (check-eq? (rw:get-prev-detection 2) (h:next-track h:START_LOCATIONS_2))

 ;; Switch
 (define switches (h:skip-tag (rw:get-switches)))
 
 ; loop over all switches for basic checks
 (map
  (lambda (switch)
    ; type
    (check-true (or (eq? (switch 'type) 'switch)
                    (eq? (switch 'type) 'three-way)))

    ; init state 1
    (check-eq? (switch 'state) 1)

    ; connected-tracks
    (check-false (or (eq? (switch 'track-id-1) 'init)
                     (eq? (switch 'track-id-2) 'init)))

    ; init reserved
    (check-false (switch 'reserved)))
  switches)

 ; check a few specific switches' connections
 ; 'S-1, 'S-2, '2-1, 'S-25
 (check-eq? 'S-2-3 ((rw:find-switch 'S-1) 'track-id-0))
 (check-eq? '2-1   ((rw:find-switch 'S-1) 'track-id-1))
 (check-eq? 'S-25  ((rw:find-switch 'S-1) 'track-id-2))

 ; 'S-25, '1-8, 'S-7, 'S-1
 (check-eq? '1-8  ((rw:find-switch 'S-25) 'track-id-0))
 (check-eq? 'S-7  ((rw:find-switch 'S-25) 'track-id-1))
 (check-eq? 'S-1  ((rw:find-switch 'S-25) 'track-id-2))
 
 ; 'S-11, 'S-12, 'S-9, 'S-10
 (check-eq? 'S-12 ((rw:find-switch 'S-11) 'track-id-0))
 (check-eq? 'S-9  ((rw:find-switch 'S-11) 'track-id-1))
 (check-eq? 'S-10  ((rw:find-switch 'S-11) 'track-id-2))

 ; S-16! 2do
 ; 'S-16, '16-16, 'S-10, '2-8
 (check-eq? '16-16 ((rw:find-switch 'S-16) 'track-id-0))
 (check-eq? 'S-10  ((rw:find-switch 'S-16) 'track-id-1))
 (check-eq? '2-8   ((rw:find-switch 'S-16) 'track-id-2))

 ; 3-way 'S-2-3, 'S-1, 'S-7, '2-2, 'S-8
 (check-eq? 'S-1 ((rw:find-switch 'S-2-3) 'track-id-0))
 (check-eq? 'S-7 ((rw:find-switch 'S-2-3) 'track-id-1))
 (check-eq? '2-2 ((rw:find-switch 'S-2-3) 'track-id-2))
 (check-eq? 'S-8 ((rw:find-switch 'S-2-3) 'track-id-3))

 ;; Detection
 (define detections (h:skip-tag (rw:get-detections)))
 
 ; loop over all detections for basic checks
 (map
  (lambda (detection)
    ; type
    (check-eq? (detection 'type) 'detection-block)

    ; init reserved
    (check-false (detection 'reserved)))
  detections)
 
 ;; Graph algos
 ; edge-label
 (check-eq? '0-0 (graph:get-edge-label 'S-2-3 'S-1))
 (check-eq? '2-1 (graph:get-edge-label 'S-2-3 'S-7))
 (check-eq? '2-0 (graph:get-edge-label 'S-2-3 '2-2))
 (check-eq? '3-0 (graph:get-edge-label 'S-2-3 'S-8))
 (check-eq? '1-0 (graph:get-edge-label 'S-10 '1-1))
 (check-eq? '2-0 (graph:get-edge-label 'S-10 'S-11))
 (check-eq? '2-1 (graph:get-edge-label 'S-10 'S-16))

 ; shortest path
 (check-true
  (test-list-ids
   '(2-7 S-4 S-8 S-2-3 S-7 S-5 1-6 1-7 S-28 S-26 S-27 1-3)
   (flatten (graph:shortest-path (h:label->index '2-7) (h:label->index '1-3)))))
 (check-true
  (test-list-ids
   '(1-8 S-25 S-7 S-5 1-6 1-7 S-28 1-1 S-10 S-16 2-8)
   (flatten (graph:shortest-path (h:label->index '1-8) (h:label->index '2-8)))))
 (check-true
  (test-list-ids
   '(2-1 S-1 S-25 S-7 S-5 1-6 1-7 S-28 S-26 S-27 1-2)
   (flatten (graph:shortest-path (h:label->index '2-1) (h:label->index '1-2)))))
 (check-true
  (test-list-ids
   '(2-6 S-4 S-8 S-2-3 S-7 S-5 1-6)
   (flatten (graph:shortest-path (h:label->index '2-6) (h:label->index '1-6)))))

 ; state-connections
 (check-true
  (test-list-ids
   '(1 S-7 0 S-1 2 2-2 3 S-8)
   (mlist->list (graph:state-connections (h:label->index 'S-2-3)))))
(check-true
  (test-list-ids
   '(1 1-6 0 S-6 2 S-7)
    (mlist->list (graph:state-connections (h:label->index 'S-5)))))
(check-true
  (test-list-ids
   '(2 S-20 0 S-5 1 2-3)
    (mlist->list (graph:state-connections (h:label->index 'S-6)))))
(check-true
  (test-list-ids
   '(2 S-26 1 1-7 0 1-1)
    (mlist->list (graph:state-connections (h:label->index 'S-28)))))
(check-true
  (test-list-ids
   '(0 2-4 1 S-24 2 S-12)
    (mlist->list (graph:state-connections (h:label->index 'S-23)))))

;; Traject and Reservations
(define test-train-2 (train '2 '1-3 'S-27))

; setting traject
(check-eq? (test-train-2 'traject) '())
(test-train-2 'traject! (list '1-1 '1-2 '1-3))
(check-equal? (test-train-2 'traject) (list '1-1 '1-2 '1-3))

; no-esrvations predicate
(check-true (h:no-reservations? '()))
(check-true (h:no-reservations? (list '1-1)))
(check-false (h:no-reservations? (list '1-1 '1-2)))

; setting reservations
(check-eq? (test-train-2 'reservations) '())
(check-true (h:no-reservations? (test-train-2 'reservations)))
(test-train-2 'reservations! (list '1-5 '1-6 '1-7))
(check-equal? (test-train-2 'reservations) (list '1-5 '1-6 '1-7))
(check-false (h:no-reservations? (test-train-2 'reservations)))


;; Test closing of application
; Exit prev railway and make new setup
(rw:exit-railway!)

; tcp port for communication
(define tcp-port 8800)
; width - height
(define ui-size (h:make-ui-size 500 620))

; define Infrabel en NMBS
(define INFRABEL (infrabel tcp-port))
(define NMBS (nmbs tcp-port ui-size))
 
; setup simulation, hardware
(INFRABEL 'setup-railway! simulation?)

; start gui
(NMBS 'start-ui)

; close gui
(NMBS 'close-ui)

; stop NMBS
(NMBS 'exit!)

; stop Infrabel
(INFRABEL 'exit!)

; exit simulation
(rw:exit-railway!)



; end
(h:debug "Done!"))
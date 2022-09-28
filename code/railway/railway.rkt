#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;; Railway ADT (= exec facade?)
;;
;; !: Not an 'object' with dispatcher class
;;     -> synchronization
;; --------------------------------------------


;; Imports
(require (prefix-in hardware: "simulator/interface.rkt"))
;require (prefix-in hardware: "??/interface.rkt")) ; hardware
; 2do: exec. fac

(require "../helpers.rkt")
(require "train.rkt")
(require (prefix-in detect: "detection-block.rkt"))
(require (prefix-in switch: "switch.rkt"))
(require (prefix-in graph: "../a-d/graph/graph-algos.rkt"))
(require racket/mpair)

;; Exports
(provide
 setup-railway!
 exit-railway!
 ; train
 get-trains
 find-train
 get-train-direction
 get-prev-detection
 get-train-speed
 get-train-traject
 change-train-speed!
 flip-direction!
 ; tracks
 get-detections
 get-switches
 find-switch
 find-detection
 get-switch-state
 set-switch-state!)


;; Global vars
; lists of all elements for NMBS and Infrabel
; the specific railway-... files filter information
(define trains     (cons 'trains '()))
(define detections (cons 'detection-blocks '()))
(define switches   (cons 'switches '()))

; id generator
(define id-gen (id))

;; Var getters
(define (get-trains) trains)
(define (get-detections) detections)
(define (get-switches) switches)


;; Setup the railway
(define (setup-railway! simulation?)
  ; 2do: change import based on incoming boolean
  
  ; setup train track
  (hardware:setup-hardware)

  ; start simulation
  (hardware:start)

  ; set lists
  (set-detections!)
  (set-switches!)
  
  ; add train(s)
  (add-train! (arg-1 START_LOCATIONS_1) (next-track START_LOCATIONS_1))
  (add-train! (arg-1 START_LOCATIONS_2) (next-track START_LOCATIONS_2)))

;; Reset railway
(define (exit-railway!)
  ; kill running threads
  (map (lambda (train)
         ; kill speed-thread
         (unless (empty? (train 'speed-controller))
           (kill-thread (train 'speed-controller)))
         ; removes all trains  from hardware!!
         (hardware:remove-loco))
       (skip-tag trains))
  ; kill local flipping-thread
  (unless (empty? flipping-thread)
    (kill-thread flipping-thread))
  
  ; reset lists and ids
  (set! trains     (cons 'trains '()))
  (set! detections (cons 'detect '()))
  (set! switches   (cons 'switches '()))
  (id-gen 'reset-ids!)

  ; stop hardware
  (hardware:stop))


;;; Tracks
;; Make detection tracks based on hardware
(define (set-detections!)
  ; loop over detection-ids
  (let loop
    ((det-ids (hardware:get-detection-block-ids)))

    ; create new detection track and add to list
    (set! detections
          (append detections
                  (list (detect:detection (head det-ids)))))

    ; only loop if not last
    (unless (null? (next-track det-ids))
      (loop (next-track det-ids)))))


;; Make switches tracks based on hardware
(define (set-switches!)
  ; get switch id's
  (define switch-ids (hardware:get-switch-ids))
  (define new-switch 'init)
  (define done? #f)
  
  ; loop over switch id's and deconstruct harware-graph to set connections
  (let loop
    ((switch-ids switch-ids))
    (cond
      ; stop condition is empty list
      ((null? switch-ids) (set! done? #t))

      ; Three-way
      ((three-way? (head switch-ids))
       ; set railway's switches
       (set! new-switch (switch:three-way 'S-2-3 'S-1 'S-7 '2-2 'S-8)) ; hardcoded
       ; set states in railway and hardware
       (new-switch 'state! 1)
       (set-switch-state! 'S-2-3 (cons (new-switch 'state-1) (new-switch 'state-2))))
       
      ; Regular switch
      (else
       (define curr-switch-id (head switch-ids))
       
       ; index in graph
       (define curr-switch-index (label->index curr-switch-id))
       ; 'x means dead-end, init value
       (define track-id-0 'x)
       (define track-id-1 'x)
       (define track-id-2 'x)
          
       ; returns a list with of switch-states and the connected track-id 
       (define state-connections
         (mlist->list (graph:state-connections curr-switch-index)))
           
       ; loop over state-connections
       (let inner-loop
         ((curr state-connections))
         
         ; check what state is currently in front of list
         (cond ((= (head curr) 0)
                (set! track-id-0 (next-element curr)))
               ((= (head curr) 1)
                (set! track-id-1 (next-element curr)))
               ((= (head curr) 2)
                (set! track-id-2 (next-element curr))))
         
         ; stop condition and loop
         (unless (empty? (next-state curr))
           (inner-loop (next-state curr))))

       ; create new switch
       (set! new-switch (switch:switch curr-switch-id track-id-0 track-id-1 track-id-2))
       
       ; set state
       (new-switch 'state! (hardware:get-switch-position (new-switch 'id)))))

    ; update list and loop with next switch
    (unless done?
      (set! switches (append switches (list new-switch)))
      (loop (next-track switch-ids)))))


; Abstract find for tracks
(define (find-track track-id list)
  ; loop over given list
  (let loop
    ((curr-list list))
    (cond ((eq? ((first curr-list) 'id) track-id)
           ; return track instance
           (first curr-list))
          ((null? (next-track curr-list))
           ; not found
           #f)
          (else (loop (next-track curr-list))))))

; Find a certain switch and return it, returns false if not found
(define (find-switch switch-id)
  (find-track switch-id (skip-tag (get-switches))))

; Find a certain detection-block and return it, returns false if not found
(define (find-detection detect-id)
  (find-track detect-id (skip-tag (get-detections))))


;; Returns the state of a specific switch
(define (get-switch-state switch-id)
  (let ((switch (find-switch switch-id)))
    (if switch
        (switch 'state)
        switch)))

;; Sets switch state, state is a cons cell if its a three-way switch
(define (set-switch-state! switch-id state)
  (if (three-way? switch-id)
      ; set sepperate states in hardware
      ; hardcoded 2do: break down string
      (begin
        ; cons cell: use car and cdr
        (set-switch-state! 'S-2 (car state))
        (set-switch-state! 'S-3 (cdr state)))
      ; else send to hardware
      (hardware:set-switch-position! switch-id state)))


;;; Train
;; Returns train based on given id
(define (find-train id)
  (let loop
    ((train-list (skip-tag trains)))
    (let ((curr-train (head train-list))
          (next-train (next-train train-list)))
      (cond ((eq? (curr-train 'id) id) curr-train) ; found train
            ((null? next-train) #f) ; not found
            (else (loop next-train))))))

;; Makes train object
(define (add-train! prev-track curr-track)
  ; add new train to Railway (for NMBS and Infrabel)
  (define new-train (train (id-gen 'new-id) prev-track curr-track))  
  (set! trains (append trains (list new-train))) 
  ; add train to sim, hardware
  (hardware:add-loco (new-train 'id) prev-track curr-track))

;; Get last detection-track id that a train with given id was on then return it 
;     if hardware returns #f it returns the train's last detection field
(define (get-prev-detection train-id)
  (let
      ((det-id (hardware:get-loco-detection-block train-id))
       (train (find-train train-id)))
    (if det-id
        det-id
        (train 'prev-detect))))

;; Gets direction of train based on given id
(define (get-train-direction train-id)
  ((find-train train-id) 'direction))

;; Gets speed of train based on given id
(define (get-train-speed train-id)
  ((find-train train-id) 'speed))

;; Gets traject of train based on given id
(define (get-train-traject train-id)
  ((find-train train-id) 'traject))


;;; Dynamic speeds
; 2do: parts only if simulation

; Set a trains speed in hardware
(define (hardware-train-speed! train speed)
  ;; argument speed is always stricly positive
  (train 'speed! speed)
  
  ; the direction-boolean determens if speed is positive or
  ;    negative in hardware, in the train adt it's always positive
  (unless (train 'direction)
    (set! speed (- speed)))
  (hardware:set-loco-speed! (train 'id) speed))

;; Calculates new speed based on current speed
(define (smooth-speed goal-speed train)
  ; calculate value to add
  (define (to-add X)
    (if (<= X 50) 15
        (round (/ X 4))))
  
  ; loop, the changed value is the speed of the current train
  (define (smooth-speed-loop compare)
    (let
        ((curr-speed (train 'speed)))
      ; stop if goal-speed is transcended
      (when (compare curr-speed goal-speed)
        ; calculate new speed
        (define new-speed 
          (if (compare 1 2)
              ; speed up
              (+ curr-speed (to-add curr-speed))
              ; slow down
              ;  to-add is the same amount when speeding up but on inverse axis
              (- curr-speed (to-add (- MAX_TRAIN_SPEED curr-speed)))))
          
        ; adjust speed
        (hardware-train-speed!
         train
         ; boundary check
         (if (compare new-speed goal-speed)
             new-speed
             goal-speed))
        
        ; wait and restart
        (sleep sleep-time-speed)
        (smooth-speed-loop compare))))
    
  ; start loop with right comparator
  (if (> goal-speed (train 'speed))
      ; speed up
      (smooth-speed-loop <)
      ; slow down
      (smooth-speed-loop >))
    
  ; ensure goal speed is correct
  (hardware-train-speed! train goal-speed))

; A 'lock' for changing train's speed,
;   currently only used when flipping a train
(define flipping? #f)

;; Sets speed of train based on given id
(define (change-train-speed! train-id new-speed)
  ; find train from id
  (define train (find-train train-id))

  ; if not locked
  (unless flipping?
    ; stop previous running thread
    (unless (empty? (train 'speed-controller))
      (kill-thread (train 'speed-controller)))
  
    ; create new thread
    (train 'speed-controller!
           (thread (lambda () (smooth-speed new-speed train))))))

; global so can be killed
(define flipping-thread '())

;; Flips direction of the train with given id
(define (flip-direction! train-id)
  (let*
      ((train (find-train train-id))
       (curr-speed (train 'speed)))

    ; stop train
    (change-train-speed! train-id MIN_TRAIN_SPEED)

    ; lock speed controls
    (set! flipping? #t)

    ; smooth flipping
    (set! flipping-thread (thread (lambda () (flip-train! train-id curr-speed))))))

; Wait for the train to stand still to change direction 
(define (flip-train! train-id prev-speed)
  ; find train from id
  (define train (find-train train-id))
  (define (loop)
    ; as long as train doesnt reach zero
    (when (> (train 'speed) MIN_TRAIN_SPEED)
      ; wait and loop
      (sleep sleep-time-thread)
      (loop)))
  ; start loop
  (loop)    

  ; train is standing still
  ; 'flips' boolean
  (train 'direction! (not (train 'direction)))

  ; unlock speed controls
  (set! flipping? #f)
    
  ; restart train
  (change-train-speed! train-id prev-speed))

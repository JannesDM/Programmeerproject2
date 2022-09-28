#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;; Railway Infrabel ADT
;; --------------------------------------------


;; Imports and export
(require (prefix-in rw: "railway.rkt"))
(require "../helpers.rkt")
(provide railway-infrabel)

;; Railway-infrabel ADT
(define (railway-infrabel)
  
  ; Set the state of the switch from given id
  (define (set-switch-state! switch-id state)    
    ; get switch
    (define switch (rw:find-switch switch-id))

    ; change state in railway
    (switch 'state! state)

    ; 3-way, state for Railway is a cons cell
    (when (eq? (switch 'type) 'three-way)
      (set! state (cons (switch 'state-1) (switch 'state-2))))

    ; change state in hardware
    (rw:set-switch-state! switch-id state))
    
  ;; Reservations
  ; Abstraction for mapping reservations-list
  (define (map-tracks-reserved curr-track train-id tracks)
    (map
     (lambda (track)
       ; when reserved by current train except given track id 
       (when (and
              (track 'reserved)
              (eq? (track 'reserved) train-id)
              (not (eq? (track 'id) curr-track)))
         ; unreserve
         (track 'reserve! #f)))
     tracks))
  
  ; Unreserve all tracks reserved by train-id except given track id
  (define (unreserve-all! curr-track train-id)
    (map-tracks-reserved curr-track train-id (skip-tag (rw:get-switches)))
    (map-tracks-reserved curr-track train-id (skip-tag (rw:get-detections))))
 
  ; Conditional for reservation, true if reserved by other train
  (define (res?-conditional train-id track-reserved)
    (and track-reserved
         (not (eq? train-id track-reserved))))

  ; is given track is reserved or not
  (define (reserved? track-id train-id)
    (cond ((switch? track-id)
           (res?-conditional train-id ((rw:find-switch track-id) 'reserved)))
          ((detection? track-id)
           (res?-conditional train-id ((rw:find-detection track-id) 'reserved)))))
  ; set the reservation of a given track id
  (define (set-reserve! track-id value)
    (cond ((switch? track-id)
           ((rw:find-switch track-id) 'reserve! value))
          ((detection? track-id)
           ((rw:find-detection track-id) 'reserve! value))))

  ; Dispatcher
  (define (dispatch-railway-infrabel msg . args)
    (cond
      ; setup railway
      ((eq? msg 'setup-railway!)   (rw:setup-railway!      (arg-1 args)))
      ; exit simulation
      ((eq? msg 'exit-railway!)    (rw:exit-railway!))
      ; get list of trains, skips 'train tag
      ((eq? msg 'get-trains)       (skip-tag (rw:get-trains)))
      ; returns train based on given id
      ((eq? msg 'find-train)       (rw:find-train          (arg-1 args)))
      ; get last detection-block id that train with given was/is on
      ((eq? msg 'prev-detection)   (rw:get-prev-detection  (arg-1 args)))
      ; get switch with given id
      ((eq? msg 'get-switch)       (rw:find-switch         (arg-1 args)))
      ; returns the state of a specific switch (cons to switch id from prev func?)
      ((eq? msg 'switch-state)     (rw:get-switch-state    (arg-1 args)))
      ; get train's speed
      ((eq? msg 'train-speed)      (rw:get-train-speed     (arg-1 args)))
      ; flips train direction
      ((eq? msg 'flip-dir!)        (rw:flip-direction!     (arg-1 args)))
      ; set train speed
      ((eq? msg 'train-speed!)     (rw:change-train-speed! (arg-1 args) (arg-2 args)))
      ; not directly passing commando to railway
      ((eq? msg 'reserved?)        (reserved?              (arg-1 args) (arg-2 args)))
      ((eq? msg 'switch-state!)    (set-switch-state!      (arg-1 args) (arg-2 args)))
      ((eq? msg 'set-reserve!)     (set-reserve!           (arg-1 args) (arg-2 args)))
      ((eq? msg 'unreserve-all!)   (unreserve-all!         (arg-1 args) (arg-2 args)))
      (else (error "Dispatcher railway-infrabel." msg))))
  dispatch-railway-infrabel)

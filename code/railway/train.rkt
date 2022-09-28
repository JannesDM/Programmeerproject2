#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;; Train ADT
;;
;; 2do: change direction name to 'forward?'
;; --------------------------------------------

; Import and export
(require "../helpers.rkt")
(provide train)

; Train ADT
(define (train id prev-track curr-track)
  (let
      ; defaults
      ((id id)
       (speed 0)
       ; #t = forward, #f = backwards
       (direction #t)
       ; prev detection block id
       (prev-detection curr-track)
       ; lsit with current traject, track ids
       (traject '())
       ; list for reserved track ids
       (reservations '())
       ; waiting for reservation to clear
       (waiting? #f)
       ; thread for managing speeds
       (speed-controller '()))
    
    ; Dispatcher train
    (define (dispatch-train msg . args)
      (cond
        ; Getters
        ((eq? msg 'speed)        speed)
        ((eq? msg 'id)           id)
        ((eq? msg 'direction)    direction)
        ((eq? msg 'prev-detect)  prev-detection)
        ((eq? msg 'prev-track)   prev-track)
        ((eq? msg 'traject)      traject)
        ((eq? msg 'reservations) reservations)
        ((eq? msg 'waiting?)     waiting?)
        ((eq? msg 'speed-controller) speed-controller)
        ((eq? msg 'type)         'train)
        ; Setters
        ((eq? msg 'speed!)        (set! speed          (arg-1 args)))
        ((eq? msg 'direction!)    (set! direction      (arg-1 args)))
        ((eq? msg 'prev-detect!)  (set! prev-detection (arg-1 args)))
        ((eq? msg 'prev-track!)   (set! prev-track     (arg-1 args)))
        ((eq? msg 'traject!)      (set! traject        (arg-1 args)))
        ((eq? msg 'reservations!) (set! reservations   (arg-1 args)))
        ((eq? msg 'waiting!)     (set! waiting?       (arg-1 args)))
        ((eq? msg 'speed-controller!) (set! speed-controller   (arg-1 args)))
        (else (error "Dispatcher train." msg))))
    dispatch-train))
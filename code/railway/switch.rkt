#lang racket

;; --------------------------------------------------------
;; Jannes De Mets
;; ---------------
;; Switch ADT
;;
;; --------------------------------------------------------

;; Import and exports
(require "../helpers.rkt")
(provide switch three-way)

;; Regular-switch ADT
(define (switch id track-id-0 track-id-1 track-id-2)
  (let
      ; defaults
      ((state 0)
       (reserved #f))

    ; dispatcher
    (define (dispatch-switch msg . args)
      (cond
        ; getters
        ((eq? msg 'id)          id)
        ((eq? msg 'state)       state)
        ((eq? msg 'track-id-0) track-id-0)
        ((eq? msg 'track-id-1) track-id-1)
        ((eq? msg 'track-id-2) track-id-2)
        ((eq? msg 'reserved)   reserved)
        ; setters
        ((eq? msg 'state!)       (set! state (arg-1 args)))
        ((eq? msg 'track-id-0!)  (set! track-id-0 (arg-1 args)))
        ((eq? msg 'track-id-1!)  (set! track-id-1 (arg-1 args)))
        ((eq? msg 'track-id-2!)  (set! track-id-2 (arg-1 args)))
        ((eq? msg 'reserve!)     (set! reserved   (arg-1 args)))
        ; others
        ((eq? msg 'type)   'switch)
        (else (error "Dispatcher switch." msg))))
    dispatch-switch))


;; Three-way-switch ADT
;  S-2 & S-3 are hardcoded
(define (three-way id track-id-1-0 track-id-1-1 track-id-2-1 track-id-2-2)
  (let*
      ; defaults, 2 switches in a row
      ((second-switch (switch 'S-3 'temp track-id-2-1 track-id-2-2))
       (first-switch  (switch 'S-2 track-id-1-0 track-id-1-1 (second-switch 'id))) ;
       (state -1)
       (reserved #f))

    ; connect internal switches
    (second-switch 'track-id-0! (first-switch 'id))

    ; setters
    (define (set-state! new-state)
      ; only if it needs to be updated
      (when (!= new-state state)
        ; set this state
        (set! state new-state)
      
        ; abstraction
        (define first-switch-state #f)
        (define second-switch-state #f)

        ; set the values based of incoming state
        ; derail danger! always change first then second switch state.
        (cond ((eq? new-state 1)
               (set! first-switch-state 1)
               (set! second-switch-state 2))
              ((eq? new-state 2)
               (set! first-switch-state 2)
               (set! second-switch-state 1))
              ((eq? new-state 3)
               (set! first-switch-state 2)
               (set! second-switch-state 2)))
      
        ; set both switches
        (first-switch 'state! first-switch-state)
        (second-switch 'state! second-switch-state)))

    ; dispatcher
    (define (dispatch-three-way msg . args)
      (cond
        ; getters
        ((eq? msg 'state)      state)
        ((eq? msg 'id)         id)
        ((eq? msg 'type)       'three-way)
        ((eq? msg 'state-1)    (first-switch 'state))
        ((eq? msg 'state-2)    (second-switch 'state))
        ((eq? msg 'track-id-0) track-id-1-0)
        ((eq? msg 'track-id-1) track-id-1-1)
        ((eq? msg 'track-id-2) track-id-2-1)
        ((eq? msg 'track-id-3) track-id-2-2)
        ((eq? msg 'reserved)   reserved)
        ; setters
        ((eq? msg 'state!)     (set-state!    (arg-1 args)))
        ((eq? msg 'reserve!)   (set! reserved (arg-1 args)))
        (else (error "Dispatcher three-way." msg args))))
    dispatch-three-way))

#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;; Detection block ADT
;; --------------------------------------------

; Import and export
(provide detection)
(require "../helpers.rkt")

; Detection ADT
(define (detection id)
  (let
      ; defaults
      ((reserved #f))

    ; dispatcher
    (define (dispatch-detection msg . args)
      (cond
        ; getters
        ((eq? msg 'id)           id)
        ((eq? msg 'reserved)     reserved)
        ((eq? msg 'type)        'detection-block)
        ; setters
        ((eq? msg 'reserve!)    (set! reserved (arg-1 args)))
        (else (error "Dispatcher detection track." msg))))
    dispatch-detection))
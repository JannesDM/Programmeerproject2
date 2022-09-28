#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;; Helper functions and abstractions
;; --------------------------------------------

;; Exports everything
(provide (all-defined-out))

;; Abstractions

; ui sizing
(define make-ui-size cons)
(define ui-width car)
(define ui-height cdr)

; list operations
(define arg-1 car)
(define arg-2 cadr)
(define arg-3 caddr)
(define arg-4 cadddr)
(define head car)
(define next-track cdr)
(define next-train cdr)
(define skip-tag cdr)
(define next-element cadr)
(define next-state cddr)
(define skip-2-elements cddr)

; tcp
(define make-tcp-msg list)
(define input-name car)
(define input-args cdr)

; train
(define MIN_TRAIN_SPEED 0)
(define ENDING_TRAIN_SPEED 70)
(define SLOW_TRAIN_SPEED 120)
(define DEFAULT_TRAIN_SPEED 200)
(define FAST_TRAIN_SPEED 300)
(define MAX_TRAIN_SPEED 400)
(define START_LOCATIONS_1 (cons 'S-12 '2-3))
(define START_LOCATIONS_2 (cons 'S-23 '2-4))
(define empty-traject '())

; returns if list has no reservations
(define (no-reservations? reservations)
  (if (list? reservations)
      ; only current track reserved
      (>= 1 (length reservations))
      #t))

; timings for threads and ?!
(define sleep-time-thread 0.3)
(define tcp-sleep-time 0.2)
(define goal-sleep-time 0.1)
(define sleep-time-speed 0.4)
(define train-wait-time 3)
(define timing-debounce-slider 500)
(define slider-buffer-time 0.55)

; '!='-operation
(define (!= n1 n2)
  (if (or (void? n1) (void? n2))
      #f
      (not (= n1 n2))))

;; Copied from:
;    https://stackoverflow.com/questions/5397144/how-do-i-compare-the-symbol-a-with-the-character-a
;; converting a character to a symbol:
(define (char->symbol ch)
  (string->symbol (string ch)))


;; Unique id generator
(define (id)
  ; keeps track of ids
  (let ((curr 0))
    ; makes new id
    (define (get-id)
      (set! curr (+ curr 1))
      curr)
    ; reset id generator
    (define (reset-ids!)
      (set! curr 0)
      curr)
    ; dispatcher
    (define (dispatch-id msg . args)
      (cond 
        ((eq? msg 'new-id)     (get-id))
        ((eq? msg 'reset-ids!) (reset-ids!))
        (else (error "dispatch id-generator" msg))))
    dispatch-id))


;; Debugging tool
(define (debug . args)
  (if (null? args)
      (display "Empty debugging arg")
      (begin
        (let loop
          ((args args))
          (display (first args))
          (if (not (null? (cdr args)))
              (begin
                (display  "  -  ")
                (loop (cdr args)))
              (newline)))))
  (newline))

;;; Tracks
;; Threshold of the amount of switches for slowing down speed
(define switch-threshold 2)

;; Returns if the given id is a switch
; 'type = 'switch ??
(define (switch? id)
  (eq? 'S (char->symbol (string-ref (symbol->string id) 0))))

;; Returns if the given id is a 3-way
; 'type = '3-way ??
(define (three-way? id)
  (> (string-length (symbol->string id)) 4))

;; Returns if the given id is a detection-block
; 'type = '3-way ??
(define (detection? id)
  (not (switch? id)))

;; Returns if a track id is a long track
(define (long-track? track-id)
  (and (not (eq? track-id '1-8))
       (eq? #\1 (string-ref (symbol->string track-id) 0))))
;; All other tracks are short
(define (short-track? track-id)
  (and (not (long-track? track-id))
       (not (switch? track-id))
       (not (eq? track-id '2-3))
       (not (eq? track-id '2-4))))

;; Hardcoded translation of labels to node indexes in graph
(define (label->index label)
  ; 2do: keep symbol
  (define sLabel (if (symbol? label) (symbol->string label) label))

  ; splitsen op eerste karakter?
  (case sLabel
    ; detection-blocks
    ; 1 - x  ;(if (eq? (string-ref sLabel 0) 1))
    [("1-1")  20]
    [("1-2")  10]
    [("1-3")   0]
    [("1-4")   3]
    [("1-5")   4]
    [("1-6")  13]
    [("1-7")  12]
    [("1-8")  23]
    ; 2 - x
    [("2-1")  24]
    [("2-2")  28]
    [("2-3")  16]
    [("2-4")   6]
    [("2-5")  30]
    [("2-6")  32]
    [("2-7")  33]
    [("2-8")  35]
    ; S - x
    [("S-1")  25]
    [("S-2-3")  26] ; 3-way
    [("16-16")  27] ; 'detection-block S-16'
    [("S-4")  31]
    [("S-5")  14]
    [("S-6")  15]
    [("S-7")  21]
    [("S-8")  29]
    [("S-9")   9]
    [("S-10") 19]
    [("S-11") 18]
    [("S-12") 17]
    [("S-16") 34]
    [("S-20")  5]
    [("S-23")  7]
    [("S-24")  8]
    [("S-25") 22]
    [("S-26")  2]
    [("S-27")  1]
    [("S-28") 11]
    [else (error "Label not found in label->index" label)]))
#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;
;       scenario.rkt
;
;       Example scenario.
;

(require "interface.rkt")

(define (set-speed-at train detection-block speed)
  (if (eq? (get-loco-detection-block train) detection-block)
      (set-loco-speed! train speed)
      (begin (sleep 1)
             (set-speed-at train detection-block speed))))

(define (back-and-forth)
  (define train 'TEST-ROUTE-TRAIN)
  (add-loco train 'D6 'D7)
  (set-loco-speed! train 200)

  (define (loop)
    (set-speed-at train 'D1 (- (get-loco-speed train)))
    (set-speed-at train 'D3 (- (get-loco-speed train)))
    (loop))
  (loop))

; Load the hardware track
(setup-loop)

; Start the simulator
(start)

(thread back-and-forth)
  

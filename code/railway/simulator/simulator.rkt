#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;
;       simulator.rkt
;
;       Main simulator loop. Runs in a separate thread.
;

(require racket/gui/base racket/class)

(require "railway.rkt" "graphics.rkt" "trains.rkt")

(provide initialize-simulator launch-simulator-loop request-redraw-segments stop-simulator-loop)

(define WINDOW '())
(define REDRAW-SEGMENTS? #f)
(define CONTINUE? #t)
(define SCREEN-WIDTH 3000)
(define SCREEN-HEIGHT 1800)
(define FIRST-TIME? #t)

(define (draw-all-segments)
  (send RAILWAY draw-all WINDOW 'segments))

(define (draw-all-trains)
  (for-each
   (lambda (train)
     (send train draw WINDOW 'trains))
   TRAINS))

(define (request-redraw-segments)
  (set! REDRAW-SEGMENTS? #t))

(define (initialize-simulator)
  (when FIRST-TIME?
    (set! WINDOW (make-object window% "GUI Simulator" SCREEN-WIDTH SCREEN-HEIGHT))
    (sleep/yield 1)
    (send WINDOW create-buffer-bitmap 'segments)
    (send WINDOW create-buffer-bitmap 'trains)
    (set! FIRST-TIME? #f)))

(define (end-simulator)
  (send WINDOW clear 'segments)
  (send WINDOW clear 'trains)
  (remove-all-trains)
  (remove-all-tracks)
  (send WINDOW refresh-all))

(define (launch-simulator-loop)
  (let* ((wait-per-frame 50) ;; Lower this value to get a higher framerate
         (previous-time (current-inexact-milliseconds))
         (delta-time 0)
         (game-loop-timer '()))
    (when (not RAILWAY)
      (error "Please set up a railway before launching the simulator"))
    (send RAILWAY build SCREEN-WIDTH SCREEN-HEIGHT)
    (draw-all-segments)
    (set! CONTINUE? #t)
    (define (simulator-loop)
      (set! delta-time (- (current-inexact-milliseconds) previous-time))
      (set! previous-time (current-inexact-milliseconds))
      (send WINDOW clear 'trains)
      (when REDRAW-SEGMENTS?
        (set! REDRAW-SEGMENTS? #f)
        (send WINDOW clear 'segments)
        (draw-all-segments))
      (for-each
       (lambda (train)
         (send train move (/ delta-time 1000)))
       TRAINS)      
      (draw-all-trains)
      (send WINDOW refresh-all)
      (send WINDOW set-frame-title (string-append "FPS: " (number->string (round (/ 1000 delta-time)))))
      (if CONTINUE?
          (send game-loop-timer start wait-per-frame #t)
          (end-simulator)))
    
    (set! game-loop-timer
          (new timer% [notify-callback simulator-loop]))
    (send game-loop-timer start wait-per-frame #t)))

(define (stop-simulator-loop)
  (set! CONTINUE? #f))

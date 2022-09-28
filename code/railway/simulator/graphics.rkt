#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;
;       graphics.rkt
;
;       create window with overlayed canvas
;

(require racket/gui/base)
(require racket/class)

(provide window%)

(define window%
  (class object%
    (super-new)
    (init-field title width height)
        
    (define (scale value)
      (/ value 3))
    (set! width (round (scale width)))
    (set! height (round (scale height)))
    (define frame (new frame%
                       [label title]
                       [width width]
                       [height height]))
    ; Make the drawing area
    (define canvas (new canvas% [parent frame]))
    ; Get the canvas's drawing context
    (define dc (send canvas get-dc))

    
    ;brushes
    (define brushes '())
    (define (add-brush id brush)
      (set! brushes (cons (cons id brush) brushes)))
    (define (get-brush id)
      (cdr (assoc id brushes)))
    ;pens
    (define pens '())
    (define (add-pen id pen)
      (set! pens (cons (cons id pen) pens)))
    (define (get-pen id)
      (cdr (assoc id pens)))
    
    ; Make some pens and brushes
    (add-pen 'red (make-object pen% "RED" 5 'solid))
    (add-pen 'yellow (make-object pen% "YELLOW" 5 'solid))
    (add-pen 'yellow-fat (make-object pen% "YELLOW" 15 'solid))
    (add-pen 'orange-fat (make-object pen% "ORANGE" 15 'solid))
    (add-pen 'green (make-object pen% "GREEN" 5 'solid))
    (add-pen 'black (make-object pen% "BLACK" 5 'solid))
    (add-pen 'blue (make-object pen% "BLUE" 5 'solid))
    (add-pen 'purple (make-object pen% "PURPLE" 5 'solid))
    
    (add-brush 'transparent (make-object brush% "BLACK" 'transparent))
    
    (define bitmaps '())
    (define bitmap-dcs '())
    
    (define/public (create-buffer-bitmap id)
      (let* ((buffer-bitmap (make-object bitmap% width height #f #t))
             (buffer-bitmap-dc (make-object bitmap-dc% buffer-bitmap)))
        (send buffer-bitmap-dc set-brush (get-brush 'transparent))
        (set! bitmaps (cons (cons id buffer-bitmap) bitmaps))
        (set! bitmap-dcs (cons (cons id buffer-bitmap-dc) bitmap-dcs))))
    
    (define (get-bitmap-dc id)
      (cdr (assoc id bitmap-dcs)))
    
    (define/public (draw-arc dc-id pen-id x y width height start-radians end-radians)
      (let ((dc (get-bitmap-dc dc-id)))
        (send dc set-pen (get-pen pen-id))
        (send dc draw-arc (scale x) (scale y) (scale width) (scale height) start-radians end-radians)))
    
    (define/public (draw-line dc-id pen-id x y x2 y2)
      (let ((dc (get-bitmap-dc dc-id)))
        (send dc set-pen (get-pen pen-id))
        (send dc draw-line (scale x) (scale y) (scale x2) (scale y2))))
    
    (define/public (draw-rectangle dc-id pen-id x y width height)
      (let ((dc (get-bitmap-dc dc-id)))
        (send dc set-pen (get-pen pen-id))
        (send dc draw-rectangle (scale x) (scale y) (scale width) (scale height))))
    
    (define/public (clear dc-id)
      (send (get-bitmap-dc dc-id) clear)
      (send (get-bitmap-dc dc-id) erase))

    (define/public (set-frame-title title)
      (send frame set-label title))

    (define/public (refresh id)
      (send dc draw-bitmap (cdr (assoc id bitmaps)) 0 0))
    
    (define/public (refresh-all)
      (send dc clear)
      (for-each
       (lambda (buffer-bitmap)
         (send dc draw-bitmap (cdr buffer-bitmap) 0 0))
       (reverse bitmaps)))
    
    ; Show the frame
    (send frame show #t)))
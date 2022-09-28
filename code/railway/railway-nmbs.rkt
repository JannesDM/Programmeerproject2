#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;; Railway - NMBS
;; --------------------------------------------

;; Import and export
(require (prefix-in rw: "railway.rkt"))
(require "../helpers.rkt")
(provide railway-nmbs)

;; Railway-NMBS ADT
(define (railway-nmbs)  
  ; Get trains and return the ids
  (define (get-train-ids)
    (let loop
      ((trains (skip-tag (rw:get-trains)))
       (train-ids '()))
      ; last train
      (if (empty? trains)
          ; return ids
          train-ids
          ; else append id and loop
          (loop (next-train trains)
                (append train-ids (list ((head trains) 'id)))))))

  ; get detection tracks
  (define (get-detection-ids)
    ; get a list of detection-ids
    (map (lambda (detector) (detector 'id)) (skip-tag (rw:get-detections))))
  
  ; returns list of all switch id's
  (define (get-switch-ids)
    (map (lambda (switch) (switch 'id)) (skip-tag (rw:get-switches))))

  ; dispatcher
  (define (dispatch-railway-nmbs msg . args)
    (cond
      ((eq? msg 'train-ids)       (get-train-ids))
      ((eq? msg 'detection-ids)   (get-detection-ids))
      ((eq? msg 'switch-ids)      (get-switch-ids))
      ; get current detection-track-id that train with given id is on
      ((eq? msg 'prev-detection)  (rw:get-prev-detection  (arg-1 args)))
      ; returns the state of a specific switch
      ((eq? msg 'switch-state)    (rw:get-switch-state    (arg-1 args)))
      ((eq? msg 'train-speed)     (rw:get-train-speed     (arg-1 args)))
      ((eq? msg 'train-direction) (rw:get-train-direction (arg-1 args)))
      ((eq? msg 'train-traject)   (rw:get-train-traject   (arg-1 args)))
      (else (error "Dispatcher railway-nmbs." msg))))
  dispatch-railway-nmbs)

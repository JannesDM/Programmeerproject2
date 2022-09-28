#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;
;       interface.rkt
;
;       main interface to the simulator, import this file
;

(require "trains.rkt"
         "railway.rkt"
         "simulator.rkt")

(provide setup-hardware
         setup-straight
         setup-straight-with-switch
         setup-loop
         setup-loop-and-switches

         start
         stop

         get-detection-block-ids
         get-switch-ids
         
         add-loco
         remove-loco
         
         get-loco-speed
         set-loco-speed!
         get-loco-detection-block
         
         get-switch-position
         set-switch-position!)

;;; INTERFACE ;;;


;; ========================================================================== ;;
;;                                                                            ;;
;; (setup-hardware)                                                           ;;
;; (setup-straight)                                                           ;;
;; (setup-straight-with-switch)                                               ;;
;; (setup-loop)                                                               ;;
;; (setup-loop-and-switches)                                                  ;;
;;                                                                            ;;
;; initializes the simulator with a particular setup; check the pdf files     ;;
;; for an overview of all block and switch ids.                               ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (setup-hardware)                                                           ;;
;;                                                                            ;;
;; ========================================================================== ;;

;; ========================================================================== ;;
;;                                                                            ;;
;; (start)                                                                    ;;
;;                                                                            ;;
;; Starts the simulator. A higher framerate can be obtained by lowering       ;;
;; wait-per-frame in simulator.rkt.                                           ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (start)
  (initialize-simulator)
  (launch-simulator-loop))

;; ========================================================================== ;;
;;                                                                            ;;
;; (stop)                                                                     ;;
;;                                                                            ;;
;; Stops the simulator.                                                       ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define stop stop-simulator-loop)

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-detection-block-ids)                                                  ;;
;;                                                                            ;;
;; returns a list of all symbols of detection block ids                       ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-detection-block-ids)                                                  ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-detection-block-ids)
  (send RAILWAY get-detection-block-ids))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-switch-ids)                                                           ;;
;;                                                                            ;;
;; returns a list of all symbols of switch ids                                ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-switch-ids)                                                           ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-switch-ids)
  (send RAILWAY get-switch-ids))

;; ========================================================================== ;;
;;                                                                            ;;
;; (add-loco TRAIN-ID PREVIOUS-SEGMENT-ID CURRENT-SEGMENT-ID)                 ;;
;;                                                                            ;;
;; Adds a train to the segment with id CURRENT-SEGMENT-ID.                    ;;
;; The direction of  the train is determined by PREVIOUS-SEGMENT-ID.          ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (add-loco 'T-1 'S-27 '1-3)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define add-loco add-train)

;; ========================================================================== ;;
;;                                                                            ;;
;; (remove-loco TRAIN-ID)                                                     ;;
;;                                                                            ;;
;; Removes the train from the tracks                                          ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (remove-loco 'T-1)                                                         ;;
;;                                                                            ;;
;; ========================================================================== ;;


;; CHANGED!!
(define remove-loco remove-all-trains) ;remove-train)

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-loco-speed TRAIN-ID)                                                  ;;
;;                                                                            ;;
;; Returns the current speed of the train with id TRAIN-ID                    ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-loco-speed 'T-1)                                                      ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define get-loco-speed get-train-speed)

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-loco-speed! TRAIN-ID SPEED)                                           ;;
;;                                                                            ;;
;; Sets the speed of the train in mm/s with id TRAIN-ID to SPEED. Setting the ;;
;; speed to a negative number will invert the direction of the train.         ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (set-loco-speed! 'T-1 100)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define set-loco-speed! set-train-speed!)

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-loco-detection-block TRAIN-ID)                                        ;;
;;                                                                            ;;
;; Returns the id of the current detection block the train with id is on.     ;;
;; If the train is not currently on a detection block this will return #f.    ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-loco-detection-block 'T-1)                                            ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define get-loco-detection-block get-train-detection-block)

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-switch-position! SWITCH-ID POSITION)                                  ;;
;;                                                                            ;;
;; Sets the switch with id SWITCH-ID to POSITION. Only 1 and 2 are valid      ;;
;; position numbers. Consult opstelling_schema.pdf to get an overview of all  ;;
;; switch positions. Consult railway.rkt for an overview of all switch ids.   ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (set-switch-position! 'S-9 2)                                              ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (set-switch-position! id position)
  (request-redraw-segments)
  (send RAILWAY set-switch-position! id position))


;; ========================================================================== ;;
;;                                                                            ;;
;; (get-switch-position SWITCH-ID)                                            ;;
;;                                                                            ;;
;; Returns the position of the switch with id SWITCH-ID. Consult railway.rkt  ;;
;; for an overview of all switch ids.                                         ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-switch-position 'S-9)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-switch-position id)
  (send RAILWAY get-switch-position id))




#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;;  NMBS ADT
;; --------------------------------------------

;; Imports
(require (prefix-in gui: "gui.rkt"))
(require (prefix-in rw:  "../railway/railway-nmbs.rkt"))
(require (prefix-in graph: "../a-d/graph/graph-algos.rkt"))

(require "../helpers.rkt")

;; Export
(provide nmbs)

;; NMBS ADT
(define (nmbs tcp-port ui-size)
  (let
      ; setup nmbs-railway (get info)
      ((railway (rw:railway-nmbs))
       ; 1 GUI/NMBS
       (ui (gui:ui (ui-width ui-size) (ui-height ui-size)))
       ; tcp variables
       (in '())
       (out '())
       ; stop application boolean
       (exit #f))

    ; --------------------------------------------------------------
    ;; -- TCP ------------------------------------------------------
    
    ; Wait for server to set in and out vars
    (thread
     (lambda ()
       (define-values (local-in local-out)
         (tcp-connect "localhost" tcp-port))
       (set! in local-in)
       (set! out local-out)))

    ; Input-loop for reading commands
    (define (input-loop)
      (let ((input (read in)))
        (unless (empty? input)
          ; check if incoming commando is valid
          (when (hash-has-key? hash-table-in (input-name input))
            ; find and apply correct function from hash table
            (apply (hash-ref hash-table-in (input-name input))
                   (input-args input)))
          (unless exit (input-loop)))))

    ; Check for input, wait and restart
    (define (input-buffer-nmbs)
      (unless (empty? in)
        (input-loop))
      ; wait before restarting loop
      (sleep tcp-sleep-time)
      (unless exit (input-buffer-nmbs)))
    
    ; Start input buffer in thread
    (define thread-input
      (thread input-buffer-nmbs))

    ; Send message to infrabel via TCP
    (define (send-INFRA command)
      (write command out)
      ; flush output
      (flush-output out))

    ; --------------------------------------------------------------
    ;; -- Callback functions for UI --------------------------------
    
    ; Callback function to properly exit application
    (define (exit-callback)
      ; stop sisgnal
      (set! exit #t)
      
      ; send exit signal to infrabel
      (send-INFRA (make-tcp-msg 'exit-railway!))

      ; kill running thread
      (kill-thread thread-input)

      ; close the tcp connection
      (close-input-port in)
      (close-output-port out))

    
    ; Callback function for drawing Train(s)
    (define (train-callback train-id)
      (let ((slider-buffer #t))
        ; start the train
        (define (start)
          (infrabel-set-speed! train-id DEFAULT_TRAIN_SPEED))
      
        ; stops the train
        (define (stop)
          (infrabel-set-speed! train-id MIN_TRAIN_SPEED))

        ; change direction
        (define (change-dir!)
          (send-INFRA (make-tcp-msg 'flip-direction! train-id)))
      
        ; change train speed, callback for slider controller
        (define (set-speed-from-slider! speed)
          (infrabel-set-speed! train-id speed))

        ; returns the id of the last detection-track this train was on
        (define (get-prev-detection)
          (railway 'prev-detection train-id))
  
        ; dispatcher
        (define (dispatch-train-callback msg . args)
          (cond
            ((eq? msg 'start)           (start))
            ((eq? msg 'stop)            (stop))
            ((eq? msg 'change-dir!)      (change-dir!))
            ((eq? msg 'id)              train-id)
            ((eq? msg 'speed)           (railway 'train-speed train-id))
            ((eq? msg 'speed!)          (set-speed-from-slider! (arg-1 args)))
            ((eq? msg 'prev-detect)     (get-prev-detection))
            (else (error "Dispatcher train-callback." msg))))
        dispatch-train-callback))

    
    ; Callback function for drawing detection component
    (define (detection-callback)
      ; get a list of detection-ids
      (define (get-detection-ids)
        (railway 'detection-ids))

      ; is there a train on given detection-id?
      (define (train-on-track? detection-id)
        (train? detection-id))

      ; dispatcher
      (define (dispatch-detection-callback msg . args)
        (cond
          ((eq? msg 'detection-ids)  (get-detection-ids))
          ((eq? msg 'train?)         (train-on-track? (arg-1 args)))
          (else (error "Dispatcher detection-callback." msg))))
      dispatch-detection-callback)

    
    ; Callback function for drawing switch component
    (define (switch-callback)
      ; Get a list of switch-ids
      (define (get-switch-ids)
        (railway 'switch-ids))

      ; Get the state of the switch from given id
      (define (get-switch-state switch-id)
        (railway 'switch-state switch-id))

      ; Set the state of the switch from given id
      (define (set-switch-state switch-id state)
        ; send message to infrabel
        (send-INFRA (make-tcp-msg 'switch-state! switch-id state)))

      ; Dispatcher
      (define (dispatch-switch-callback msg . args)
        (cond
          ; getters
          ((eq? msg 'switch-ids)   (get-switch-ids))
          ((eq? msg 'switch-state) (get-switch-state  (arg-1 args)))
          ((eq? msg 'type)         'nmbs)
          ; setter
          ((eq? msg 'switch-state!) (set-switch-state (arg-1 args) (arg-2 args)))
          (else (error "Dispatcher switch-callback." msg))))
      dispatch-switch-callback)

    
    ; Callback function for traject management
    (define (traject-callback)     
      ; get a list of detection-ids
      (define (get-detection-ids)
        (railway 'detection-ids))

      ; returns list of all train-ids
      (define (get-train-ids)
        (railway 'train-ids))
            
      ; calculates traject and ?!
      ;     args: train id: number
      (define (calculate-traject train-id destination)
        ; train location, start from detection block
        (define start (label->index (railway 'prev-detection train-id)))

        ; get the index for the given label
        (define stop (label->index destination))
        
        ; traverse graph from train location to destination
        (define traject (get-trajectory start stop))
        
        ; send to infrabel
        (send-INFRA (make-tcp-msg 'set-trajectory! train-id traject)))

      ; reset traject of train with given id
      (define (clear-traject train-id)
        (send-INFRA (make-tcp-msg 'set-trajectory! train-id empty-traject)))

      ; is the given id of a train's traject empty?
      (define (empty-traject? train-id)
        (empty? (railway 'train-traject train-id)))
           
      ; dispatcher traject callback
      (define (dispatch-traject-callback msg . args)
        (cond
          ((eq? msg 'detection-ids)     (get-detection-ids))
          ((eq? msg 'train-ids)         (get-train-ids))
          ((eq? msg 'calculate-traject) (calculate-traject (arg-1 args) (arg-2 args)))
          ((eq? msg 'clear-traject)     (clear-traject (arg-1 args)))
          ((eq? msg 'empty-traject?)    (empty-traject? (arg-1 args)))
          (else (error "Dispatcher traject-callback." msg))))
      dispatch-traject-callback)

    ; --------------------------------------------------------------
    ;; -- Draw train components ------------------------------------

    ; Draw train components
    (define (draw-trains ui)
      ; skip 'trains tag
      (let ((train-ids (railway 'train-ids)))
        ; loop over trains
        (for ([curr-train train-ids])
          ; draw ui train components/train
          (ui 'draw-train (train-callback curr-train)))))

    ; Draw detection component
    (define (draw-detection)
      (ui 'draw-detection (detection-callback)))

    ; Draw switch component
    (define (draw-switch)
      (ui 'draw-switch (switch-callback)))

    ; Draw traject
    (define (draw-traject)
      (ui 'draw-traject (traject-callback)))
    
    ; --------------------------------------------------------------
    ;; -- Communication Infrabel -> NMBS ---------------------------
    
    ; Update UI's train speed
    (define (update-train-speed train-id new-speed)
      (ui 'update-speed train-id new-speed))

    ; Update UI's switch state
    (define (update-switch switch-id state)
      (ui 'update-switch switch-id state))

    ; Update UI's detection-block values
    (define (update-detection train-id prev-det)
      ; db = dropbox
      (ui 'update-det-db train? (amount-of-trains))
      (ui 'update-det-msg train-id prev-det (amount-of-trains)))

    ; Updates UI's traject
    (define (update-traject train-id traject)
      (ui 'update-traject train-id traject (amount-of-trains)))
    
    ; Gets a trajectory from the graph algos
    (define (get-trajectory start stop)
      ; traverse graph from train location to destination
      ;    doens't incalculate special manouvre time/length
      (flatten (graph:shortest-path start stop)))
    
    ; Same as above but sends answer to infrabel
    (define (get-trajectory-infra start stop)
      (send-INFRA (make-tcp-msg
                   'tcp-return-traject
                   (get-trajectory start stop))))

    ; Enable manual controls
    (define (destination-reached train-id)
      ; enable start, disable clear-traject
      (ui 'manual-controls! train-id #t #f))

    ; Create hash-table to map input and functions (string - procedure)
    (define hash-table-in (make-hash))
    (hash-set! hash-table-in 'update-speed      update-train-speed)
    (hash-set! hash-table-in 'update-switch     update-switch)
    (hash-set! hash-table-in 'update-detection  update-detection)
    (hash-set! hash-table-in 'update-traject    update-traject)
    (hash-set! hash-table-in 'dest-reached      destination-reached)
    ; return value
    (hash-set! hash-table-in 'get-trajectory    get-trajectory-infra)
    
    ; --------------------------------------------------------------
    ;; -- Local functions ------------------------------------------

    ; Returns the amount of trains based on the ids
    (define (amount-of-trains)
      (length (railway 'train-ids)))
    
    ; Is there a train on given detection-id?
    (define (train? detection-id)
      ; loop over trains
      (let loop
        ((train-ids (railway 'train-ids)))
        (cond
          ((eq? (railway 'prev-detection (head train-ids))
                detection-id)
           ; return found train-id
           (head train-ids))
          ; when no trains left, stop loop
          ((null? (next-train train-ids)) #f)
          (else (loop (next-train train-ids))))))

    ; Makes commando to send to infrabel
    (define (infrabel-set-speed! train-id speed)        
      (send-INFRA (make-tcp-msg 'set-speed! train-id speed)))

    ; --------------------------------------------------------------
    ;; -- Functions accesible trough dispatcher --------------------

    ; Start gui
    (define (start-ui)      
      ; draw components
      (draw-trains ui)
      (draw-detection)
      (draw-switch)
      (draw-traject)
      ; show ui
      (ui 'show exit-callback))

    ; Close gui
    (define (close-ui)
      (ui 'close))

    ; Exit NMBS instance
    (define (exit!)
      ; stop sisgnal
      (set! exit #t)

      ; kill running thread
      (kill-thread thread-input)

      ; close the tcp connection
      (close-input-port in)
      (close-output-port out))
      
    
    ;; Dispatcher NMBS
    (define (dispatch-nmbs msg . args)
      (cond
        ((eq? msg 'exit!)        (exit!))
        ((eq? msg 'start-ui)     (start-ui))
        ((eq? msg 'close-ui)     (close-ui))
        ((eq? msg 'type)         'nmbs)
        (else (error "Dispatcher nmbs." msg))))
    dispatch-nmbs))
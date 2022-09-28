#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;; Infrabel ADT
;; --------------------------------------------


;; Imports
(require "helpers.rkt")
(require (prefix-in rw: "railway/railway-infrabel.rkt"))
(require (prefix-in graph: "a-d/graph/graph-algos.rkt"))
;; Export
(provide infrabel)

;; Infrabel ADT 
(define (infrabel tcp-port)
  ; setup infra-railway
  (let ((railway (rw:railway-infrabel))
        ; tcp variables
        (in '())
        (out '())
        (exit #f))

    ; --------------------------------------------------------------
    ;; -- TCP ------------------------------------------------------
  
    ; Creates 'listening' server
    (define tcp-listener (tcp-listen tcp-port))

    ; Wait for a client to set ports
    (thread
     (lambda ()
       ; define port numbers
       (define-values
         (local-in local-out)
         (tcp-accept tcp-listener))
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
    (define (input-buffer-infra)
      (unless (empty? in)
        (input-loop))
      ; wait before restarting loop
      (sleep tcp-sleep-time)
      (unless exit (input-buffer-infra)))

    ; Start input reader in a thread
    (define thread-input
      (thread input-buffer-infra))
    (define thread-traject '())

    ; Send messege to NMBS via TCP
    (define (send-NMBS command)
      ; if connected to server
      (unless (empty? out)
        ; write command and flush output
        (write command out) 
        (flush-output out)))

    ; --------------------------------------------------------------
    ;; -- Local functions ------------------------------------------
    
    ; Setup railway and start trajectory-thread
    (define (setup-railway! simulation?)
      (railway 'setup-railway! simulation?)
      ; start traject manager
      (set! thread-traject (thread execute-trajectories)))
    
    ; Stops simulation
    (define (exit-railway!)
      ; stop signal
      (set! exit #t)
            
      ; exit simulation
      (railway 'exit-railway!)

      ; kill threads
      (kill-thread thread-traject)
      (kill-thread thread-input)
      
      ; close tcp connection
      (close-input-port in)
      (close-output-port out)
      (tcp-close tcp-listener))

    ; Set train speed and send update to NMBS
    (define (set-speed! train-id new-speed)
      ; only update if different
      (when (!= new-speed (railway 'train-speed train-id))
        (railway 'train-speed! train-id new-speed)
        (send-NMBS (make-tcp-msg 'update-speed train-id new-speed))))

    ; Flips train direction
    (define (flip-direction! train-id)
      (railway 'flip-dir! train-id))

    ; Set the state of the switch from given id
    ;   and send update to NMBS
    (define (set-switch-state! switch-id state)
      (railway 'switch-state! switch-id state)
      (send-NMBS (make-tcp-msg 'update-switch switch-id state)))

    ; NMBS -> set train from given id his traject
    (define (set-trajectory! train-id traject)
      (let ((curr-train (railway 'find-train train-id)))
        (when (empty? traject)
          ; clear traject
          (set-speed! (curr-train 'id) MIN_TRAIN_SPEED))
        ; clear reservations
        (unreserve-all! (curr-train 'prev-detect) curr-train)
        (curr-train 'traject! traject)))

    ; ---------------------------------------------------------------
    ;; -- Reservations ----------------------------------------------

    ; Sets reserved variable to #f
    (define (unreserve! track-id)
      (railway 'set-reserve! track-id #f))
      
    ; Reserve a track in railway
    (define (reserve! track-id train-id)
      (railway 'set-reserve! track-id train-id))

    ; Reserve the train's current location
    (define (reserve-current-location curr-train)
      ; track
      (reserve! (curr-train 'prev-detect) (curr-train 'id))
      ; train
      (curr-train 'reservations! (list (curr-train 'prev-detect))))

    ; unreserves all tracks reserved by train with id, except the current location
    (define (unreserve-all! curr-track curr-train)
      ; track
      (railway 'unreserve-all! (curr-train 'prev-detect) (curr-train 'id))
      ; train
      (curr-train 'reservations! (list curr-track)))

    ; --------------------------------------------------------------
    ;; -- Automatic trajectories -----------------------------------
    
    ; Thread a loop for updating the railway and NMBS
    (define (execute-trajectories)     
      ; main loop, keeps executing
      (define (trajectory-loop) 
        ; wait
        (sleep sleep-time-thread)
        
        ; loop over trains
        (train-loop (railway 'get-trains))
       
        ; if user didnt quit, restart loop
        (unless exit (trajectory-loop)))
      
      ; loop over trains
      (define (train-loop trains)
        (let*
            ; Abstractions
            ([trains           trains]
             [curr-train       (first trains)]
             [curr-traject     (curr-train 'traject)]
             [traject?         (not (empty? curr-traject))]
             [train-id         (curr-train 'id)]
             [prev-detect      (railway 'prev-detection train-id)]
             [prev-track       (curr-train 'prev-track)])

          ; update train's prev detection
          (curr-train 'prev-detect! prev-detect)

          ; always reserve current track
          (when (empty? (curr-train 'reservations))
            (reserve-current-location curr-train))
                      
          ; only perform actions if theres a traject loaded in the train
          (when traject?  
            ; ------ Abstractions -----------------------------------------------
            (define curr-destination (first curr-traject))
            
            ; final destionation reached -boolean
            (define final-destination?
              (and (empty? (next-track curr-traject))
                   (eq? prev-detect (head curr-traject))))
            
            ; Tries to make a reservation for the first block of tracks
            (define (make-reservation)
              ; make list of tracks to reserve
              ; start with train's current position
              (define to-reserve (list prev-detect))
              (define succes? #t)
              
              ; loop untill next detection is reached
              (let loop
                ((traject curr-traject)
                 (head-traj (head curr-traject)))

                ; main conditional
                (cond
                  ; skip first track if already reserved (= curr location)
                  ((eq? head-traj prev-detect)
                   (loop (next-track traject) (next-element traject)))
                  
                  ; already reserved track, stop loop
                  ((railway 'reserved? head-traj train-id)
                   (set! succes? #f)
                   (curr-train 'waiting! #t)

                   ; start thread that resets train's waiting status
                   (thread (lambda () (train-wait curr-train))))

                  ; else add to reservations list
                  (else (set! to-reserve (flatten (list to-reserve head-traj)))
                        ; continue loop if still tracks in traject
                        (unless (empty? (next-track traject))
                          ; continue loop if not a detection block
                          (unless (detection? head-traj)
                            (loop (next-track traject) (next-element traject)))))))
                
              ; if reservation is possible
              (when succes?
                ; set reservation for each track
                (map (lambda (track-id)
                       (reserve! track-id train-id))
                     to-reserve)
                
                ; set train's reserved list and start train
                (curr-train 'reservations! to-reserve)
                (set-speed! train-id DEFAULT_TRAIN_SPEED)))

            ; update traject
            (define (update-traject)
              ; update traject and train variables
              (curr-train 'prev-track! (first curr-traject))
              (set! curr-traject (next-track curr-traject))
              (curr-train 'traject! curr-traject)
              
              ; send update to NMBS
              (send-NMBS (make-tcp-msg 'update-traject train-id curr-traject))
              
              ; set correct train speed
              (speed-control))

            ; Wait before trying to re-reserve
            (define (train-wait train)
              (sleep train-wait-time)
              (train 'waiting! #f))

            ; only update if train is faster then given speed
            (define (set-speed-slower! new-speed)
              (when (> (curr-train 'speed) new-speed)
                (set-speed! train-id new-speed)))

            ; set train's speed based on traject
            (define (speed-control)
              (let*
                  ((curr-track (head curr-traject)))
                (cond
                  ; nearing destination (1 & 2 tracks left)
                  ((<= (length curr-traject) 1)
                   (set-speed-slower! ENDING_TRAIN_SPEED))
                  ((<= (length curr-traject) 2)
                   (set-speed-slower! SLOW_TRAIN_SPEED))

                  ; nearing reservation's end
                  ((= (length (curr-train 'reservations)) 2)
                   (set-speed-slower! SLOW_TRAIN_SPEED))

                  ; prev = long track
                  ((long-track? prev-detect)
                   (set-speed! train-id FAST_TRAIN_SPEED))

                  ; prev = short track
                  ((short-track? prev-detect)
                   (set-speed! train-id SLOW_TRAIN_SPEED))

                  ; 2do: switches & special manoeuvre
                  
                  ; else default speed
                  (else (set-speed! train-id DEFAULT_TRAIN_SPEED)))))

            ; code to execute if next detection block is reached
            (define (next-detection-reached)
              ; unreserve
              (when (eq? curr-destination (last (curr-train 'reservations)))
                ; unreserve tracks except current position
                (unreserve-all! curr-destination curr-train))

              ; flip direction if needed
              (when (or (eq? prev-track (first curr-traject))
                        (eq? prev-track (next-element curr-traject)))
                (flip-direction! train-id))
      
              ; send update to NMBS and update traject
              (send-NMBS (make-tcp-msg 'update-detection train-id prev-detect))
               
              ; update the trajectory
              (update-traject))

            ; code to excecute if next track is a switch
            (define (next-is-switch)
              ; switch info
              (define switch-current-state (railway 'switch-state curr-destination))

              ; get goal and from states of switch
              (define goal-state
                (get-goal-state curr-destination (second curr-traject)))
              (define from-state
                (get-goal-state curr-destination prev-track))
               
              ; Switch conditional
              (cond
                ;; 1 Special procedure
                ((and (!= from-state 0)
                      (!= goal-state 0))
                 ; stop train
                 (set-speed! train-id MIN_TRAIN_SPEED)
                  
                 ; get next detectionblock to turn on
                 (define next-detect
                   (next-detection-block curr-destination prev-track))

                 ; get the trajectory to get to next detection block
                 (define next-trajectory (calculate-trajectory prev-track next-detect))

                 ; unreserve past traject
                 (let loop
                   ((reservations (curr-train 'reservations)))
                   (unless (empty? reservations)
                     (unless (eq? (head reservations) curr-destination)
                       (unreserve! (head reservations))
                       (loop (next-track reservations)))))
                  
                 ; reset train's reservation list to trigger re-reservation in next iteration
                 (curr-train 'reservations! (list prev-detect))
                 
                 ; update current trajectories
                 ;   dummy is to 'counter' the next '(set! curr-traject ..'
                 (define dummy (list (head next-trajectory)))
                 (set! curr-traject (append dummy next-trajectory curr-traject))

                 ; set switch
                 (set-switch-state! curr-destination from-state)
                  
                 ; restart train
                 (set-speed! train-id SLOW_TRAIN_SPEED))
                      
                ;; 2 Standard procedure
                ((!= goal-state switch-current-state)
                 ; set swithc in correct state
                 (when (not (and (= goal-state 0) (= from-state 0)))
                   (set-switch-state! curr-destination
                                      (if (!= goal-state 0) goal-state from-state)))))
              ; either case: update traject
              (update-traject))
            
            ; --------------------------------------------------------------------
            ; -- (when traject? begin --------------------------------------------
            
            ; Update traject for nmbs' gui
            (send-NMBS (make-tcp-msg 'update-traject train-id curr-traject))
            (send-NMBS (make-tcp-msg 'update-speed train-id (curr-train 'speed)))
            
            ;; Main Conditional
            (cond
              ; 1. Final destination reached
              (final-destination?
               ; 2do: send status message to NMBS
               (debug "Destination reached, stopping train with id:" train-id)
               (set-speed! train-id MIN_TRAIN_SPEED)
               
               ; init values
               (unreserve-all! curr-destination curr-train)
               (set! curr-traject '())
               (curr-train 'traject! '())

               ; send update to NMBS
               (send-NMBS (make-tcp-msg 'dest-reached train-id)))

              ; 2. If no resrvations
              ((no-reservations? (curr-train 'reservations))
               (if (curr-train 'waiting?)
                   ; stop train
                   (set-speed! train-id MIN_TRAIN_SPEED)
                   ; try to make reservation
                   (make-reservation)))

              ; 3. Next detection track is reached
              ((eq? prev-detect curr-destination)
               (next-detection-reached))
      
              ; 4. Next track is a switch
              ((switch? curr-destination)
               (next-is-switch))))

          ; When no traject is loaded still send updates to NMBS
          ;    for manual train controls
          (unless traject?
            ; manual control reserving
            (unless (eq? prev-detect (head (curr-train 'reservations)))
              (unreserve-all! prev-detect curr-train)
              (reserve-current-location curr-train))
            ; send updates to NMBS
            (send-NMBS (make-tcp-msg 'update-detection train-id prev-detect))
            (send-NMBS (make-tcp-msg 'update-traject train-id curr-traject)))
          
          ; Iterate with next train
          (unless (null? (next-train trains))
            (train-loop (next-train trains)))))
      
      ;; Start main loop
      (trajectory-loop))
    
    ; Get the next detection block from current position
    ;   coming from state 1 or 2 -> only go to 0 
    ;   coming from state 0      -> doesn't matter (1 or 2)
    ;     2do: choose the one with closest detection block (shortest path ...)
    (define (next-detection-block curr-id from-id)
      (if (switch? curr-id)
          (let*
              ((curr-switch (railway 'get-switch curr-id))
               (next-0 (curr-switch 'track-id-0))
               (next-1 (curr-switch 'track-id-1))
               (next-2 (curr-switch 'track-id-2))
               (goal-value '()))

            (set! goal-value (get-goal-state curr-id from-id))
            ;
            (if (= 0 goal-value)
                ; continue on 'track-id-1 or 2
                ; 1 or 2?
                (if (switch? next-1)
                    (next-detection-block next-2 curr-id)
                    (next-detection-block next-1 curr-id))
                ; continue on 'track-id-0
                (next-detection-block next-0 curr-id)))
          curr-id))

    ; TCP return value
    ; 2do: remove var and remove wait
    (define traject-value '())
    
    ; Sets trajectory (from NMBS)
    (define (tcp-return-traject traject)
      (set! traject-value traject))
    (set! traject-value '())
    
    ; Returns a traject from arg-1 to arg-2
    (define (calculate-trajectory prev-track next-detect)
      ; Waits for traject to be set
      (define (wait-for-traject)
        (when (empty? traject-value)
          (sleep goal-sleep-time)
          (wait-for-traject)))
      
      ; send commando to nmbs
      (send-NMBS (make-tcp-msg
                  'get-trajectory
                  (label->index prev-track)
                  (label->index next-detect)))
      ; wait untill NMBS 'replied'
      ;   (-> 2thread & stop train untill value has returned?)
      (wait-for-traject)
      
      ; skip prev and curr tracks
      (let ((road-to (skip-2-elements traject-value)))
        ; append reverse for turning and return this list
        (append road-to (next-track (reverse road-to)))))

    ; Get the goal state for the current traject
    ;    if #f then return 0, (= it doesn't matter)
    (define (get-goal-state switch-id to-id)     
      ; gets the label from a given edge
      (define label-states (graph:get-edge-label switch-id to-id))

      ; value transformation
      (if label-states
          (string->number
           (string
            (string-ref
             (symbol->string label-states)
             ; index of the node determens direction
             (if (< (label->index switch-id)
                    (label->index to-id))
                 0 2))))
          0))

    ; Create hash-table: symbol - procedure
    (define hash-table-in (make-hash))
    (hash-set! hash-table-in 'exit-railway!      exit-railway!)
    (hash-set! hash-table-in 'set-speed!         set-speed!)
    (hash-set! hash-table-in 'flip-direction!    flip-direction!)
    (hash-set! hash-table-in 'switch-state!      set-switch-state!)
    (hash-set! hash-table-in 'set-trajectory!    set-trajectory!)
    ; return value
    (hash-set! hash-table-in 'tcp-return-traject tcp-return-traject)

    ; Exit Infrabel instance
    (define (exit!)
      ; stop signal
      (set! exit #t)

      ; kill threads
      (kill-thread thread-traject)
      (kill-thread thread-input)
      
      ; close tcp connection
      (close-input-port in)
      (close-output-port out)
      (tcp-close tcp-listener))
    
    ;; Dispatcher Infrabel
    (define (dispatch-infrabel msg . args)
      (cond
        ((eq? msg 'exit!)           (exit!))
        ((eq? msg 'setup-railway!)  (setup-railway! (arg-1 args)))
        ((eq? msg 'type)            'infrabel)
        (else (error "Dispatcher Infrabel." msg))))
    dispatch-infrabel))








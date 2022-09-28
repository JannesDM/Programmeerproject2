#lang racket

;; --------------------------------------------
;; Jannes De Mets
;; ---------------
;; GUI ADT
;; --------------------------------------------
;; 2do:
;;  abstractify list operations!
;;  abstractify get-children w/ global constants for order!
;;  Reservations:
;;     show train reservations list
;;     label reserved for detection
;;     label reserved for switch
;; -----------------------------------------------------------

;; Imports
(require racket/gui/base)  ; Racket GUI
(require "../helpers.rkt") ; Helpers

; Export
(provide ui)

;; The User Interface ADT
(define (ui screen-width screen-height)
  (let ()
    
    ;;; Functions accesible via dispatcher
    ;; Show frame
    (define (open-GUI exit-callback)
      ;; Button to exit the GUI and simulation
      (new button%
           [parent main-app-frame]
           [label "Exit"]
           [callback (lambda (button event)
                       (exit-callback)
                       (close-GUI))])
      (send main-app-frame show #t))

    ;; Close frame
    (define (close-GUI)
      (send main-app-frame show #f))

    ;;-- Config ----------------------------------------------
    
    ;; The main window
    (define main-app-frame
      (new frame%
           [label "GUI - v1.01"]
           ; init location of frame
           [x 100]
           [y 100]
           [width screen-width]
           [height screen-height]))
    ; error messages
    (send main-app-frame create-status-line)

    ;; Panels for grouping components
    ; order is important
    (define panel-train
      (new horizontal-panel%
           [parent main-app-frame]
           [vert-margin 4]
           [horiz-margin 5]
           [style (list 'border)]
           [border 1]))

    (define panel-traject
      (new vertical-panel%
           [parent main-app-frame]
           [vert-margin 4]
           [horiz-margin 5]
           [style (list 'border)]
           [border 1]))
    
    (define panel-detection
      (new horizontal-panel%
           [parent main-app-frame]
           [vert-margin 4]
           [horiz-margin 5]
           [style (list 'border)]
           [alignment '(center center)]
           [border 1]))
 
    (define panel-switch
      (new horizontal-panel%
           [parent main-app-frame]
           [vert-margin 4]
           [horiz-margin 5]
           [style (list 'border)]
           [alignment '(center center)]
           [border 1]))

    
    ; ----- Draw train components ------------------------------
    
    (define (draw-train callback)
      ; panel for commands
      (define panel-commands
        (new vertical-panel%
             [parent panel-train]
             [vert-margin 5]
             [spacing 5]))

      ; message with the correct train id
      (define train-label
        (string-append "Train "
                       (number->string (callback 'id))
                       " commands:"))
      (new message%
           [parent panel-commands]
           [label train-label])
   
      ; button to start the train (= default speed)
      ; position: 2
      (new button%
           [parent panel-commands]
           [label "Start"]
           [callback (lambda (button event)
                       (send speed-slider set-value DEFAULT_TRAIN_SPEED)
                       (callback 'start))])

      ; button to stop the train
      (new button%
           [parent panel-commands]
           [label "Stop"]
           [callback (lambda (button event)
                       (send speed-slider set-value MIN_TRAIN_SPEED)
                       ; enable start & clear-traject
                       (manual-controls! (callback 'id) #t #t)
                       (callback 'stop))])

      ; button to change direction of the train
      ; position: 4
      (new button%
           [parent panel-commands]
           [label "Change Direction"]
           [callback (lambda (button event)
                       (callback 'change-dir!))])
        
      ; message with the correct train id
      (new message%
           [parent panel-commands]
           [label "Last detection:"])
      ; position: 6
      (new message%
           [parent panel-commands]
           [label (symbol->string (callback 'prev-detect))]
           [auto-resize #t])

      ; messages for showing current traject
      (new message%
           [parent panel-commands]
           [label "Traject:"])
      ; position: 8
      (new message%
           [parent panel-commands]
           [label "-"]
           [auto-resize #t])
            
      ; panel for speed slider
      (define panel-slider
        (new vertical-panel%
             [parent panel-train]
             [spacing 5]))

      ; proces to run in thread for 'debouncing' slider input
      (define (buffer)
        (sleep slider-buffer-time)
        (callback 'speed! (send speed-slider get-value)))

      ; for debouncing timing
      (define prev-time 0)
      (define worker-thread (thread (lambda () (sleep 0))))
      
      ; slider for changing speed
      (define speed-slider
        (new slider%
             [parent panel-slider]
             [style (list 'vertical 'vertical-label)]
             [label "Speed: "]
             [init-value (callback 'speed)]
             [min-value MIN_TRAIN_SPEED]	 
             [max-value MAX_TRAIN_SPEED]
             ; callback debounces using event's time-stamp
             [callback (lambda (slider event)
                         (let ((time (send event get-time-stamp)))
                           ; kill worker
                           (kill-thread worker-thread)
                           
                           ; when enough time has passed    
                           (when (> (- time prev-time) timing-debounce-slider)
                             ; set speed and update timer
                             (callback 'speed! (send speed-slider get-value))
                             (set! prev-time time))

                           ; reset worker
                           (set! worker-thread (thread buffer))))]))

      ; done
      #t)

    ; ----- Draw trajectory controls -----------------------------------
    
    (define (draw-traject callback)
      ; panel for message
      (define panel-message
        (new horizontal-panel%
             [parent panel-traject]
             [spacing 5]))
      
      ; select a traject message
      (new message%
           [parent panel-message]
           [label "Choose a train and its destination:"])

      ; panel for combo-fields
      (define panel-combo-fields
        (new horizontal-panel%
             [parent panel-traject]
             [alignment '(center center)]
             [spacing 5]))
      
      ; combo-field for selecting a train
      (define train-combo
        (new combo-field%
             [parent panel-combo-fields]
             [label "Train: "]	 
             [choices  (map (lambda (train-id)
                              (string-append "Train " (number->string train-id)))
                            (callback 'train-ids))]
             [init-value "Choose a train"]
             [stretchable-width #f]
             [min-width 170]
             [callback (lambda (combo event)
                         (let ((input-id (transform-train-input (send train-combo get-value))))
                           ; check if current selected train equals incoming message
                           (when (valid-train? input-id)
                             (if (callback 'empty-traject? input-id)
                                 (send clear-traject enable #f)
                                 (send clear-traject enable #t)))))]))

      ; combo-field for selecting a detector block  
      (define detection-combo
        (new combo-field%
             [parent panel-combo-fields]
             [label "Detectors:  "]	 
             [choices  (map symbol->string (callback 'detection-ids))]
             [init-value "Choose a detector"]
             [stretchable-width #f]
             [min-width 210]))

      ; panel for submit button
      (define panel-submit
        (new horizontal-panel%
             [parent panel-traject]
             [alignment '(center center)]
             [spacing 5]))

      ; check for valid input, hardcoded
      ;  2do: abstractify and use for all input fields
      (define (valid-train? input)
        (if input
            (let loop
              ((train-ids (callback 'train-ids)))
              (cond ((empty? train-ids) #f)
                    ((eq? input (head train-ids))
                     ; reset error message and return #t
                     (send main-app-frame set-status-text "")
                     #t)
                    (else (loop (next-train train-ids)))))
            #f))

      ; railway's (get-switch-state) returns #f if switch not found
      (define (valid-detection? input)
        (let loop
          ((det-list (callback 'detection-ids)))
          (cond ((empty? det-list) #f)
                ((eq? input (head det-list))
                 ; reset error message and return #t
                 (send main-app-frame set-status-text "")
                 #t)
                (else (loop (next-track det-list))))))
      
      ; Callback function for pressing submit on traject controls
      ;   validates input and sets manual controls
      (define (submit-button-callback button event)
        (let
            ; get input
            ((input-train (transform-train-input (send train-combo get-value)))
             (input-detection (string->symbol (send detection-combo get-value))))
          ; validate input
          (if (and (valid-train? input-train) (valid-detection? input-detection))
              ; valid
              (begin
                (callback 'calculate-traject input-train input-detection)
                ; enable on loading track
                (send clear-traject enable #t)
                ; disable start, enable clear-traject
                (manual-controls! input-train #f #t))
              ; invalid
              (send main-app-frame set-status-text "False input for traject"))))
      
      ; submit button
      (new button%
           [parent panel-submit]
           [label "Submit"]
           [callback (lambda (button event)
                       (submit-button-callback button event))])

      ; button to clear trajectory
      (define clear-traject
        (new button%
             [parent panel-submit]
             [label "Clear traject"]
             [enabled #f]
             [callback (lambda (button event)
                         (let ((train-id (transform-train-input (send train-combo get-value))))
                           (callback 'clear-traject train-id)
                           ; enable start, disable clear-traject
                           (manual-controls! train-id #t #f)))]))
      ; done
      #t)

    ; ----- Draw switch and detection components -------------------------------
    
    ;; Detection
    (define (draw-detection callback)
      ; abstraction
      (define (on-detecion-combo)
        (define train-id
          (callback 'train? (string->symbol (send detection-combo get-value))))
        (if train-id
            (send detect-state set-label
                  ; show train id
                  (string-append "Train #" (number->string train-id)))
            (send detect-state set-label "No train")))
      
      ; combo-field for selecting a detector block  
      (define detection-combo
        (new combo-field%
             [parent panel-detection]
             [label "Detectors:  "]	 
             [choices  (map symbol->string (callback 'detection-ids))]
             [init-value "Choose a detector"]
             [stretchable-width #f]
             [min-width 210]
             [callback (lambda (detector event) (on-detecion-combo))]))
      
      ; message for state of detector
      (define detect-state
        (new message%
             [parent panel-detection]
             [label "-"]
             [min-width 60]))

      ; done
      #t)

    ;; Switch
    (define (draw-switch callback)
      ; Abstraction
      (define (on-switch-combo)
        (define switch-id (string->symbol (send switch-combo get-value)))
        (define switch-state (callback 'switch-state switch-id))

        ; Sometimes the number returns in a list
        (when (pair? switch-state) (set! switch-state (head switch-state)))
        
        ; #t if switch, #f if not
        (if switch-state
            (begin
              ; enable radio buttons
              (send switch-radio enable true)
              
              ; 3-way?
              (if (three-way? switch-id)
                  (send switch-radio enable 2 true)
                  (send switch-radio enable 2 false))
              ; - 1 because combo radio-list starts counting from 0
              (send switch-radio set-selection (- switch-state 1))
              
              ; reset error message 
              (send main-app-frame set-status-text ""))

            ; not a switch
            (begin
              ; disable radio buttons
              (send switch-radio enable false)
              ; set error message
              (send main-app-frame set-status-text "No switch with that id"))))
      
      ; Combo-field for selecting a switch
      (define switch-combo    
        (new combo-field%
             [parent panel-switch]
             [label "Switch: "]
             [init-value "Choose a switch"]
             [stretchable-width #f]
             [min-width 180]
             [choices  (map symbol->string (callback 'switch-ids))]	 
             [callback (lambda (switch event) (on-switch-combo))]))
  
      ; Abstraction
      (define (on-radio)
        (let
            ; + 1 becouse combo field starts counting from 0
            ((position (+ 1 (send switch-radio get-selection)))
             (id (string->symbol (send switch-combo get-value))))
          (callback 'switch-state! id position)))
      
      ; Radio-box for setting and reading switch position
      (define switch-radio
        (new radio-box%
             [parent panel-switch]
             [label "Position: "]
             [choices (list "1" "2" "3")]
             [enabled false]
             [callback (lambda (radio event) (on-radio))]))
          
      ; done
      #t)

    ; ----- Updates -------------------------------------------------
     
    ;; Update train speed
    (define (update-speed train-id speed)
      (let ((slider
             (first (train-panel-children train-id slider-sub-position))))
        (send slider set-value speed)))

    ;; Update switch state
    (define (update-switch switch-id state)
      (let ((radio
             (list-ref (send panel-switch get-children) radio-position))
            (selected-switch
             (string->symbol (send (list-ref (send panel-switch get-children)
                                             switch-sub-position)
                                   get-value))))
        ; only change radio buton if selected switch schanged
        (when (eq? selected-switch switch-id)
          (send radio set-selection (- state 1))))) ; -1 for index

    ;; Update current selected detector's state-message
    ;    if proc-train? returns #f it means theres no train on the given detect-id
    (define (update-detection-dropbox proc-train? amount-of-trains)
      ; wait for the gui to draw its components
      (when (= (length (send panel-train get-children))
               (* amount-of-trains panels-per-train))
        ; get panel components
        (let*
            ; first sub-panel
            ((detect-combo (list-ref (send panel-detection get-children)
                                     detect-sub-position))
             ; second sub-panel
             (detect-message (list-ref (send panel-detection get-children)
                                       det-msg-sub-position))
             (selected-detect (string->symbol (send detect-combo get-value))))

          ; check for train on selected dropbox item
          (define detect-id (proc-train? selected-detect))
          (if detect-id
              (send detect-message set-label
                    ; show train id
                    (string-append "Train #" (number->string detect-id)))
              (send detect-message set-label "No train")))))

    ;; update prev-detection message, abstractify!
    ; 2do: abstractify, same as above
    (define (update-detection-message train-id prev-det amount-of-trains)
      ; wait for the gui to draw its components
      (when (= (length (send panel-train get-children))
               (* amount-of-trains panels-per-train))
        (let ((status-msg
               (status-msg-position (train-panel-children train-id train-sub-position))))
          (send status-msg set-label (symbol->string prev-det)))))

    ; Updates traject in ui
    (define (update-traject train-id traject amount-of-trains)
      ; wait for the gui to draw its components
      (when (= (length (send panel-train get-children))
               (* amount-of-trains panels-per-train))
        (let ((traject-msg
               (traject-msg-position (train-panel-children train-id train-sub-position)))
              (dir-button
               (dir-button-position (train-panel-children train-id train-sub-position))))
          ; check if there is currently a traject
          (if (empty? traject)
              (begin
                (send dir-button enable #t)
                (send traject-msg set-label "-"))
              (begin
                (send dir-button enable #f)
                ; format with "~a" 'converts each v to a string in display mode'
                (send traject-msg set-label (format "~a" traject)))))))

    
    ; --- Abstractions -----------------------------------
    
    ; Deconstruct train-panel
    ; 2do: same for other panels
    (define (train-panel-children train-id sub-position)
      (send 
       (list-ref
        (send panel-train get-children)
        (- (* train-id panels-per-train) sub-position))
       get-children))

    ; set manual control for given train
    (define (manual-controls! train-id start? clear?)
      (let ; start button is second element
          ((start-button
            (start-button-position (train-panel-children train-id start-sub-position)))
           (clear-traject
            (clear-traject-position
             ; third sub-panel
             (send (third (send panel-traject get-children)) get-children))))
        
        ; disable manual controls
        (send start-button enable start?)
        (send clear-traject enable clear?)))

    ; Transforms the train name's user input
    (define (transform-train-input input)
      ; has to be length 6
      (if (>= (string-length input) 6)
          ; - 48 for converting from ascii-value (= integer) to number
          (- (char->integer (string-ref input 6)) 48)
          #f))

    ; panel info
    (define panels-per-train 2)
    (define radio-position 1)
    ; position of graphical elements in their panel
    (define traject-msg-position eighth)
    (define status-msg-position sixth)
    (define start-button-position second)
    (define dir-button-position fourth)
    (define clear-traject-position second)
    (define train-combo-position first)
    ; position of elements in subpanel
    (define slider-sub-position 1)
    (define switch-sub-position 0)
    (define detect-sub-position 0)
    (define det-msg-sub-position 1)
    (define train-sub-position 2)
    (define start-sub-position 2)
    
    ;; Dispatcher gui
    (define (dispatch-gui msg . args)
      (cond
        ((eq? msg 'show)             (open-GUI (arg-1 args)))
        ((eq? msg 'close)            (close-GUI))
        ((eq? msg 'draw-train)       (draw-train (arg-1 args)))
        ((eq? msg 'draw-detection)   (draw-detection (arg-1 args)))
        ((eq? msg 'draw-switch)      (draw-switch (arg-1 args)))
        ((eq? msg 'draw-traject)     (draw-traject (arg-1 args)))
        ((eq? msg 'update-speed)     (update-speed (arg-1 args) (arg-2 args)))
        ((eq? msg 'update-switch)    (update-switch (arg-1 args) (arg-2 args)))
        ((eq? msg 'update-det-db)    (update-detection-dropbox (arg-1 args) (arg-2 args)))
        ((eq? msg 'update-det-msg)   (update-detection-message (arg-1 args) (arg-2 args) (arg-3 args)))
        ((eq? msg 'update-traject)   (update-traject (arg-1 args) (arg-2 args) (arg-3 args)))
        ((eq? msg 'manual-controls!) (manual-controls! (arg-1 args) (arg-2 args) (arg-3 args)))
        (else (error "Dispatcher GUI." msg))))
    dispatch-gui))

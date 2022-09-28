#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;
;       railway.rkt
;
;       Description of the different setups according to piko_tracks.pdf and setup_*.pdf
;       Last update: 2019-07-05
;

(require racket/class math/bigfloat)

(require "tracks.rkt")

(provide RAILWAY remove-all-tracks setup-hardware setup-straight setup-straight-with-switch setup-loop setup-loop-and-switches)

(define RAILWAY #f)

(define (remove-all-tracks)
  (set! RAILWAY #f))

;; ************************** ;;
;;      PIKO TRACK TYPES      ;;
;; ************************** ;;

(define G62    (make-object straight-track% 61.88))

(define G119   (make-object straight-track% 119.54))

(define G231   (make-object straight-track% 230.93))

(define G239   (make-object straight-track% 239.07))

(define R2     (make-object curved-track% 421.88 (/ pi 6)))

(define R2-7.5 (make-object curved-track% 421.88 (/ pi 24)))

(define R3     (make-object curved-track% 483.75 (/ pi 6)))

(define R9     (make-object curved-track% 907.97 (/ pi 12)))

(define WR     (make-object switch-track% (list G239 R9)))

(define WL     (make-object switch-track% (list G239 (invert R9))))

(define BWR    (make-object switch-track% (list R2 (list G62 R2))))

(define BWL    (make-object switch-track% (list (invert R2) (list G62 (invert R2)))))

(define W3     (make-object switch-track% (list R9 G239 (invert R9))))

(define DKWR   (make-object switch-track% (list (make-object straight-track% (/ 239.07 2)) (make-object curved-track% (/ 907.97 2) (/ pi 12)))))

;; ***************** ;;
;;      RAILWAY      ;;
;; ***************** ;;

(define railway%
  (class object%
    (super-new)
    (init-field segments switches detection-blocks)
    (define (find-segment-in id segments)
      (let ((segment
             (findf (lambda (segment)
                      (equal? (get-field id segment) id))
                    segments)))
        (if segment
            segment
            #f)))
    (define/public (find-segment id)
      (find-segment-in id segments))
    (define/public (find-switch id)
      (find-segment-in id switches))
    (define/public (find-detection-block id)
      (find-segment-in id detection-blocks))
    (define/public (set-switch-position! id position)
      (if (member position '(1 2))
          (let ((switch (find-segment-in id switches)))
            (if switch
                (send switch set-switch-position (- position 1))
                (error "Not a valid switch id" id)))
          (error "Not a valid switch position" position)))
    (define/public (get-switch-position id)
      (+ (send (find-switch id) get-switch-position) 1))
    (define/public (get-detection-block-ids)
      (map (lambda (block)
             (get-field id block))
           detection-blocks))
    (define/public (get-switch-ids)
      (map (lambda (block)
             (get-field id block))
           switches))      
    (define/public (draw-all . context)
      (for-each
       (lambda (segment)
         (send/apply segment draw context))
       segments))
    (define/public (build width height)
      (send (car segments) initiate-build (/ width 5) (/ height 5) 0))))

;; ************************ ;;
;;      SETUP HARDWARE      ;;
;; ************************ ;;

(define (setup-hardware)
  (define 1-1 (make-object block% '1-1 (list G231 G231 G231 G231 G231)))
  (define 1-2 (make-object block% '1-2 (list R2 G62 G239 G231 G231 G231 G231)))
  (define 1-3 (make-object block% '1-3 (list R3 G239 G231 G231 G231 G231 G231 G239)))
  (define 1-4 (make-object block% '1-4 (list R3 R3 R3)))
  (define 1-5 (make-object block% '1-5 (list G239 G231 G231 G231 G231 G231)))
  (define 1-6 (make-object block% '1-6 (list G239 G231 G231 G231 G231)))
  (define 1-7 (make-object block% '1-7 (list R2 R2 R2 R2 R2)))
  (define 1-8 (make-object block% '1-8 (list G239 G119)))
  (define 2-1 (make-object block% '2-1 (list G231 G239 G119)))
  (define 2-2 (make-object block% '2-2 (list G231 (invert R2-7.5) (invert R2-7.5) G239 G239)))
  (define 2-3 (make-object block% '2-3 (list R2 R2 R2 R2 R2 G239)))
  (define 2-4 (make-object block% '2-4 (list R3 R3 R3)))
  (define 2-5 (make-object block% '2-5 (list G231 G239 G231)))
  (define 2-6 (make-object block% '2-6 (list G239 G231)))
  (define 2-7 (make-object block% '2-7 (list G231 R9)))
  (define 2-8 (make-object block% '2-8 (list G239 G239)))
  (define U-1 (make-object block% 'U-1 R2))
  (define U-2 (make-object block% 'U-2 R3))
  (define U-3 (make-object block% 'U-3 (list G231 G62 R2)))
  (define U-4 (make-object block% 'U-4 R2))
  (define U-5 (make-object block% 'U-5 (list G231 G231 G231)))
  (define U-6 (make-object block% 'U-6 G231))
  (define U-7 (make-object block% 'U-7 (list G231 G231 G231)))
  
  (define S-1   (make-object switch% 'S-1 WL))
  (define S-2-3 (make-object switch% 'S-2-3 W3))
  (define S-4   (make-object switch% 'S-4 (reverse-indeces WR)))
  (define S-5   (make-object switch% 'S-5 DKWR))
  (define S-6   (make-object switch% 'S-6 DKWR))
  (define S-7   (make-object switch% 'S-7 WR))
  (define S-8   (make-object switch% 'S-8 WL))
  (define S-9   (make-object switch% 'S-9 WR))
  (define S-10  (make-object switch% 'S-10 WL))
  (define S-11  (make-object switch% 'S-11 (reverse-indeces WR)))
  (define S-12  (make-object switch% 'S-12 (reverse-indeces BWR)))
  (define S-16  (make-object switch% 'S-16 (reverse-indeces WL)))
  (define S-20  (make-object switch% 'S-20 WR))
  (define S-23  (make-object switch% 'S-23 (reverse-indeces BWL)))
  (define S-24  (make-object switch% 'S-24 BWL))
  (define S-25  (make-object switch% 'S-25 WL))
  (define S-26  (make-object switch% 'S-26 BWR))
  (define S-27  (make-object switch% 'S-27 (reverse-indeces BWR)))
  (define S-28  (make-object switch% 'S-28 BWL))
  
  (connect (end   S-26 0) (start U-1   ))
  (connect (end   S-26 1) (start S-27  ))
  (connect (end   S-27 1) (start 1-2   ))
  (connect (end   S-27 0) (start 1-3   ))
  (connect (end   1-3   ) (start U-2   ))
  (connect (end   U-2   ) (end   S-24 1))
  (connect (end   U-3   ) (end   S-24 0))
  (connect (start S-24  ) (end   S-23 0))
  (connect (end   U-4   ) (end   S-23 1))
  (connect (start S-23  ) (start 2-4   ))
  (connect (end   1-2   ) (start S-9   ))
  (connect (end   S-9  0) (start U-3   ))
  (connect (end   S-9  1) (end   S-11 0))
  (connect (end   S-11 1) (start S-10  ))
  (connect (start S-11  ) (start S-12  ))
  (connect (end   S-10 0) (end   1-1   ))
  (connect (end   S-10 1) (end   S-16 0))
  (connect (start S-16  ) (end   U-7   ))
  (connect (end   S-16 1) (start 2-8   ))
  (connect (end   S-12 1) (start 2-3   ))
  (connect (end   S-12 0) (start U-4   ))
  (connect (end   2-4   ) (start S-20  ))
  (connect (end   S-20 0) (start 1-5   ))
  (connect (end   1-5   ) (start 1-4   ))
  (connect (end   1-4   ) (start S-26  ))
  (connect (start 1-1   ) (start S-28  ))
  (connect (end   S-28 0) (end   1-7   ))
  (connect (end   S-28 1) (end   U-1   ))
  (connect (start 1-7   ) (end   1-6   ))
  (connect (end   S-20 1) (end   S-6  1))
  (connect (end   2-3   ) (end   S-6  0))
  (connect (start S-6   ) (start S-5   ))
  (connect (end   S-5  0) (start 1-6   ))
  (connect (end   S-5  1) (start S-7   ))
  (connect (end   S-7  0) (start U-5   ))
  (connect (end   S-7  1) (end   S-2-3 0))
  (connect (start U-6   ) (start S-2-3 ))
  (connect (start 2-2   ) (end   S-2-3 1))
  (connect (start S-8   ) (end   S-2-3 2))
  (connect (end   U-6   ) (start S-1    ))
  (connect (end   S-1  0) (start 2-1    ))
  (connect (end   S-1  1) (end   S-25  1))
  (connect (end   U-5   ) (end   S-25  0))
  (connect (start S-25  ) (start 1-8    ))
  (connect (end   S-8  0) (start 2-5    ))
  (connect (end   S-8  1) (start S-4    ))
  (connect (end   S-4  1) (start 2-7    ))
  (connect (end   S-4  0) (start 2-6    ))
  
  
  ;; S-2 and S-3 are bundled into one switch S-2-3 with 3 positions in our implementation
  (define S-2-position 0)
  (define S-3-position 0)
  
  (define (update-switch-2-3)
    (if (= S-2-position 0)
        (send S-2-3 set-switch-position 0)
        (if (= S-3-position 0)
            (send S-2-3 set-switch-position 1)
            (send S-2-3 set-switch-position 2))))
  
  
  (define hw-railway%
    (class railway%
      (inherit-field segments switches detection-blocks)
      (super-new)
      (define/override (set-switch-position! id position)
        (cond ((eq? id 'S-2)
               (set! S-2-position (- position 1))
               (update-switch-2-3))
              ((eq? id 'S-3)
               (set! S-3-position (- position 1))
               (update-switch-2-3))
              (else (super set-switch-position! id position))))
      
      (define/override (get-switch-position id)
        (cond ((eq? id 'S-2)
               (+ S-2-position 1))
              ((eq? id 'S-3)
               (+ S-3-position 1))
              (else (super get-switch-position id))))))
  
  (define SEGMENTS
    (list 1-1  1-2   1-3  1-4  1-5  1-6  1-7  1-8
          2-1  2-2   2-3  2-4  2-5  2-6  2-7  2-8
          U-1  U-2   U-3  U-4  U-5  U-6  U-7
          S-1  S-2-3      S-4  S-5  S-6  S-7  S-8  S-9 S-10 S-11 S-12 S-16
          S-20       S-23 S-24 S-25 S-26 S-27 S-28))
  
  (define SWITCHES
    (list S-1  S-2-3      S-4  S-5  S-6  S-7  S-8  S-9 S-10 S-11 S-12 S-16
          S-20       S-23 S-24 S-25 S-26 S-27 S-28))
  
  (define DETECTION-BLOCKS
    (list 1-1  1-2   1-3  1-4  1-5  1-6  1-7  1-8
          2-1  2-2   2-3  2-4  2-5  2-6  2-7  2-8))
  
  (for-each (lambda (block)
              (send block set-color 'blue))
            DETECTION-BLOCKS)
  
  (define UNDEFINED-BLOCKS
    (list U-1  U-2   U-3  U-4  U-5  U-6  U-7))
  
  (set! RAILWAY (make-object hw-railway% SEGMENTS SWITCHES DETECTION-BLOCKS)))


;; ************************ ;;
;;      SETUP STRAIGHT      ;;
;; ************************ ;;

(define (setup-straight)
  (define D1 (make-object block% 'D1 (list G231 G231)))
  (define D2 (make-object block% 'D2 (list G231 G231)))
  (define T  (make-object block% 'T  (list G231 G231)))
  (define D3 (make-object block% 'D3 (list G231 G231)))
  (define D4 (make-object block% 'D4 (list G231 G231)))
  (connect (end D1) (start D2))
  (connect (end D2) (start T ))
  (connect (end T ) (start D3))
  (connect (end D3) (start D4))

  (define SEGMENTS (list D1 D2 T D3 D4))
  (define SWITCHES '())
  (define DETECTION-BLOCKS (list D1 D2 D3 D4))
  
  (for-each (lambda (block)
              (send block set-color 'blue))
            DETECTION-BLOCKS)
  
  (set! RAILWAY (make-object railway% SEGMENTS SWITCHES DETECTION-BLOCKS)))

;; *********************************** ;;
;;      SETUP STRAIGHT WITH SWITCH     ;;
;; *********************************** ;;

(define (setup-straight-with-switch)
  (define D1 (make-object block%  'D1 (list G231 G231)))
  (define S1 (make-object switch% 'S1 WR))
  (define D4 (make-object block%  'D4 (list G231 G231)))
  (define D5 (make-object block%  'D5 (list G231 G231)))
  (define D6 (make-object block%  'D6 (list G231 R9)))
  (define D7 (make-object block%  'D7 (list G231 G231)))
  (connect (end   D1  ) (start S1))
  (connect (end   S1 0) (start D4))
  (connect (end   D4  ) (start D5))
  (connect (end   S1 1) (end   D6))
  (connect (start D6  ) (start D7))

  (define SEGMENTS (list D1 S1 D4 D5 D6 D7))
  (define SWITCHES (list S1))
  (define DETECTION-BLOCKS (list D1 D4 D5 D6 D7))
  
  (for-each (lambda (block)
              (send block set-color 'blue))
            DETECTION-BLOCKS)
  
  (set! RAILWAY (make-object railway% SEGMENTS SWITCHES DETECTION-BLOCKS)))

;; ******************** ;;
;;      SETUP LOOP      ;;
;; ******************** ;;

(define (setup-loop)
  (define D1 (make-object block% 'D1 (list R2 R2 R2)))
  (define D2 (make-object block% 'D2 (list G231 G231 G231)))
  (define D3 (make-object block% 'D3 (list R2 R2 R2)))
  (define D4 (make-object block% 'D4 (list G231 G231)))
  (define D5 (make-object block% 'D5 (list R2 R2 R2)))
  (define T1 (make-object block% 'T1 G231))
  (define D6 (make-object block% 'D6 G231))
  (define D7 (make-object block% 'D7 G231))
  (define D8 (make-object block% 'D8 (list R2 R2 R2)))
  (define T2 (make-object block% 'T2 (list G231 G231)))
  
  (connect (end D1) (start D2))
  (connect (end D2) (start D3))
  (connect (end D3) (start D4))
  (connect (end D4) (start D5))
  (connect (end D5) (start T1))
  (connect (end T1) (start D6))
  (connect (end D6) (start D7))
  (connect (end D7) (start D8))
  (connect (end D8) (start T2))
  (connect (end T2) (start D1))

  (define SEGMENTS (list T1 D6 D7 D8 T2 D1 D2 D3 D4 D5))
  (define SWITCHES '())
  (define DETECTION-BLOCKS (list D1 D2 D3 D4 D5 D6 D7 D8))
  
  (for-each (lambda (block)
              (send block set-color 'blue))
            DETECTION-BLOCKS)
  
  (set! RAILWAY (make-object railway% SEGMENTS SWITCHES DETECTION-BLOCKS)))

;; ********************************* ;;
;;      SETUP LOOP AND SWITCHES      ;;
;; ********************************* ;;

(define (setup-loop-and-switches)
  (define D1 (make-object block%  'D1 (list R2 R2 R2)))
  (define D2 (make-object block%  'D2 (list G231 G231 G231)))
  (define D3 (make-object block%  'D3 (list R2 R2 R2)))
  (define D4 (make-object block%  'D4 G231))
  (define S1 (make-object switch% 'S1 BWR))
  (define U1 (make-object block%  'U1 (list R2 R2)))
  (define U2 (make-object block%  'U2 (list R2 R2)))
  (define T1 (make-object block%  'T1 G231))
  (define D5 (make-object block%  'D5 G231))
  (define D6 (make-object block%  'D6 G231))
  (define S2 (make-object switch% 'S2 BWL))
  (define U3 (make-object block%  'U3 (list R2 R2)))
  (define U4 (make-object block%  'U4 (list R2 R2))) 
  (define T2 (make-object block%  'T2 G231))
  (define T3 (make-object block%  'T3 G231))
  (define S3 (make-object switch% 'S3 WL))
  (define D7 (make-object block%  'D7 G231))
  (define D8 (make-object block%  'D8 R9))
  (define D9 (make-object block%  'D9 G231))

  
  (connect (end   D1  ) (start D2  ))
  (connect (end   D2  ) (start D3  ))
  (connect (end   D3  ) (start D4  ))
  (connect (end   D4  ) (start S1  ))
  (connect (end   S1 0) (start U1  ))
  (connect (end   U1  ) (start T1  ))
  (connect (end   T1  ) (start D5  ))
  (connect (end   D5  ) (start D6  ))
  (connect (end   D6  ) (start U3  ))
  (connect (end   U3  ) (end   S2 0))
  (connect (start S2  ) (start T2  ))
  (connect (end   T2  ) (start D1  ))
  (connect (end   S1 1) (start U2  ))
  (connect (end   U2  ) (start T3  ))
  (connect (end   T3  ) (start S3  ))
  (connect (end   S3 0) (start D7  ))
  (connect (end   D7  ) (start U4  ))
  (connect (end   U4  ) (end   S2 1))
  (connect (end   S3 1) (start D8  ))
  (connect (end   D8  ) (start D9  ))


  (define SEGMENTS (list T1 D5 D6 U3 S2 T2 D1 D2 D3 D4 S1 U1 U2 T3 S3 D7 U4 D8 D9))
  (define SWITCHES (list S1 S2 S3))
  (define DETECTION-BLOCKS (list D1 D2 D3 D4 D5 D6 D7 D8 D9))
  
  (for-each (lambda (block)
              (send block set-color 'blue))
            DETECTION-BLOCKS)
  
  (set! RAILWAY (make-object railway% SEGMENTS SWITCHES DETECTION-BLOCKS)))
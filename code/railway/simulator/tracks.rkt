#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;
;       tracks.rkt
;
;       Internal representation of tracks, blocks and switches
;

(require racket/class)

(provide
 ;; TRACKS
 straight-track% curved-track% switch-track%
 ;; BLOCKS
 block% switch%
 ;; CONNECTIONS
 invert reverse-indeces connect end start)

(define (remainder-2pi angle)
  (let ((quotient (truncate (/ angle (* 2 pi)))))
    (- angle (* quotient (* 2 pi)))))

;; **************** ;;
;;      TRACKS      ;;
;; **************** ;;

(define straight-track%
  (class object%
    (super-new)
    (init-field length)
    (define/public (build-gui-track)
      (make-object gui-straight-track% length))))

(define curved-track%
  (class object%
    (super-new)
    (init-field radius angle [inverted #f])
    (define/public (build-gui-track)
      (make-object gui-curved-track% radius angle inverted))))

(define (invert curved-track)
  (make-object curved-track%
    (get-field radius curved-track)
    (get-field angle  curved-track)
    (not (get-field inverted curved-track))))

(define switch-track%
  (class object%
    (super-new)
    (init-field tracks)))

(define (reverse-indeces switch-track)
  (make-object switch-track%
    (reverse (get-field tracks switch-track))))


(define gui-straight-track%
  (class object%
    (super-new)
    (init-field length)
    (field [x 'uninitialized]
           [y 'uninitialized]
           [x2 'uninitialized]
           [y2 'uninitialized]
           [start-orientation 'uninitialized]
           [end-orientation 'uninitialized])
    
    (define (build x y orientation)
      (values x
              y
              (remainder-2pi (+ pi orientation))
              (+ x (* length (cos orientation)))
              (- y (* length (sin orientation)))
              orientation))
    
    (define/public (build-from-start-point xp yp orientation)
      (set!-values (x y start-orientation x2 y2 end-orientation) (build xp yp orientation)))
    
    (define/public (build-from-end-point xp yp orientation)
      (set!-values (x2 y2 end-orientation x y start-orientation) (build xp yp orientation)))
    
    (define/public (start-point-orientation)
      (list x y start-orientation))
    
    (define/public (end-point-orientation)
      (list x2 y2 end-orientation))
    
    (define/public (move-along-track distance from-start?)
      (if (> distance length)
          (- distance length)
          (if from-start?
              (cons (+ x (* distance (cos end-orientation)))
                    (- y (* distance (sin end-orientation))))
              (cons (+ x2 (* distance (cos start-orientation)))
                    (- y2 (* distance (sin start-orientation)))))))
    
    (define/public (get-length)
      length)
    
    (define/public (draw window dc-id pen-id)
      (send window draw-line dc-id pen-id x y x2 y2))))


(define gui-curved-track%
  (class object%
    (super-new)
    (init-field radius angle inverted)
    (field [x 'uninitialized]
           [y 'uninitialized]
           [x2 'uninitialized]
           [y2 'uninitialized]
           [mx 'uninitialized]
           [my 'uninitialized]
           [start-orientation 'uninitialized]
           [end-orientation 'uninitialized]
           [arc 'uninitialized])
    
    (define (build x y orientation angle)
      (let* ((mx (+ x (* radius (cos (- (/ pi 2) orientation)))))
             (my (+ y (* radius (sin (- (/ pi 2) orientation)))))
             (end-radians   (+ (/ pi 2) orientation))
             (start-radians (- end-radians angle))
             (x2 (- mx (* radius (cos (+ (- (/ pi 2) orientation) angle)))))
             (y2 (- my (* radius (sin (+ (- (/ pi 2) orientation) angle))))))
        (values x
                y
                (remainder-2pi (+ pi orientation))
                x2
                y2
                (remainder-2pi (+ (- orientation angle) (* 2 pi)))
                mx
                my
                (list (- mx radius) (- my radius) (* 2 radius) (* 2 radius) start-radians end-radians))))
    
    (define (build-inverted x y orientation angle)
      (let* ((mx (- x (* radius (cos (- (/ pi 2) orientation)))))
             (my (- y (* radius (sin (- (/ pi 2) orientation)))))
             (start-radians (+ (/ (* 3 pi) 2) orientation))
             (end-radians (+ start-radians angle))
             (x2 (+ mx (* radius (cos (+ (+ (/ (* 3 pi) 2) orientation) angle)))))
             (y2 (- my (* radius (sin (+ (+ (/ (* 3 pi) 2) orientation) angle))))))
        (values x
                y
                (remainder-2pi (+ pi orientation))
                x2
                y2
                (remainder-2pi (+ orientation angle))
                mx
                my
                (list (- mx radius) (- my radius) (* 2 radius) (* 2 radius) start-radians end-radians))))
    
    (define/public (build-from-start-point xp yp orientation)
      (if (not inverted)
          (set!-values (x y start-orientation x2 y2 end-orientation mx my arc) (build xp yp orientation angle))
          (set!-values (x y start-orientation x2 y2 end-orientation mx my arc) (build-inverted xp yp orientation angle))))
    
    (define/public (build-from-end-point xp yp orientation)
      (if (not inverted)
          (set!-values (x2 y2 end-orientation x y start-orientation mx my arc) (build-inverted xp yp orientation angle))
          (set!-values (x2 y2 end-orientation x y start-orientation mx my arc) (build xp yp orientation angle))))
    
    (define/public (start-point-orientation)
      (list x y start-orientation))
    
    (define/public (end-point-orientation)
      (list x2 y2 end-orientation))
    
    (define (move-from-start distance)
      (let* ((angle (/ distance radius))
             (orientation (remainder-2pi (+ pi (if inverted end-orientation start-orientation))))
             (cx (+ mx (* radius (cos (- (+ (/ pi 2) orientation) angle)))))
             (cy (- my (* radius (sin (- (+ (/ pi 2) orientation) angle))))))
        (cons cx cy)))
    
    (define (move-from-end distance)
      (let* ((angle (/ distance radius))
             (orientation (remainder-2pi (+ pi (if inverted start-orientation end-orientation))))
             (cx (+ mx (* radius (cos (+ (+ (/ (* 3 pi) 2) orientation) angle)))))
             (cy (- my (* radius (sin (+ (+ (/ (* 3 pi) 2) orientation) angle))))))
        (cons cx cy)))
    
    (define/public (move-along-track distance from-start?)
      (if (> distance (* radius angle))
          (- distance (* radius angle))
          (if (xor from-start? inverted)
              (move-from-start distance)
              (move-from-end distance))))
    
    (define/public (get-length)
      (* radius angle))
    
    (define/public (draw window dc-id pen-id)
      (send/apply window draw-arc (cons dc-id (cons pen-id arc))))))


(define gui-section%
  (class object%
    (super-new)
    (init-field tracks)
    
    (define gui-tracks
      (map (lambda (track)
             (send track build-gui-track))
           tracks))
    
    (define/public (build-from-start-point x y orientation)
      (let loop ((tracks gui-tracks)
                 (start-point (list x y orientation)))
        (when (not (null? tracks))
          (send/apply (car tracks) build-from-start-point start-point)
          (loop (cdr tracks) (send (car tracks) end-point-orientation)))))
    
    (define/public (build-from-end-point x y orientation)
      (let loop ((tracks (reverse gui-tracks))
                 (end-point (list x y orientation)))
        (when (not (null? tracks))
          (send/apply (car tracks) build-from-end-point end-point)
          (loop (cdr tracks) (send (car tracks) start-point-orientation)))))
    
    (define/public (start-point-orientation)
      (send (car gui-tracks) start-point-orientation))
    
    (define/public (end-point-orientation)
      (send (last gui-tracks) end-point-orientation))
    
    (define/public (move-along-track distance from-start?)
      (let loop ((tracks (if from-start? gui-tracks (reverse gui-tracks)))
                 (d distance))
        (let ((p-or-d (send (car tracks) move-along-track d from-start?)))
          (if (or (pair? p-or-d) (null? (cdr tracks)))
              p-or-d
              (loop (cdr tracks) p-or-d)))))
    
    (define/public (get-length)
      (foldl
       (lambda (track result)
         (+ result (send track get-length)))
       0
       gui-tracks))
    
    (define/public (draw . context)
      (for-each (lambda (track)
                  (send/apply track draw context))
                gui-tracks))))

;; ****************** ;;
;;      SEGMENTS      ;;
;; ****************** ;;

(define block%
  (class object%
    (super-new)
    (init-field id tracks)
    (field [start-connection 'uninitialized]
           [end-connection 'uninitialized]
           [color 'black])
    
    (define gui-track
      (if (pair? tracks)
          (make-object gui-section% tracks)
          (send tracks build-gui-track)))
    
    (define visited #f)
    
    (define/public (build-gui-track from x y orientation)
      (when (not visited)
        (set! visited #t)
        (when (eq? from start-connection)
          (send gui-track build-from-start-point x y orientation)
          (when (not (eq? end-connection 'uninitialized))
            (send/apply end-connection build-gui-track (cons this (send gui-track end-point-orientation)))))
        (when (eq? from end-connection)
          (send gui-track build-from-end-point x y orientation)
          (when (not (eq? start-connection 'uninitialized))
            (send/apply start-connection build-gui-track (cons this (send gui-track start-point-orientation)))))))
    
    (define/public (initiate-build x y orientation)
      (set! visited #t)
      (send gui-track build-from-start-point x y orientation)
      (when (not (eq? end-connection 'uninitialized))
        (send/apply end-connection build-gui-track (cons this (send gui-track end-point-orientation))))
      (when (not (eq? start-connection 'uninitialized))
        (send/apply start-connection build-gui-track (cons this (send gui-track start-point-orientation)))))
    
    (define/public (start-connector)
      (cons this
            (lambda (track)
              (set! start-connection track))))
    
    (define/public (end-connector)
      (cons this
            (lambda (track)
              (set! end-connection track))))
    
    (define/public (move-along-track from distance)
      (if (eq? from start-connection)
          (let ((p-or-d (send gui-track move-along-track distance #t)))
            (if (pair? p-or-d)
                (values this from distance p-or-d)
                (if (not (eq? end-connection 'uninitialized))
                    (send end-connection move-along-track this p-or-d)
                    (error "Train Crash"))))
          (let ((p-or-d (send gui-track move-along-track distance #f)))
            (if (pair? p-or-d)
                (values this from distance p-or-d)
                (if (not (eq? start-connection 'uninitialized))
                    (send start-connection move-along-track this p-or-d)
                    (error "Train Crash"))))))
    
    (define/public (get-next-track from)
      (if (eq? from start-connection)
          end-connection
          start-connection))
    
    (define/public (get-length from)
      (send gui-track get-length))
    
    (define/public (set-color new-color)
      (set! color new-color))
    
    (define/public (draw . context)
      (send/apply gui-track draw (append context (list color))))))

(define (index-of el v)
  (let loop ((i 0))
    (cond ((>= i (vector-length v)) #f)
          ((eq? el (vector-ref v i)) i)
          (else (loop (+ i 1))))))


(define switch%
  (class object%
    (super-new)
    (init-field id switch-track)
    (field [start-connection 'uninitialized]
           [end-connections  (make-vector (length (get-field tracks switch-track)) 'uninitialized)]
           [active-color 'green]
           [inactive-color 'red])
    
    (define switch-position 0)
    
    (define gui-tracks
      (map (lambda (tracks)
             (if (pair? tracks)
                 (make-object gui-section% tracks)
                 (send tracks build-gui-track)))
           (get-field tracks switch-track)))
    
    (define visited #f)
    
    (define/public (build-gui-track from x y orientation)
      (when (not visited)
        (set! visited #t)
        (if (eq? from start-connection)
            (let loop ((tracks gui-tracks)
                       (i 0))
              (when (not (null? tracks))
                (let ((track (car tracks))
                      (end-connection (vector-ref end-connections i)))
                  (send track build-from-start-point x y orientation)
                  (when (not (eq? end-connection 'uninitialized))
                    (send/apply end-connection build-gui-track (cons this (send track end-point-orientation)))))
                (loop (cdr tracks) (+ i 1))))
            (let* ((i (index-of from end-connections))
                   (track (list-ref gui-tracks i)))
              (send track build-from-end-point x y orientation)
              (let* ((start-point-orientation (send track start-point-orientation))
                     (inverse-start-point-orientation (list (car start-point-orientation) (cadr start-point-orientation) (+ pi (caddr start-point-orientation)))))
                (when (not (eq? start-connection 'uninitialized))
                  (send/apply start-connection build-gui-track (cons this start-point-orientation)))
                (let loop ((tracks gui-tracks)
                           (j 0))
                  (when (not (null? tracks))
                    (when (not (= i j))
                      (let ((track (car tracks))
                            (end-connection (vector-ref end-connections j)))
                        (send/apply track build-from-start-point inverse-start-point-orientation)
                        (when (not (eq? end-connection 'uninitialized))
                          (send/apply end-connection build-gui-track (cons this (send track end-point-orientation))))))
                    (loop (cdr tracks) (+ j 1)))))))))
    
    (define/public (start-connector)
      (cons this
            (lambda (track)
              (set! start-connection track))))
    
    (define/public (end-connector track-index)
      (cons this
            (lambda (track)
              (vector-set! end-connections track-index track))))
    
    (define/public (move-along-track from distance)
      (if (eq? from start-connection)
          (let* ((track (list-ref gui-tracks switch-position))
                 (end-connection (vector-ref end-connections switch-position))
                 (p-or-d (send track move-along-track distance #t)))
            (if (pair? p-or-d)
                (values this from distance p-or-d)
                (if (not (eq? end-connection 'uninitialized))
                    (send end-connection move-along-track this p-or-d)
                    (error "Train Crash"))))
          (let* ((i (index-of from end-connections))
                 (track (list-ref gui-tracks i))
                 (p-or-d (send track move-along-track distance #f)))
            (if (pair? p-or-d)
                (values this from distance p-or-d)
                (if (not (eq? start-connection 'uninitialized))
                    (send start-connection move-along-track this p-or-d)
                    (error "Train Crash"))))))
    
    (define/public (get-next-track from)
      (if (eq? from start-connection)
          (vector-ref end-connections switch-position)
          start-connection))
    
    (define/public (get-length from)
      (if (eq? from start-connection)
          (send (list-ref gui-tracks switch-position) get-length)
          (let* ((i (index-of from end-connections))
                 (track (list-ref gui-tracks i)))
            (send track get-length))))
    
    (define/public (set-switch-position position)
      (set! switch-position position))
    
    (define/public (get-switch-position)
      switch-position)
    
    (define/public (set-colors active inactive)
      (set! active-color active)
      (set! inactive-color inactive))
    
    (define/public (draw . context)
      (let loop ((l gui-tracks)
                 (i 0))
        (when (not (null? l))
          (if (= i switch-position)
              (send/apply (car l) draw (append context (list active-color)))
              (send/apply (car l) draw (append context (list inactive-color))))
          (loop (cdr l) (+ i 1)))))))

(define (start block)
  (send block start-connector))

(define (end block . index)
  (send/apply block end-connector index))

(define (connect connector-1 connector-2)
  (let ((track-1 (car connector-1))
        (track-2 (car connector-2)))
    ((cdr connector-1) track-2)
    ((cdr connector-2) track-1)))


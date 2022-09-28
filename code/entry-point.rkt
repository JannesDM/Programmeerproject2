#lang racket

;; --------------------------------------------
;; Jannes De Mets    
;; ---------------
;; Entry point
;; --------------------------------------------

; Imports
(require (prefix-in NMBS:  "nmbs/nmbs.rkt"))
(require (prefix-in INFRA: "infrabel.rkt"))
(require "helpers.rkt")

;; Entry point

; Variables
(define tcp-port 8080)
(define simulation? #t)
; width - height
(define ui-size (make-ui-size 500 620))

; define Infrabel
(define INFRABEL (INFRA:infrabel tcp-port))

; define NMBS
(define NMBS (NMBS:nmbs tcp-port ui-size))

;; Config, defines
; setup simulation, hardware
(INFRABEL 'setup-railway! simulation?)

;; GUI
; start in thread
(define NMBS-UI (thread (lambda () (NMBS 'start-ui))))

;; End
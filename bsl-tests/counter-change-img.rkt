;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname counter-change-img) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

;; from Bloch, Picturing Programs
; counter-change :  image image -> image
; Produces a square arrangement with the top-left image also
; in the bottom right, and the top-right image also in the
; bottom left.

(define man (bitmap/url "http://picturingprograms.com/pictures/stick-figure.png"))
(define calendar (bitmap/url "http://picturingprograms.com/pictures/calendar.png"))

(check-expect (counter-change man calendar)
              (above (beside man calendar)
                     (beside calendar man)))

(define (counter-change top-left top-right)
  (above (beside top-left top-right)
         (beside top-right top-left)))


(require racketui)

#;
(web-launch
 "Criss-Cross"
 (function "Produces a criss-crossed arrangement of two pictures you select."
           (counter-change ["Top left picture" image]
                           ["Top right picture" image]
                           -> ["Criss-crossed combination" image])))

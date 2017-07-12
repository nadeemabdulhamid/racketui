;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname clock-img) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

;; max-dimension : image -> number
;; produces the larger value of the width or length of the given image
(define (max-dimension background)
  (max (image-width background) (image-height background)))

;; minutes->angle : number -> number
;; given minutes value, produces the proper angle of the minutes hand
(define (minutes->angle mins)
  (* -6 mins))

;; hour->angle : number number -> number
;; given hour and minutes, produces the proper angle of the hour hand
(define (hour->angle hour mins)
  (floor (* -30 (+ hour (/ mins 60)))))

;; make-12oclock-hand : number number number string boolean -> image
;; produces an image of a clock hand pointing straight up (12:00 position)
;; with given proportions of arm and tip length with respect to the radius,
;; and with given color
(define (make-12oclock-hand radius arm-prop tip-prop color frame?)
   (place-image/align 
    (above (isosceles-triangle (* tip-prop radius) 50 "solid" color)
           (isosceles-triangle (* arm-prop radius) 350 "solid" color))
    radius radius "middle" "bottom" 
    (circle radius "outline" (if frame? "black" (make-color 0 0 0 0)))))

;; make-12oclock-big-hand : image string boolean -> image
;; produces an image of a long clock hand pointing straight up, scaled
;; to the size of the given background
(define (make-12oclock-big-hand background color frame?)
  (make-12oclock-hand (/ (max-dimension background) 2) .8 .16 color frame?))

;; make-12oclock-small-hand : image string boolean -> image
;; produces an image of a small clock hand pointing straight up, scaled
;; to the size of the given background
(define (make-12oclock-small-hand background color frame?)
  (make-12oclock-hand (/ (max-dimension background) 2) .4 .08 color frame?))

;; time-clock : image number number string string boolean -> image
;; produces a clock with hands pointing appropriately for the given
;; hour and minutes, on the given background image (should be round'ish)
(define (time-clock background hour minutes hour-color min-color frame?)
  (overlay/align "left" "middle"
                 (rotate (minutes->angle minutes) 
                         (make-12oclock-big-hand background min-color frame?))
                 (rotate (hour->angle hour minutes)
                         (make-12oclock-small-hand background hour-color frame?))
                 background))

;; ------------------

(require racketui)
(require racketui/common)

#;
(web-launch
 "Clock Image Generator"
 (function "Generates an image of a clock on a background of your choice (should be round'ish)."
           (time-clock ["Background image" image]
                       ["Hour" number]
                       ["Minutes" number]
                       ["Hour hand color" color/web]
                       ["Minute hand color" color/web]
                       ["Draw outside frame" boolean]
                       -> ["Clock image" image])))


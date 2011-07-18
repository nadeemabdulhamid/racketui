;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wgt-on-moon-simple) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


;; wgt-on-moon : number -> number
;; produces a person's weight on the moon, given their weight on earth

(check-expect (wgt-on-moon 12) 2)

(define (wgt-on-moon earth-wgt)
  (* 1/6 earth-wgt))




(require racket/base)
(require (planet nah22/racketui))
; (require (planet "main.rkt" ("nah22" "racketui.plt" 1 1)))


(web-launch 
 "Moon Weight Computer"
 (function "This program computes what your weight would
            be on the moon, given your Earth weight."
           (wgt-on-moon ["Earth Weight (lbs)" number] -> ["Moon Weight (lbs)" number])))


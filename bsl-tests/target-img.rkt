;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname target-img) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require 2htdp/image)


; A Target is either:
;   - a String, representing the color of a solid inner disk, or
;   - (make-ring String Ring), representing a ring of the given
;             color with one or more rings inside it
(define-struct ring (color inner))

(define BULLSEYE
  (make-ring "black"
             (make-ring "white"
                        (make-ring "blue"
                                   (make-ring "red" "yellow")))))
(define BULLSEYE/LIST
  (list "black" "white" "blue" "red" "yellow"))

(define RING-THICKNESS 10)


; depth : Target -> Number
; determines the maximum level of nested rings in the given target

(check-expect (depth "blue") 1)
(check-expect (depth BULLSEYE) 5)
(check-expect (depth (make-ring "green" BULLSEYE)) 6)

(define (depth a-target)
  (cond
    [(string? a-target) 1]
    [(ring? a-target)
     (+ 1 (depth (ring-inner a-target)))]))



; image-of : Target -> Image
; produces a image of the target

(check-expect (image-of "blue") (circle RING-THICKNESS "solid" "blue"))
(check-expect (image-of (make-ring "blue" (make-ring "red" "yellow")))
              (overlay (circle 10 "solid" "yellow")
                       (overlay (circle 20 "solid" "red")
                                (circle 30 "solid" "blue"))))
(check-expect (image-of (make-ring "black"
                                   (make-ring "blue" (make-ring "red" "yellow"))))
              (overlay (overlay (circle 10 "solid" "yellow")
                                (overlay (circle 20 "solid" "red")
                                         (circle 30 "solid" "blue")))
                       (circle 40 "solid" "black")))

(define (image-of a-target)
  (cond
    [(string? a-target) (circle RING-THICKNESS "solid" a-target)]
    [(ring? a-target)
     (overlay (image-of (ring-inner a-target))
              (circle (* RING-THICKNESS (depth a-target)) "solid" (ring-color a-target)))]))


;; list->target : NEListof String -> Target
(check-expect (list->target BULLSEYE/LIST) BULLSEYE)
(define (list->target a-los)
  (cond [(empty? (rest a-los)) (first a-los)]
        [else (make-ring (first a-los) (list->target (rest a-los)))]))


(define (image-of/list a-los)
  (image-of (list->target a-los)))



;; ----------------------------
(require (planet nah22/racketui))

(define/web color/web
  (oneof ["Black" (constant "black")]
         ["White" (constant "white")]
         ["Red" (constant "red")]
         ["Blue" (constant "blue")]))

(web-launch
 "Bulls Eye Generator"
 (function "Produces a bullseye target image with rings colored as you specify."
           (image-of/list ["Ring Colors" (listof+ ["Color" color/web])]
                          -> ["Target Image" image])))



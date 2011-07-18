;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dish-order-cost-struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


;A dish is (make-dish string number number number)
(define-struct dish (name price mintemp maxtemp))

(define dish1 (make-dish "Haggis" 12 100 150))
(define dish2 (make-dish "Gnocchi" 7 110 140))
(define dish3 (make-dish "Pie" 5 40 50))
(define dish4 (make-dish "Hummus" 5 60 80))

;; An order is (make-order dish dish dish)
(define-struct order (appetizer entree dessert))

(define order1 (make-order dish4 dish1 dish3))
(define order2 (make-order dish4 dish2 dish3))

#| TEMPLATE
(define (order-func an-order ...)
... (dish-func (order-appetizer an-order))...
... (dish-func (order-entree an-order)) ...
... (dish-func (order-dessert an-order)) ... 
|#

; order-cost : order -> number
; Takes and order and produces the total amount for the order including 18% gratuity.

(check-expect (order-cost order1) 25.96)
(check-expect (order-cost order2) 20.06)

(define (order-cost an-order)
(* 1.18 ( + (dish-price (order-appetizer an-order))
    (dish-price (order-entree an-order))
    (dish-price (order-dessert an-order)) )))





(require racket/base)
(require (planet nah22/racketui))
; (require (planet "main.rkt" ("nah22" "racketui.plt" 1 1)))


(define/web dish/web
  (structure make-dish
             ["Name" string+]
             ["Price" number]
             ["Minimum serving temperature" number]
             ["Maximum serving temperature" number]))

(define/web order/web
  (structure make-order
             ["Appetizer" dish/web]
             ["Entree" dish/web]
             ["Dessert" dish/web]))

(web-launch "Order Cost"
            (function "Computes the total cost of the order including gratuity"
                      (order-cost ["The order" order/web] -> ["Order total" number])))


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname total-sales-seats-matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;****************************************************************

;; Developing a web application for reserving seats in a performing
;; arts center. The center has 504 seats, arranged in rows with 42 seats
;; in each row. The price for seats in the first four rows (#1 - #168) 
;; is $200. The price for seats in the next four rows (#169 - #336) is
;; $100. The price for remaining seats is $75.

;; seat-price : Number -> Number
;; given seat number, produces the price of the seat

(check-expect (seat-price 4) 200)
(check-expect (seat-price 160) 200)
(check-expect (seat-price 168) 200)
(check-expect (seat-price 169) 100)
(check-expect (seat-price 200) 100)
(check-expect (seat-price 336) 100)
(check-expect (seat-price 337) 75)
(check-expect (seat-price 500) 75)

(define (seat-price num)
  (cond [(<= 1 num 168) 200]
        [(<= 169 num 336) 100]
        [(<= 337 num 504) 75]))


;; A SeatingChart is a List-of-list-of-seats


;; A Seat is (make-seat Number Boolean)
(define-struct seat (number reserved?))


;; A List-of-seats is
; empty
; (cons seat list-of-seats)

#|TEMPLATE
; los-func : list-of-seats ... -> ...
; purpose ...

(check-expect (los-func ...))


(define (los-func a-los ...)
  (cond [(empty? a-los) ...]
        [(cons? a-los) ... (seat-func (first a-los))
                       ... (los-func (rest a-los) ...)]
        )
  )
|#


;; A List-of-list-of-seats is either:
; empty
; (cons list-of-seats list-of-list-of-seats)

#|TEMPLATE
; lolos-func : list-of-list-of-seats ... -> ...
; purpose ...

(check-expect (lolos-func ...))

(define (lolos-func a-lolos ...)
  (cond [(empty? a-lolos) ...]
        [(cons? a-lolos) ... (los-func (first a-lolos))
                         ... (lolos-func (rest a-lolos) ...)]
        )
  )
|#

;; small example:
(define CHART1
  (list (list (make-seat 1 false) (make-seat 2 false) (make-seat 3 false))
        (list (make-seat 4 false) (make-seat 5 false) (make-seat 6 false) (make-seat 7 false))
        (list (make-seat 8 false) (make-seat 9 false))))
(define CHART2
  (list (list (make-seat 1 false) (make-seat 2 false) (make-seat 3 false))
        (list (make-seat 4 false) (make-seat 5 false) (make-seat 6 false))
        (list (make-seat 7 false) (make-seat 8 false) (make-seat 9 false))))
(define CHART3
  (list (list (make-seat 1 false) (make-seat 2 false) (make-seat 3 true))
        (list (make-seat 4 false) (make-seat 5 true) (make-seat 6 false) (make-seat 7 true))
        (list (make-seat 8 true) (make-seat 9 false))))

; available-seats : list-of-list-of-seats -> number
; consumes a seating chart and produces the number of unreserved seats

(check-expect (available-seats CHART1) 9)
(check-expect (available-seats CHART2) 9)
(check-expect (available-seats CHART3) 5)

(define (available-seats a-lolos)
  (cond [(empty? a-lolos) 0]
        [(cons? a-lolos) 
         (+ (number-open (first a-lolos))(available-seats (rest a-lolos)))]
  )
  )

; number-open : list-of-seats -> number
; produces the number of open seats

(check-expect (number-open (list (make-seat 1 false))) 1)
(check-expect (number-open (list (make-seat 1 true))) 0)
(check-expect (number-open (list (make-seat 1 false) (make-seat 2 false))) 2)
(check-expect (number-open (list (make-seat 1 false) (make-seat 2 true))) 1)


(define (number-open a-los)
  (cond [(empty? a-los) 0]
        [(cons? a-los) 
         (cond [(seat-reserved? (first a-los)) (number-open (rest a-los))]
               [else (+ 1 (number-open (rest a-los))) ])]
        )
  )

; total-sales : list-of-list-of-seats -> number
; to produce the total amount of income made from seat sales

(check-expect (total-sales CHART1) 0)
(check-expect (total-sales CHART3) 800)
(check-expect (total-sales (append CHART1 
                                 (list (list (make-seat 180 true) 
                                       (make-seat 500 true))))) 175)

(define (total-sales a-lolos)
  (cond [(empty? a-lolos) 0]
        [(cons? a-lolos) 
         (+ (row-profit (first a-lolos)) (total-sales (rest a-lolos)))]
        )
  )

; row-profit : list-of-seats -> number
; determine the profit for the row

(check-expect (row-profit (list (make-seat 1 false))) 0)
(check-expect (row-profit (list (make-seat 1 true))) 200)
(check-expect (row-profit (list (make-seat 1 true) 
                                (make-seat 169 true))) 300)
(check-expect (row-profit (list (make-seat 200 true) 
                                (make-seat 500 true))) 175)
(check-expect (row-profit (list (make-seat 200 false) 
                                (make-seat 500 true))) 75)


(define (row-profit a-los)
  (cond [(empty? a-los) 0]
        [(cons? a-los)
         (cond [(seat-reserved? (first a-los))
                (+ (seat-price (seat-number (first a-los)))
                   (row-profit (rest a-los)))]
               [else (row-profit (rest a-los))])]
        )
  )


(require racket/base)
(require (planet nah22/racketui/web-launch))

(web-launch
 "Total Sales"
 (function "Computer total amount of income made from seat sales"
           (total-sales ["Stadium"
                         (listof ["Row" 
                                  (listof ["Seat" 
                                           (structure make-seat 
                                                      ["Seat number" number]
                                                      ["Reserved?" boolean])])])]
                        -> ["Total sales" number])))


                                            


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname all-bigger-2list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

; all-bigger? : List-of-Numbers List-of-Numbers -> Boolean
; purpose to determine whether each component in the first list is bigger than each component in the second
(check-expect (all-bigger? empty (list 0 1)) true)
(check-expect (all-bigger? (list 1 2 3) empty) true)
(check-expect (all-bigger? (list 1 2 3) (list 0 1)) false)
(check-expect (all-bigger? (list 2 3 4) (list 0 1)) true)

(define (all-bigger? lon1 lon2)
  (cond [(empty? lon1) true]
        [(cons? lon1) (and (>all? (first lon1) lon2)
                      (all-bigger? (rest lon1) lon2))]))


; >all? : Number List-of-Numbers -> Boolean
; to determine if a number is larger than all the numbers in a list

(define (>all? num a-lon)
  (cond [(empty? a-lon) true]
        [(cons? a-lon) (cond [(> num (first a-lon)) (>all? num (rest a-lon))]
                             [else false])]
        )
  )

(require racket/base)
(require (planet nah22/racketui))
; (require (planet "main.rkt" ("nah22" "racketui.plt" 1 1)))

(web-launch
 "All Bigger?"
 (function "Checks if every number in the first list is bigger than all numbers in the second."
           (all-bigger? ["First List" (listof ["Number" number])]
                        ["Second List" (listof ["Number" number])]
                        ->
                        ["All bigger?" boolean])))

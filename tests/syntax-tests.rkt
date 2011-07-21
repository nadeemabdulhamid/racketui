#lang racket

(require "../tfield.rkt")
(require "../syntax.rkt")
(require test-engine/racket-tests)


(define (fah->cel f x) (let ([r (* 5/9 f)]) (if x (round r) r)))

(define-struct boa (name age food) #:transparent)
(define-struct dillo (name age) #:transparent)
(define-struct posn (x y) #:transparent)

(check-expect (web-spec number) 'number)
(check-expect (web-spec string) 'string)
(check-expect (web-spec string+) 'string+)
(check-expect (web-spec boolean) 'boolean)
(check-expect (web-spec filename) 'filename)
(check-expect
 (rest (rest    ; check-expect doesn't compare procedures (make-boa)
        (web-spec (structure make-boa
                             ["Name" string]
                             ["Length" number]
                             ["Favorite foods" 
                              (listof ["Food" string])]))))
 `(["Name" string] ["Length" number] 
                   ["Favorite foods" (listof ["Food" string])]))
(check-expect
 (web-spec
  (oneof ["Grade value" number]
         ["Unrecorded" boolean]
         ["Retakes" (listof ["Grade" number])]
         ["Pass" symbol]))
 '(oneof ["Grade value" number]
         ["Unrecorded" boolean]
         ["Retakes" (listof ["Grade" number])]
         ["Pass" symbol]))


(define/web boa/web
  (structure make-boa
             ["Name" string]
             ["Length" number]
             ["Favorite foods" 
              (listof ["Food" string])]))

(define/web dillo/web+lab
  "Armadillo"
  (structure make-dillo
             ["Name" string] ["Age" number]))

(define/web animal/web+lab
  "Animal"
  (oneof ["Boa constrictor" boa/web]
         dillo/web+lab
         ["Fish (of weight)" number]))

(define/web tempconv-func/web
  "Temperature Converter"
  (function "This program converts Fahrenheit to Celsius."
            (fah->cel ["Degrees in Fahrenheit" number] ["Integral only" boolean]
                      -> ["Degrees in Celsius" number])))

(check-expect (equal?
               tempconv-func/web
               `("Temperature Converter"
                 (function
                  "This program converts Fahrenheit to Celsius."
                  (,fah->cel
                   ("Degrees in Fahrenheit" number)
                   ("Integral only" boolean)
                   ->
                   ("Degrees in Celsius" number)))))
              #t)


(define tf/s+ ((parse/web-spec (web-spec string+)) "t"))
(check-expect tf/s+
              (new-tfield/string "t" #f #t #:name (tfield-name tf/s+)))
(define tf/f ((parse/web-spec (web-spec filename)) "data file"))
(check-expect tf/f
              (new-tfield/file "data file" #f #f #f #:name (tfield-name tf/f)))

(test)
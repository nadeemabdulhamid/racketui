;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood-test-2list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
 blood test
 given a list of 'normal ranges' and a list of patient readings
 produce a list of strings indicating "L" or "H" if patient reading
  is below or above corresponding 'normal' range
|#

;;=========================================================================
;; DATA DEFINITIONS/TEMPLATES

; A Range is (make-mrange String Number Number String)
(define-struct mrange (label low high units))



#| TEMPLATE
; range-func : Range ... -> ...
; purpose...
(check-expect (range-func ...) ...)
(define (range-func a-range)
   ... (range-label a-range) ...
   ... (range-low a-range) ...
   ... (range-high a-range) ...
   ... (range-units a-range) ...
  )
|#

; A Reading is a Number

; A ResultCode is one of the strings:
;   - "L"  (representing a low result)
;   - "N"  (representing a normal result)
;   - "H"  (representing a high result)

; A Result is (make-result String ResultCode)
(define-struct result (label code))


; A List-of-ranges is either:
 ;; ... FILL IN ...

; A List-of-readings is either:
 ;; ... FILL IN ... 


;;=========================================================================
;; EXAMPLES OF DATA

(define READINGS
  (list 1.9
        136
        48
        887
        33
        6.9
        0.9))

(define RANGES
  (list (make-mrange "ALB" 2.2 3.9 "g/dl")
        (make-mrange "ALKP" 23 212 "U/L")
        (make-mrange "ALT" 10 100 "U/L")
        (make-mrange "AMYL" 500 1500 "U/L")
        (make-mrange "BUN" 7 27 "mg/dl")
        (make-mrange "Ca" 7.9 12.0 "mg/dl")
        (make-mrange "CREA" 0.5 1.8 "mg/dl")))
        
(define RESULTS
  (list (make-result "ALB" "L")
        (make-result "ALKP" "N")
        (make-result "ALT" "N")
        (make-result "AMYL" "N")
        (make-result "BUN" "H")
        (make-result "Ca" "L")
        (make-result "CREA" "N")))


;;=========================================================================
;; FUNCTIONS

; readings->results : List-of-readings List-of-ranges -> List-of-strings
; given a list of ranges and readings, produces a list of ResultCodes
; assumes both lists are the same length

(check-expect (readings->results READINGS RANGES) RESULTS)

(define (readings->results a-lord a-lorg)
  (cond 
    [(empty? a-lorg) empty]
    [(cons? a-lorg)
     (cons (make-result (mrange-label (first a-lorg)) (compute-code (first a-lorg) (first a-lord)))
           (readings->results (rest a-lord) (rest a-lorg)))]))


; compute-code : Range Reading -> ReadingCode
; determine whether the reading was low, normal, or high with respect to the given range

(check-expect (compute-code (make-mrange "ALT" 10 100 "U/L") 67) "N")
(check-expect (compute-code (make-mrange "ALT" 10 100 "U/L") 6) "L")
(check-expect (compute-code (make-mrange "ALT" 10 100 "U/L") 167) "H")
(check-expect (compute-code (make-mrange "ALT" 10 100 "U/L") 100) "N")
                  
(define (compute-code a-range a-reading)
  (cond [(< a-reading (mrange-low a-range)) "L"]
        [(> a-reading (mrange-high a-range)) "H"]
        [else "N"]))
     




(require 2htdp/image)
(define BAR (add-line (add-line (empty-scene 200 10) 50 0 50 10 "black")
                      150 0 150 10 "black"))


; result-bars->image : List-of-images -> Image

(define (result-bars->image a-loi)
  (cond [(empty? a-loi) (empty-scene 0 0)]
        [(cons? a-loi) (above (first a-loi)
                              (result-bars->image (rest a-loi)))]))

; readings->image : 
(define (readings->image a-lorg a-lord)
  (result-bars->image (readings->result-bars a-lorg a-lord)))


; readings->result-bars : List-of-ranges List-of-readings -> List-of-images

(define (readings->result-bars a-lorg a-lord)
  (cond 
    [(empty? a-lorg) empty]
    [(cons? a-lorg)
     (cons (above/align "left"
                        (text (mrange-label (first a-lorg)) 10 "black")
                        (indicator-bar (first a-lorg) (first a-lord)))
           (readings->result-bars (rest a-lorg) (rest a-lord)))]))


;; indicator-bar : Range Reading -> Image
;; produce a bar representing where the reading falls with respect to the range

(define (indicator-bar a-range a-reading)
  (place-image (rectangle 5 10 "solid" 
                          (cond [(string=? "N" (compute-code a-range a-reading)) "blue"]
                                [(string=? "L" (compute-code a-range a-reading)) "red"]
                                [(string=? "H" (compute-code a-range a-reading)) "red"]))
               (+ 50 (* (- a-reading (mrange-low a-range))
                        (/ 100 (- (mrange-high a-range) (mrange-low a-range)))))
               5
               BAR))





(require racketui)

(define/web range/web
  (structure make-mrange ["Label" string] ["Low" number] ["High" number] ["Units" string]))

(define/web reading/web
  number)

(define/web result/web
  (structure make-result
             ["Label" string]
             ["Result Code" (oneof ["Low Result" (constant "L")] 
                                   ["Normal Result" (constant "N")] 
                                   ["High Result" (constant "H")])]))
#;
(web-launch
 "Blood Test Result Analyzer"
 (function "Analyzes blood test results"
           (readings->results ["List of readings" (listof ["Reading" reading/web])]
                              ["List of ranges" (listof ["Range" range/web])]
                              -> ["List of results" (listof ["Result" result/web])])))
 



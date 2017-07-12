;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades-average-list-mixed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; A Grade is either:
;;   - a Number, or
;;   - a re-take (make-retake Number Number)
;;   - empty
;;   - a String (excusal reason -- doesn't count)
;; interpretation: a number represents an actual grade result; 
;;                 empty represents a missing grade
(define-struct retake (fst snd))


;how-many-done: List-of-Grades -> Number 
;Calculates how many non-empty grades there are in a list

(check-expect (how-many-done (list "sick" empty empty "travel")) 2)
(check-expect (how-many-done (list 45 empty 40 empty 90)) 5)
(check-expect (how-many-done empty) 0)
(check-expect (how-many-done (list 60 70 (make-retake 40 50) "sick" 60 30)) 5)

(define (how-many-done alog)
  (cond
    [(empty? alog) 0]
    [(empty? (first alog)) (+ 1 (how-many-done (rest alog)))]
    ((string? (first alog)) (+ 0 (how-many-done (rest alog))))
    [else
     (+ 1 (how-many-done (rest alog)))]))

;summation: List-of-Grades -> Number 
;Calculates the sum of the log adding 0 for empty entries

(check-expect (summation (list 45 45 45 45)) 180)
(check-expect (summation (list empty 45 45 45)) 135)
(check-expect (summation (list empty 30 empty "sick" 70)) 100)
(check-expect (summation empty) 0)

(define (summation alog)
  (cond
    [(empty? alog) 0]
    [(cons? alog)
     (cond [(empty? (first alog)) (+ 0 (summation (rest alog)))]
           [(retake? (first alog)) (+ (max (retake-fst (first alog))
                                           (retake-snd (first alog)))
                                      (summation (rest alog)))]
           [(string? (first alog)) (summation (rest alog))]
           [else (+ (first alog) (summation (rest alog)))])]))

; average: List-of-Grades -> Number or "No Data"
; Produces the average of the given list of grades
; ignoring excusals, counting missing grades as 0,
; and taking into account the higher of two grades in 
; case of a retake.
; If there are no values at all to average, produces
; the string "No Data".

(check-expect (average (list 45 45 45 45 45 empty 45)) (/ (* 6 45) 7))
(check-expect (average (list 100 empty "sick" 50)) 50)
(check-expect (average empty) "No Data")
(check-expect (average (list empty empty "excused" empty)) 0)


(define (average alog) 
  (cond
    [(= (how-many-done alog) 0) "No Data"]
 [else (/ (summation alog) (how-many-done alog))]))
     


(require racketui)

(define/web grade/web
  (oneof ["Actual grade" number]
         ["Retake"  (structure make-retake
                       ["First attempt" number]
                       ["Second attempt" number])]
         ["Excused (reason)" string+]
         ["Missing" (constant empty)]))

#;
(web-launch "Grade Average Computer"
 (function "Computes the average of grades in the given list."
   (average ["Grades" (listof ["Grade Result" grade/web])]
            -> ["Final Grade" 
                (oneof ["Average grade" number]
                       ["No Data" (constant "No Data")])])))

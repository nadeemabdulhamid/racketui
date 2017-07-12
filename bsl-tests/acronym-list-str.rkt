;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname acronym-list-str) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; acronym : List-of-string -> String
;; use capitalized words in the given list of strings to produce an acronym
(check-expect (acronym (cons "United" (cons "States" (cons "of" (cons "America" empty)))))
              "USA")
(check-expect (acronym empty) "")
(check-expect (acronym (cons "Compact" (cons "Disk" empty)))
              "CD")

(define (acronym a-los)
  (cond [(empty? a-los) ""]
        [(cons? a-los) (cond 
                         [(string-upper-case? (string-ith (first a-los) 0))
                          (string-append (string-ith (first a-los) 0)
                                         (acronym (rest a-los)))]
                         [else (acronym (rest a-los))])]))

(require racketui)

#;
(web-launch
 "Acronym Builder"
 (function "Produces an acronym of the capitalized words in the given list."
           (acronym ["Words" (listof+ ["Word" string+])] -> ["The acronym" string])))



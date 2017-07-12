;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname longest-list-str) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(check-expect (longest (list "aaa" "b" "cccc" "dddddd" "e")) "dddddd")

(define (longest a-los)
  (cond
    [(empty? a-los) ""]
    [else (local [(define lrest (longest (rest a-los)))]
            (if (> (string-length (first a-los)) (string-length lrest))
                (first a-los)
                lrest))]))

(require racketui)

#;
(web-launch
 "Longest String"
 (function "Produces the longest string from the given list."
           (longest ["Text strings" (listof+ ["String" string])] 
                    -> ["The longest string" string])))





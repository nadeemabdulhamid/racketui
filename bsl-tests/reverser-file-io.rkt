;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname reverser-file-io) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require 2htdp/batch-io)
(require racket/string)  ; string-join

(require (planet "main.rkt" ("nah22" "racketui.plt" 1 2)))

; reverser : file-name number -> file-name
; produces the name of the output file in which all lines from 
; the given input file that are at least min-len characters
; have been reversed.

(define (reverser file-name min-len)
  (write-file
   (string-append file-name "-output.txt")
   (string-join
    (map (λ(ln) (if (>= (length (string->list ln)) min-len)
                    (list->string (reverse (string->list ln)))
                    ln))
         (read-lines file-name))
    "\n")))


(web-launch 
 "Data Reverser"
 (function "This program reverses all lines from a given input file that exceed a given number of characters. It writes the result to an output file."
           (reverser ["Input file" filename] ["Minimum line length" number]
                     -> ["Processed output file" filename])))
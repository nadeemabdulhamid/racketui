;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hello) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; hello : string -> string
;; produces a reply, given your name

(check-expect (hello "Nadeem") "Hello, Nadeem!")

(define (hello name)
  (string-append "Hello, " name "!"))




(require racketui)

#;
(web-launch
 "Hello, World!"
 (function "Type in your name to receive a warm greeting."
           (hello ["Name" string] -> ["" string])))

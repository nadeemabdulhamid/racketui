;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname caesar-file) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require racket/string)  ; string-join
(require 2htdp/batch-io)

;; shift : char number -> char
;; shifts the character by the key value
(check-expect (shift #\d 3) #\g)
(check-expect (shift #\N 6) #\T)
(check-expect (shift #\4 0) #\4)

(define (shift ch key)
  (integer->char (+ key (char->integer ch))))


;; encrypt : string number boolean -> string
;; encrypts the messsage using a caesar cipher with the given key
;; if upcase? is true, then convert all characters to uppercase
;; as well before encryption
(check-expect (encrypt "hello" 1 false) "ifmmp")
(check-expect (encrypt "Hello" 1 true) "IFMMP")

(define (encrypt msg key upcase?)
  (list->string
   (map (λ(c) (shift (if upcase? (char-upcase c) c) key))
        (string->list msg))))

;; encryptFile : string string number boolean -> string
;; opens the file with given name, encrypts every line 
;; in the file using a caesar cipher with the given key
;; (converting to uppercase if upcase?) and writes the
;; encrypted lines to an output file with given name.

(define (encryptFile in-file out-file key upcase?)
  (write-file 
   out-file
   (string-join (map (λ(ln) (encrypt ln key upcase?))
                     (read-lines in-file))
                "\n")))


(require (planet "main.rkt" ("nah22" "racketui.plt" 1 3)))

(web-launch
 "!Encryptor!"
 (function "Use this program to encrypt your most secret files."
           (encryptFile ["Input file" filename]
                        ["Output file name" string+]
                        ["Secret key" number]
                        ["Uppercase?" boolean]
                        -> ["Encrypted file" filename])))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname web-file-size-mixed-struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; A File is either:
;;   - a Text-File (make-text-file String Number)
;;   - an Image-File (make-image-file String Number Number)
;;   - a Sound-File (make-sound-file String Number)
(define-struct text-file (name lines))
(define-struct image-file (name width height color?))
(define-struct sound-file (name duration))

(define article (make-text-file "article.doc" 250))
(define pic (make-image-file "blue-car.jpg" 300 450 true))
(define beep (make-sound-file "beep.mp3" 7))

#|
;; file-func : File ... -> ...
;; purpose
(check-expect ...)

(define (file-func a-file ...)
   (cond
      [(text-file? a-file) ...]
      [(image-file? a-file) ...]
      [(sound-file? a-file) ...]
   ))

      [(text-file? a-file) ... (text-file-name a-file)
                           ... (text-file-lines a-file) ...]
      [(image-file? a-file) ... (image-file-name a-file)
                            ... (image-file-width a-file)
                            ... (image-file-height a-file)
                            ... (image-file-color? a-file) ...]
      [(sound-file? a-file) ... (sound-file-name a-file)
                            ... (sound-file-duration a-file) ...]
|#


;; file-size : File -> Number
;; compute the size of a file in bytes
(check-expect (file-size article) 15000)
(check-expect (file-size pic) 405000)
(check-expect (file-size beep) 210)

(define (file-size a-file)
   (cond
      [(text-file? a-file) (* 60 (text-file-lines a-file))]
      [(image-file? a-file)
       (cond
         [(image-file-color? a-file)
          (* 3 (image-file-width a-file) (image-file-height a-file))]
         [else (* (image-file-width a-file) (image-file-height a-file))])]
      [(sound-file? a-file) (* 30 (sound-file-duration a-file))]   ))



(require (planet nah22/racketui))


(define/web text-file/web
  (structure make-text-file 
             ["File name" string+] ["Number of lines" number]))
(define/web image-file/web
  (structure make-image-file
             ["File name" string+] ["Width (pixels)" number] 
             ["Height (pixels)" number] ["Full-color" boolean]))
(define/web sound-file/web
  (structure make-sound-file 
             ["File name" string+] ["Duration of play (secs)" number]))
(define/web media/web
  (oneof ["Text file" text-file/web]
         ["Image file" image-file/web]
         ["Sound file" sound-file/web]))

(web-launch "File Size Computer"
 (function "Computes the size of a web media file in bytes"
           (file-size ["Media file info" media/web]
                      -> ["Computed size in bytes" number])))


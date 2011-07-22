#lang setup/infotab

(define name "RacketUI")
(define version "0.5.2")
(define release-notes
  (list '(ul
          (li "Added documentation")
          (li "Now supports input/output files"))))
(define repositories
  (list "4.x"))
(define required-core-version
  "5.1.2")
(define blurb
  (list "Automated Web UI generator for student programs"))
(define scribblings '(("scribblings/racketui.scrbl" ())))
(define primary-file "main.rkt")
(define categories '(ui))
(define compile-omit-paths
  (list "tests"
        "bsl-tests"))

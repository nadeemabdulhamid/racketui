#lang setup/infotab

(define name "RacketUI")
(define version "0.5")
(define release-notes
  (list '(ul
          (li "First public release"))))
(define repositories
  (list "4.x"))
(define blurb
  (list "Automated Web UI generator for student programs"))
(define scribblings '(("scribblings/racketui.scrbl" ())))
(define primary-file "main.rkt")
(define categories '(ui))
(define compile-omit-paths
  (list "tests"
        "bsl-tests"))

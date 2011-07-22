#lang setup/infotab

(define name "RacketUI")
(define version "0.5.4")
(define release-notes
  (list '(ul
          (li "Interface updates"))))
(define repositories
  (list "4.x"))
(define required-core-version
  "5.1.2")
(define blurb
  (list "Automated Web UI generator for student programs"))
(define scribblings '(("scribblings/racketui.scrbl" (multi-page))))
(define primary-file "main.rkt")
(define categories '(ui))
(define compile-omit-paths
  (list "tests"
        "bsl-tests"))

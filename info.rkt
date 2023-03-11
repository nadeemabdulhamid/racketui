#lang setup/infotab

(define name "racketui")
(define version "0.7")
(define release-notes
  (list '(ul
          (li "Fix up tests for new pkgs server")
          (li "Initial support for image fields added (specified by URL)")
          (li "Documentation updates (added screenshots)")
          )))
(define repositories
  (list "4.x"))
(define required-core-version
  "5.2")
(define blurb
  (list "Automated Web UI generator for student programs"))
(define scribblings '(("scribblings/racketui.scrbl" (multi-page))))
(define primary-file "main.rkt")
(define categories '(ui))
(define compile-omit-paths
  (list "tests"
        "bsl-tests"))
(define deps '("base"
               "draw-lib"
               "htdp-lib"
               "srfi-lite-lib"
               "web-server-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))

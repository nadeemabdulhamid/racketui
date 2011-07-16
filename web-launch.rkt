#lang racket

(require "tfield.rkt" "syntax.rkt" "web.rkt")
(require racket/runtime-path
         web-server/servlet
         web-server/servlet-env
         web-server/managers/lru)

(define-runtime-path htdocs "./htdocs")


  

;; launch-web : tfield/function -> ...
(define (launch-web func/tf)
  (define crunched (match (crunch (tfield-label func/tf)) ["" "index"] [c c]))
  (serve/servlet (start func/tf)
                 #:extra-files-paths (list htdocs)
                 #:manager (make-threshold-LRU-manager 
                            expiration-handler (* 128 1024 1024))
                 #:servlet-path (format "/~a.rkt" crunched)))


;; user syntax
(define-syntax-rule (web-launch title tfield/func)
  (launch-web ((parse/web-spec (web-spec tfield/func)) title)))


;; ============================================================================


(provide web-launch
         define/web)




#lang racket

(require "tfield.rkt" "syntax.rkt" "web.rkt")
(require racket/runtime-path
         web-server/safety-limits
         web-server/servlet
         web-server/servlet-env
         web-server/managers/lru)
(require (for-syntax syntax/parse))

(define-runtime-path htdocs "./htdocs")

;; launch-web : tfield/function -> ...
(define (launch-web func/tf)
  (define crunched (match (crunch (tfield-label func/tf)) ["" "index"] [c c]))
  (serve/servlet (start func/tf)
                 #:extra-files-paths (list htdocs)
                 #:safety-limits (make-unlimited-safety-limits)
                 #:manager (make-threshold-LRU-manager 
                            expiration-handler (* 128 1024 1024))
                 #:servlet-path (format "/~a.rkt" crunched)))


;; user syntax
;(define-syntax-rule (web-launch title tfield/func)
;  (launch-web ((parse/web-spec (web-spec tfield/func)) title)))

(define-syntax (web-launch stx)
  (syntax-parse stx
    [(web-launch title tfield/func)
     #`(launch-web ((parse/web-spec (web-spec tfield/func)) title))]
    [(web-launch lab+spec)
     #`(launch-web ((parse/web-spec (second lab+spec)) (first lab+spec)))]))

                
;; ============================================================================


(provide web-launch
         define/web)




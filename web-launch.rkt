#lang racket

(require "tfield.rkt" "syntax.rkt")
(require web-server/servlet
         web-server/servlet-env
         web-server/managers/lru)


(require "tfield-render.rkt")
(require racket/runtime-path)
(define-runtime-path mod-dir ".")

;; launch-web : tfield/function -> ...
(define (launch-web func/tf)
  (serve/servlet
   (λ(req)
     (send/suspend/dispatch
      (λ(make-url)
        (response/full 
         200  #"Okay" (current-seconds) TEXT/HTML-MIME-TYPE empty
         (list (string->bytes/utf-8 
                ;(response/xexpr 
                (render-page/edit (rename/deep func/tf) make-url))))
        )
      ))
   #:extra-files-paths (list mod-dir)
   #:manager (make-threshold-LRU-manager #f (* 512 1024 1024))
   ))


(define-syntax-rule (web-launch title tfield/func)
  (launch-web ((parse/web-spec (web-spec tfield/func)) title)))


;; ============================================================================


(provide web-launch
         define/web)




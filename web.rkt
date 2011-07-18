#lang racket

(require "tfield.rkt" "render.rkt" "save.rkt")
(require web-server/servlet
         web-server/servlet-env
         xml)

(define DEBUG #f)

;; ============================================================================


;; handles expired/invalid continuations 
(define (expiration-handler req)
  (response/full 404 #"File not found" (current-seconds)
                 TEXT/HTML-MIME-TYPE empty (list #"File not found")))


; start : tfield/function -> (request -> response)
; produces a response for the initial page load, based on the 
; given tfield (which must be a tfield/function object). 
; Assumption is that any further requests made will come through AJAX,
; which is handled by req-handler.

(define ((start tf) req)
  (when (not (tfield/function? tf))
    (error 'start (string-append "cannot start web application with a specifi"
                                 "cation for a type other than a function")))
  
  (cond [(bindings-assq            ; this isn't the very first invocation?
          (string->bytes/utf-8 "requesttype") (request-bindings/raw req))
         ((req-handler tf) req)]
        [else
         (define next-req
           (send/suspend
            (λ(cont-url)
              (response/full
               200 #"Okay" (current-seconds) TEXT/HTML-MIME-TYPE 
               (list (header #"Cache-Control" #"no-cache"))
               (list (string->bytes/utf-8 
                      (render-full/string tf cont-url)))))))
         ((req-handler tf) next-req)]))




; req-handler : tfield -> (request -> response)
; generates a response to an AJAX request based on the current state of the 
; tfield. This method parses the request, determines what type it is 
; (represented as an 'event'), updates the tfield based on that event, and
; then produces a response for AJAX, which will include a continuation url
; for any subsequence AJAX requests based on the updated tfield -- which will
; again be handled by this same procedure

(define ((req-handler tf) req)
  (when DEBUG
    (printf "BINDINGS:\n~a\n" (req/bindings->string req))
    (when (andmap filled? (tfield/function-args tf))
      (printf "TFIELD:\n") 
      (pretty-print (map tfield->value (tfield/function-args tf)))(newline)))
  
  (with-handlers
      ([exn:fail? (λ(exn) ((error-display-handler) (exn-message exn) exn)
                    (expiration-handler req))])
    (define event (parse-event req))
    (define new-tf (event-dispatch tf event))
    (define resp-type (response-type event))  ; "text/xml" or "text/html"
    (define next-req
      (send/suspend
       (λ(cont-url) 
         (when DEBUG
           (when (andmap filled? (tfield/function-args new-tf))
             (printf "TFIELD after:\n")
             (pretty-print (map tfield->value 
                                (tfield/function-args new-tf)))(newline)))
         
         (define resp-xexpr  (ajax-response new-tf event cont-url))
         (when DEBUG
           (printf "Sending response:\n")(pretty-print resp-xexpr)(newline)
           (printf "***********************************")(newline))
         
         (response/xexpr resp-xexpr 
                         #:mime-type resp-type))))
    
    ((req-handler new-tf) next-req)))



; req/bindings->string : request -> string
; for debugging purposes: produces a string with all bindings in the given
;  request
(define (req/bindings->string req)
  (define p (open-output-bytes))
  (pretty-print (map (λ(id)
                       (list id (binding:form-value 
                                 (bindings-assq id (request-bindings/raw req))))
                       )
                     (map binding-id (request-bindings/raw req)) )
                p
                )
  ;(pretty-print (map (λ(b) (list (binding-id b) (binding:form-value b)))
  ;                   (request-bindings/raw req)))
  (bytes->string/utf-8 (get-output-bytes p)))


; crunch : string -> string
; removes all non-alphanumeric characters from string and converts to
;  lowercase

(define (crunch str)
  (list->string (filter (λ(c) (or (char-alphabetic? c)
                                  (char-numeric? c)))
                        (string->list (string-downcase str)))))




;; ============================================================================


; Event structure
; Event types (with options/parameters) are:
;    - 'refresh
;    - 'reload   (parses form data, refresh doesn't)
;    - 'apply-input
;    - 'save-input   
;    - 'clear-input
;    - 'listof-reorder [name : string] [from, to : number]
;    - 'listof-delete [name : string] [item : number]
;    - 'listof-add  [name : string]
;    - 'oneof-change [name : string] [chosen : ("-" or number)]
;
;    - 'preview-saved [name : string]
;    - 'list-saved   [loosematch : #f or "loosematch"]
;    - 'load-saved-edit [name : string]
;    - 'load-saved-apply [name : string]
;    - 'remove-saved-one [name : string]
;    - 'remove-saved-all [loosematch : #f or "loosematch"]
;  binding-function is : string -> (or #f string (list string<name> bytes<content>))
(struct event (type binding-function))


; event-binding : event string -> (or #f string (list string<name> bytes<content>))
(define (event-binding e key)
  ((event-binding-function e) key))

(define (event-binding/number e key)
  (define b (event-binding e key))
  (and (string? b) (string->number b)))

; parse-event : request -> event or #f
; event type is extract from "requesttype" binding of the request

(define (parse-event req)
  (define bindings (request-bindings/raw req))
  (define (lookup-func key)
    (match (bindings-assq (string->bytes/utf-8 key) bindings)
      [(? binding:form? (binding:form _ value))
       (bytes->string/utf-8 value)]
      ; TODO: ? binding:file
      [_ #f]))
  
  (match (bindings-assq #"requesttype" bindings)
    [(? binding:form? (binding:form _ value))
     (event (string->symbol (bytes->string/utf-8 value))
            lookup-func)]
    [_ (error 'parse-event "invalid or missing requesttype")]))


; event-dispatch : tfield event -> tfield
; produces an updated tfield (if at all) based on the given event

(define (event-dispatch tf ev)
  (define lookup-func (curry event-binding ev))
  (match (event-type ev)
    
    ['reload
     (parse tf lookup-func #f)]
    
    ['clear-input 
     (clear tf)]
    
    ['apply-input 
     (define parsed-tf (parse tf lookup-func #t))
     (define applied-tf (apply-tfield/function parsed-tf))
     (define new-tf (or applied-tf parsed-tf))
     (when applied-tf   ; only auto-save on successful applications
       (save-tfield new-tf)  ; auto-save
       (purge-auto-saves tf))
     new-tf]
    
    ['load-saved-edit
     (define loaded-tf (load-tfield tf (event-binding ev "name")))
     (or loaded-tf (clear tf))]
    
    ['load-saved-apply
     (define loaded-tf (load-tfield tf (event-binding ev "name")))
     (if (not loaded-tf) (clear tf) (validate loaded-tf))]
    
    ['remove-saved-one 
     (remove-save-file tf (event-binding ev "name"))
     tf]
    
    ['remove-saved-all
     (define loose-match? (equal? (event-binding ev "loosematch") "loosematch"))
     (remove-all-saves tf loose-match?)
     tf]
    
    ['save-input
     (define parsed-tf (parse tf lookup-func #f))
     (save-tfield parsed-tf #:usersave #t)
     parsed-tf]
    
    ['listof-reorder
     (define name (event-binding ev "name"))
     (define from (event-binding/number ev "from"))
     (define to (event-binding/number ev "to"))
     (update-named tf name 
             (λ(tf) (reorder-listof tf lookup-func from to)))]
    
    ['listof-add
     (define name (event-binding ev "name"))
     (update-named tf name (λ(tf) (extend-listof tf lookup-func)))]
    
    ['listof-delete
     (define name (event-binding ev "name"))
     (define item (event-binding/number ev "item"))
     (update-named tf name 
             (λ(tf) (remove-listof-item tf lookup-func item)))]
    
    ['oneof-change
     (define name (event-binding ev "name"))
     (define chosen (event-binding/number ev "chosen")) ; #f or number
     (update-named tf name
             (λ(tf/o) (parse tf/o lookup-func #f)))]
    
    ['update-field
     (define name (event-binding ev "name"))
     (or (update-named tf name (λ(tf/u) (parse tf/u lookup-func #f))) tf)]
    
     [_ tf]))


; update-listof-elts : tfield/listof (listof tfield -> listof tfield) 
;                      lookup-func
;                      -> tfield/listof
; parses the tfield/listof, then applies procedure to its elements,
; renames them and produces a copy with updated elements
(define (update-listof-elts tf proc lookup-func)
  (define parsed-tf (parse tf lookup-func #f))
  (define new-elts (rename/deep* (proc (tfield/listof-elts parsed-tf))
                                 (tfield-name parsed-tf)))
  (match parsed-tf 
    [(tfield/listof label name error base elts non-empty?)
     (tfield/listof label name #f base new-elts non-empty?)]))
     ;(struct-copy tfield/listof parsed-tf [elts new-elts]
     ;              [error #:parent tfield #f])


; reorder-listof : tfield/listof number number -> tfield/listof
; swaps the elts at given positions in the listof tfield
(define (reorder-listof tf lookup-func from to)
  (update-listof-elts tf (λ(elts) (move-to elts from to)) lookup-func))
 
; extend-listof : tfield/listof number -> tfield/listof
; adds n copies of new elts to the end of the listof elts
(define (extend-listof tf lookup-func [n 1])
  (update-listof-elts
   tf (λ(elts) (append elts (list (tfield/listof-base tf)))) lookup-func))
 
(define (remove-listof-item tf lookup-func i)
  (update-listof-elts
   tf (λ(elts) 
        (define-values (left right) (split-at elts i))
        (append left (rest right))) lookup-func))


;; ============================================================================


; response-type : event -> bytes
; the mime type of the ajax response (xml or html)
(define (response-type ev)
  (match (event-type ev)
    [#f  ; never for now?
     #"text/html; charset=utf-8"]
    [_ #"text/xml; charset=utf-8"]))


; ajax-response : event tfield string -> xexpr
; generates an appropriate xml/html response to an ajax request, of given event
; type
; note: to update cont-url, include: (a ([id "cont-url"] [href ,cont-url]) "")

(define (ajax-response tf ev cont-url)
  (define error (tfield-error tf))
  (define args (tfield/function-args tf))
  (define result (tfield/function-result tf))
  
  (define cont-url-update/xexpr ;`(eval "CONT_URL = '" ,cont-url "';"))
    `(eval "updateContUrl('" ,cont-url "');"))
  (define (refresh-elts/xexpr parentSelector)
    `(eval "refreshElements('" ,parentSelector "');"))
  (define (full-refresh/xexpr [additional '()])
    `(taconite
       (html ([select "#form-error"] ,@(if (not error) `((arg1 "")) `()))
             ,(or error ""))
       (html ([select "#edit-args"])
             (ul ,@(map (λ(x) `(li ,x)) (render*/edit args #f))))
       (html ([select "#program-output"])
             ,(if (filled? tf) (render/disp result #f) ""))
       (html ([select "#program-input"])
             ,(if (filled? tf)
                  `(ul ,@(map (λ(x) `(li ,x)) (render*/disp args #f)))
                  ""))
       ,cont-url-update/xexpr 
       (eval "populateSaved(); $('body').scrollTop(0);")
       ,(refresh-elts/xexpr "form")
       ,@additional))
  
  (match (event-type ev)

    ['ping '(ping "Hello")]
    
    ; some events require complete refresh of all elements
    [(or 'refresh 'reload 'clear-input 'save-input)
     (full-refresh/xexpr
      (if (filled? tf) `() `((eval "resultTabState(false);"))))]
    
    ['load-saved-edit
     (full-refresh/xexpr `((eval "resultTabState(false, 1);")))]

    ; attempt to switch to results tab upon application
    [(or 'apply-input 'load-saved-apply)
     (full-refresh/xexpr
      (list (if (filled? tf) 
                `(eval "resultTabState(true);") 
                `(eval "resultTabState(false);"))))]
    
    ; a number of events just need to replace the updated outer <div>
    ; as a response...
    [(or 'listof-reorder 'listof-delete 'oneof-change 'listof-add)     
     (define name (event-binding ev "name"))
     (define divname (format "#edit-args #~a-div" name))
     `(taconite
       (replaceWith ([select ,divname])
          ,(render/edit (find-named tf name) (find-parent-of-named tf name)))
       (html ([select "#form-error"] [arg1 ""]))
       ,cont-url-update/xexpr 
       ,(refresh-elts/xexpr divname))]
       
    ['list-saved 
     (define loose-match? (equal? (event-binding ev "loosematch") "loosematch"))
     `(response ,(saved-files-xml tf loose-match? #t))]
    
    ['preview-saved
     (define name (event-binding ev "name"))
     (define preview-tf (load-tfield tf name))
     (define args (and preview-tf (tfield/function-args preview-tf)))
     `(taconite 
       (html ([select "#save-preview"])
             ,(if args
                 `(ul ,@(map (λ(x) `(li ,x)) (render*/disp args #f)))
                 `(div "No data")))
       ,cont-url-update/xexpr)]
    
    [(or 'remove-saved-one 'remove-saved-all)
     `(taconite (eval "populateSaved();"))]
    
    ['update-field
     `(taconite ,cont-url-update/xexpr)]
    
    [_ `(empty-response)]))






;; ============================================================================

(provide start
         expiration-handler
         crunch)

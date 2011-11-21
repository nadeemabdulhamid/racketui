#lang racket

(require "tfield.rkt")
(require racket/runtime-path
         setup/getinfo
         web-server/servlet
         web-server/servlet-env
         web-server/templates
         xml)

(define-runtime-path SOURCE-DIRECTORY ".")

;; ============================================================================

; render-full/string : tfield/function string -> string
; produces the initial XHTML rendering of the application's main page, as
; a string. The given continuation url string will be appropriately 
; embedded somewhere in the page to allow further AJAX requests

(define (render-full/string tf cont-url)
  (define title (tfield-label tf))
  (define version ((get-info/full SOURCE-DIRECTORY) 'version))
  (define text 
    (match (tfield/function-text tf)
      [(? string? s) s]
      [xs (apply string-append (map xexpr->string xs))]))
  (include-template "htdocs/template.html"))
  


;; ============================================================================
;; render form elements for editing

; render*/edit : (listof tfield) (or #f tfield) -> (listof xexpr)
(define (render*/edit tfs parent)
  (map (λ(t) (render/edit t parent)) tfs))

; render/edit : tfield (or #f tfield) -> xexpr
(define (render/edit tf parent)
  (define parent-not-oneof? (not (tfield/oneof? parent)))
  (define parent-not-oneof/listof? (and (not (tfield/oneof? parent))
                                        (not (tfield/listof? parent))))
  (define (input-text-of name value [disabled? #f])
    `(input ([type "text"] [name ,name] [id ,name]
             [value ,value] ,@(if disabled? `([disabled "disabled"]) '()))))
    
  (match tf
    [(tfield/const label name error value)
     (cond [parent-not-oneof?
            (define label? parent-not-oneof/listof?)
            (render-basic/edit name '(tfield-constant) (and label? label)
                               (input-text-of name (format "~a" value) #t) error)]
           [else
            (render-basic/edit name '(tfield-constant) #f "" error)])]
    [(tfield/boolean label name error value)
     (define label? parent-not-oneof/listof?)
     (render-basic/edit name '(tfield-boolean) 
                   (and label? `(label ([for ,name]) ,(colonize label)))
                   `(input ([type "checkbox"] [name ,name]
                            [id ,name] ,@(if value `([checked "checked"]) '())))
                   error)]
    [(tfield/number label name error value raw-value)
     (define label? parent-not-oneof/listof?)
     (render-basic/edit name '(tfield-number) (and label? label)
                   (input-text-of name (or raw-value "")) error)]
    [(tfield/symbol label name error value)
     (define label? parent-not-oneof/listof?)
     (render-basic/edit name '(tfield-symbol) (and label? label)
                   (input-text-of name (if value (symbol->string value) "")) 
                   error)]
    [(tfield/string label name error value non-empty?)
     (define label? parent-not-oneof/listof?)
     (render-basic/edit name '(tfield-string) (and label? label) 
                   (input-text-of name (or value "")) error)]
    [(? tfield/image? _) (render-image/edit tf parent)]
    [(? tfield/file? _) (render-file/edit tf parent)]     
    [(? tfield/struct? _) (render-struct/edit tf parent)]
    [(? tfield/oneof? _)  (render-oneof/edit tf parent)]
    [(? tfield/listof? _) (render-listof/edit tf parent)]
    [(tfield/function title name error text func args result)
     "blah"]
    [_ (error (object-name render/edit)
              (format "somehow got an unknown field type: ~a" tf))]))



; colonize : (or/c string xexpr) -> string 
; adds ": " to end of given string if it's not ""
(define (colonize str)
  (if (or (not (string? str)) (string=? str "")) str (string-append str ": ")))


; div-wrapper: (listof symbol) -> [(listof xexpr) -> (or xexpr (listof xexpr))]
; produces (λ(inner) `(div ([id [<name>-div"] [class "symbol ..."]) ,@inner)
(define ((div-wrapper name classes) inner)
  `(div (,@(if name `([id ,(format "~a-div" name)]) '())
         [class ,(string-join (map symbol->string classes) " ")])
        ,@(if (xexpr? inner) (list inner) inner)))


; produces a 'nest...' class name based on depth of given tfield or name
(define (nest-level tf/name)
  (format "nest~a" (add1 (remainder (sub1 (depth-of tf/name)) 3))))


; render-basic : (listof symbol) (or #f string) xexpr (or #f xexpr) -> xexpr
(define (render-basic/edit name classes label input-elt error)
  ((div-wrapper name `(tfield tfield-basic ,@classes))
   (list (if label `(span ([class "label"]) ,(colonize label)) "")
         input-elt
         (if error `(span ([class "error"]) ,error) ""))))


; render-image/edit : tfield/image (or #f tfield) -> xexpr
(define (render-image/edit tf parent) 
  (define label? (and (not (tfield/oneof? parent))
                      (not (tfield/listof? parent))))
  (match tf
    [(tfield/image label name error mime-type data)
     (define inner-content
       (cond
         [(and mime-type data)
          `(span
            (input ([type "hidden"] [id ,name] [name ,name]
                                    [value "*IMAGE*"]))
            (img ([src ,(string-append "data:" mime-type ";base64," 
                                      (bytes->string/locale data))]))
            ,(image-clear-link name))]
         [else
          `(input ([type "text"] [name ,name] [id ,name] [size "35"]
                                 [placeholder "URL"]))
          ]))
     (render-basic/edit name '(tfield-image) (and label? label)
                        inner-content error)]))


; render-file/edit : tfield/file (or #f tfield) -> xexpr
(define (render-file/edit tf parent)
  (define label? (and (not (tfield/oneof? parent))
                      (not (tfield/listof? parent))))
  (match tf
    [(tfield/file label name error file-name mime-type temp-path)
     (define inner-content
       (cond
       [(not file-name)  ; nothing uploaded
        `(input ([type "file"] [name ,name] [id ,name]))]
       [(not temp-path)  ; notified of upload
        `(span ([class "uploading"])
               (img ([src "file-loading.gif"] [alt "file loading"]))
               "Uploading " ,file-name "... ")]
       [else
        `(span ([class "uploaded"])
               ;; see tfield.rkt: parse - tfield/file case about this
               (input ([type "hidden"] [id ,name] [name ,name]
                                       [value ,file-name]))
               ,(file-view-link name file-name)
               " " ,(file-clear-link name file-name) 
               )]))
     (render-basic/edit name '(tfield-file) (and label? label)
                        inner-content error)]))

; render-listof : tfield/listof (or #f tfield) -> xexpr
(define (render-listof/edit tf parent)
  (match tf
    [(tfield/listof label name error base elts non-empty?)
     ((div-wrapper name `(tfield tfield-listof))
      `(fieldset ([class ,(nest-level tf)])
         (legend ,label)
         ,(if error `(div ([class "error error-listof"]) ,error) "")
         (input ([type "hidden"] [name ,name] [id ,name]
                                 [value ,(format "~a" (length elts))]))
         (ol ([id ,(string-append name "-ol")] [class "tfield-listof sortable"])
             ,@(map (curry render-listof-item/edit tf) elts)
             (li ([class "nosort"])
                 (button ([id ,(string-append name "-addbtn")]
                          [class "addbtn"] [type "button"]) 
                         "Add " ,(tfield-label base))))))]))
     
(define (render-listof-item/edit tf/listof e)
       (define elt-name (tfield-name e))
       `(li ([id ,(string-append elt-name "-li")])
            (div ([class "listof-item"])
                 (div ([class "li-handle"])
                      (span ([class "ui-icon ui-icon-arrowthick-2-n-s"]) ""))
                 (button ([id ,(string-append elt-name "-delbtn")]
                          [class "delbtn"] [type "button"]) "")
                 (div ([class "listof-data"])
                      ,(render/edit e tf/listof))
                 (div ([style "clear: both;"]) ""))))      
                
; render-oneof : tfield/oneof (or #f tfield) -> xexpr
(define (render-oneof/edit tf parent)
  (match tf
    [(tfield/oneof label name error options chosen)
     (define label? (and (not (tfield/oneof? parent))
                         (not (tfield/listof? parent))))
     (define selected-tf (and chosen (list-ref options chosen)))
     (define fieldset? (and selected-tf (or (tfield/struct? selected-tf)
                                            (tfield/listof? selected-tf))))
     (define select-elt (render-select-element/edit tf))
     (define label-span
       `(span ,(if (and label? label)
                   `(label ([for ,name]) ,label " ") "") ,select-elt))
     (cond
       [(not selected-tf)
        (render-basic/edit name '(tfield-oneof) label-span empty error)]
       [fieldset?
        ((div-wrapper name `(tfield tfield-oneof))
         `(fieldset ([class ,(nest-level tf)])
            (legend ,label-span)
            ,(if error `(div ([class "error"]) ,error) "")
            ,(render/edit selected-tf tf)))]
       [else
        (render-basic/edit name '(tfield-oneof) label-span
                      (render/edit selected-tf tf) error)])]))

; render-select-element : tfield/oneof -> xexpr
(define (render-select-element/edit tf/o)
  (define name (tfield-name tf/o))
  (define opts (tfield/oneof-options tf/o))
  (define chosen (tfield/oneof-chosen tf/o))
  `(select ([name ,name] [id ,name] [class "tfield-oneof"])
     (option ([value "-"]
              ,@(if chosen '() '([selected "selected"]))) "-")
     ,@(map (λ(tflabel i)
              `(option ([value ,(number->string i)]
                        ,@(if (and chosen (= i chosen)) 
                              '([selected "selected"]) '())) ,tflabel))
            (map tfield-label opts)
            (build-list (length opts) values))))

; render-struct : tfield/struct (or #f tfield) -> xexpr
(define (render-struct/edit tf parent)
  (match tf
    [(tfield/struct label name error constr args)
     (define arg/content 
       `(ul ([class "tfield-structure"])
           ,@(map (λ(a) `(li ,(render/edit a tf))) args)))
     (define wrapper
       (if (tfield/oneof? parent)
           ((div-wrapper name '(tfield tfield-structure))
            (list (if error `(div ([class "error"]) ,error) "")
                  arg/content))
           ((div-wrapper name '(tfield tfield-structure))
            `(fieldset ([class ,(nest-level name)])
                       ,(if label `(legend ,label) "")
                       ,(if error `(div ([class "error"]) ,error) "")
                       ,arg/content)) ))
     wrapper]))


;; ============================================================================
;; render form elements for display


; render*/disp : (listof tfield) (or #f tfield) (or #f string) -> (listof xexpr)
; renders fields for display purposes; if the rendering is for previewing
;  a saved file, then save-file is the name of the save-file
(define (render*/disp tfs parent [save-file #f])
  (map (λ(t) (render/disp t parent save-file)) tfs))

; render/disp : tfield (or #f tfield) (or #f string) -> xexpr
; renders a field for display purposes; if the rendering is for previewing
;  a saved file, then save-file is the name of the save-file
(define (render/disp tf parent [save-file #f])
  (define parent-not-oneof? (not (tfield/oneof? parent)))
  (define parent-not-oneof/listof? (and (not (tfield/oneof? parent))
                                        (not (tfield/listof? parent))))
  (define parent-not-listof? (not (tfield/listof? parent)))
  
  (match tf
    [(tfield/const label name error value)
     (render-basic/disp name '(tfield-constant) #f label)]
    [(tfield/boolean label name error value)
     (render-basic/disp name '(tfield-boolean) (and parent-not-listof? label)
                        (if value "YES" "NO"))]
    [(tfield/number label name error value raw-value)
     (render-basic/disp name '(tfield-number) (and parent-not-listof? label)
                        (or raw-value "-"))]
    [(tfield/symbol label name error value)
     (render-basic/disp name '(tfield-symbol) (and parent-not-listof? label)
                        (or (and value (symbol->string value)) "-"))]
    [(tfield/string label name error value non-empty?)
     (render-basic/disp name '(tfield-string) (and parent-not-listof? label)
                        (if (equal? value "") "-"
                            (or value "-")))]
    [(tfield/image label name error mime-type data)
     (render-basic/disp name '(tfield-image) (and parent-not-listof? label)
                        (if (and mime-type data)
                            `(img ([src ,(string-append "data:" mime-type
                                                        ";base64,"
                                                        (bytes->string/locale data))]))
                            "-"))]
    
    [(tfield/file label name error file-name mime-type temp-path)
        (render-basic/disp name '(tfield-file) (and parent-not-listof? label)
                      (file-view-link name file-name save-file))]
    [(tfield/struct label name error constr args)
     ((div-wrapper #f '(tfield tfield-structure))
      `(fieldset (legend ,(colonize label))
                 (ul ,@(map (λ(a) `(li ,a)) (render*/disp args tf save-file)))))]
    [(tfield/oneof label name error options chosen)
     (define selected-tf (and chosen (list-ref options chosen)))
     (cond
       [(not selected-tf) 
        ((div-wrapper #f `(tfield tfield-oneof))
         `(span "(" ,label " not selected)"))]
       [else 
        ((div-wrapper #f `(tfield tfield-oneof))
         (render/disp selected-tf tf save-file))])]
    [(tfield/listof label name error base elts non-empty?)
     ((div-wrapper #f `(tfield tfield-listof))
      `(fieldset (legend ,(colonize label))
                 ,(if (empty? elts) 
                      "(empty)"
                      `(ol ,@(map (λ(a) `(li ,a)) (render*/disp elts tf save-file))))))]
    [(tfield/function title name error text func args result)
     `(span () "")]
    [_ (error (object-name render/disp)
              (format "somehow got an unknown field type: ~a" tf))]))


; render-basic/disp : string (listof symbol) (or #f string) 
;                           (or xexpr (listof xexpr)) -> xexpr
(define (render-basic/disp name classes label content)
  ((div-wrapper #f  ;; don't give id to display'ed divs
    `(tfield tfield-basic ,@classes))
   (list (if label `(span ([class "label"]) ,(colonize label)) "")
         `(span ,@(if (xexpr? content) (list content) content)))))
      

;; file-view-link : string (or/c #f string) (or/c #f string) -> xexpr
(define (file-view-link name file-name [save-file #f])
  (define handler
    (if save-file
        (format "viewFile($(this), '~a', '~a'); return false;" name save-file)
        (format "viewFile($(this), '~a'); return false;" name)))
  (if file-name
      `(a ([href ,file-name] [class "filelink"] [onClick ,handler]) ,file-name
          (input ([type "hidden"] [class "id"] [value ,name]))
          ,@(if save-file `((input ([type "hidden"] [class "savefile"]
                                                    [value ,save-file]))) '()))
      "-"))


;; file-clear-link : string string -> xexpr
(define (file-clear-link name file-name)
  `(button 
    ([id ,(format "~a-clearbtn" name)] [class "fileclear"] 
     [type "button"]
     [onClick ,(format "clearFile('~a'); return false;" name)])
      "remove"))

;; image-clear-link : string -> xexpr
(define (image-clear-link name)
  `(button
    ([id ,(format "~a-clearbtn" name)] [class "imageclear"]
     [type "button"] [onClick ,(format "clearImage('~a'); return false;" name)])
     "remove"))




;; ============================================================================

(provide render-full/string 
         render*/edit
         render/edit
         render-listof-item/edit
         
         render*/disp 
         render/disp)
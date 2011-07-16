#lang racket

(require lang/prim
         syntax/parse)
(require (for-syntax syntax/parse racket/bool))
(require "tfield.rkt")



#|
  Define macros to provide users (students) a convenient and compositional
  way to define web field specifications.

  First, we have macros that take a <web-spec> form (see grammar below) and
  produce an s-exp, with any web-def'd specs expanded out. Then, define a 
  function that parses s-exp's in the form of <web-spec> to produce a 
  corresponding tfield object. The top-level "web-launch" macro takes the
  parsed tfield and launches the web application with the appropriate 
  function to render the initial web page.
  

<web-def>   :=  (define/web <id> <spec>) | (define/web <id> <label> <spec>)
<launcher>  :=  (web-launch <label> <spec/function>)     (the "big-bang")
<label>     :=  <string>                       (labels are strings)
<lab-spec>  :=  [<label> <spec>]               (labeled specs)
<web-spec>  :=  (constant <value>)
             |  number
             |  boolean
             |  string
             |  symbol
             |  string+

             |  (function <string> (<proc> <lab-spec> ... -> <lab-spec>))
             |  (structure <constr> <lab-spec> ...+)
             |  (oneof <lab-spec> ...+)
             |  (listof <lab-spec>)
             |  (listof+ <lab-spec>)

             |  (check <pred> <string> <spec>)   ***** <-- TODO

(web-spec <web-spec>)

|#




;;=============================================================================
;;=============================================================================
;;=============================================================================


(define-syntax (define/web stx)

  (define-syntax-class cmp-type  ; compound types (have to traverse sub-comps)
    (pattern (~and header
                   (~or (~datum constant) (~datum structure) (~datum check)
                        (~datum oneof) (~datum listof) (~datum listof+)
                        (~datum function)))))
  
  (syntax-parse stx
                ; these first two capture common error condition:
                ;; forgetting parens (...) around compound type specs
                [(define/web id:identifier t:cmp-type x ...) 
                 #`(define id (web-spec t.header x ...))]
                [(define/web id:identifier l:str t:cmp-type x ...) 
                 #'(define id (list l (web-spec t.header x ...)))]
                
                ; these are the intended usages
                [(define/web id:identifier s)
                 #'(define id (web-spec s))]
                [(define/web id:identifier l:str s) 
                 #'(define id (list l (web-spec s)))]))


(define-syntax (web-spec stx)
  
  (define-syntax-class all-types
    (pattern (~and header
                   (~or (~datum number) (~datum boolean) (~datum string)
                        (~datum string+) (~datum symbol)
                        (~datum constant) (~datum structure) (~datum check)
                        (~datum oneof) (~datum listof) (~datum listof+)
                        (~datum function)))))
  
  (define-syntax-class cmp-type
    (pattern (~and header
                   (~or (~datum constant) (~datum structure) (~datum check)
                        (~datum oneof) (~datum listof) (~datum listof+)
                        (~datum function)))))
  
  (define-syntax-class lab-spec/sp
    #:attributes [output]
    #:description "a pair of a label and a type specification"

    ;; this first is an error condition
    [pattern x:all-types #:attr output
             (raise-syntax-error #f (format "expected a pair of a label and a type specification surrounded by parens instead of ~s" (syntax->datum #'x))
                                 #'x )]

    ;; these two are the expected behavior
    [pattern (lab:str spc) #:attr output #'(list lab (web-spec spc))]
    [pattern x:identifier
             #:fail-when (symbol=? '-> (syntax->datum #'x)) "problem?"
             ; '-> is a special symbol in function specs, and should not be assumed
             ;   to be some user-defined symbol
             #:attr output #'x]
    
    ;; these capture error conditions...
    [pattern (~and bad (a b c ...+)) #:attr output
             (raise-syntax-error #f (format "expected a pair of a label and a type specification surrounded by parens instead of ~a things" 
                                            (length (syntax->datum #'bad)))
                                 #'bad)]
    [pattern (~and z 
                   ; this fail only happens if 2nd pattern above failed,
                   ; in which case we don't want an error message 
                   ; (i.e. if matching '->)
                   (~fail #:when (symbol? (syntax->datum #'z)))) 
             
             #:attr output
             (raise-syntax-error #f (format "expected a pair of a label and a type specification surrounded by parens instead of ~s" (syntax->datum #'z))
                                 #'z )])
  
  (syntax-parse stx
                [(web-spec ((~datum constant) v)) #'(list 'constant v)]
                [(web-spec (~datum number)) #'(quote number)]  
                [(web-spec (~datum boolean)) #'(quote boolean)]
                [(web-spec (~datum string)) #'(quote string)]  
                [(web-spec (~datum string+)) #'(quote string+)]
                [(web-spec (~datum symbol)) #'(quote symbol)]  
                [(web-spec ((~datum function) txt:str (f:identifier 
                                                          p:lab-spec/sp ... 
                                                          (~datum ->) 
                                                          r:lab-spec/sp )))
                 #`(list 'function txt (list (first-order->higher-order f)
                                             p.output ... '-> r.output))]
                [(web-spec ((~datum structure) ~! constr x:lab-spec/sp ...+))
                 #'(list 'structure (first-order->higher-order constr) 
                         x.output ...)]
                [(web-spec ((~datum oneof) ~! x:lab-spec/sp ...+))
                 #`(list 'oneof x.output ...)]
                [(web-spec ((~datum listof) ~! x:lab-spec/sp))
                 #'(list 'listof x.output)]
                [(web-spec ((~datum listof+) ~! x:lab-spec/sp))
                 #'(list 'listof+ x.output)]
                
                ; this is to allow substitution of define/web'd ids...
                [(web-spec x:identifier) #'x] 
                
                ;; error conditions...
                [(web-spec ((~datum constant) z ...)) #'(list 'constant z ...)]
                [(web-spec ((~datum structure) z ...)) #'(list 'structure z ...)]
                [(web-spec (t:cmp-type z))
                 (raise-syntax-error #f "problem 1" #'z)]
                [(web-spec (~and (~seq t:cmp-type z ...) (~seq x y ...)))
                 (raise-syntax-error #f 
                                     (format
                                      "~a should be preceded by an open parens" 
                                      (syntax->datum #'t.header))
                                     #'(x y ...)
                                     #'x)]
                [(web-spec ((~datum oneof) z ...)) #'(list 'oneof z ...)]
                
                ;; anything else
                [(web-spec (x y ...)) (raise-syntax-error
                                       #f "unexpected start of specification" #'x)]
                [(web-spec x) (raise-syntax-error #f "unexpected expression" #'x)]))



;; ==============================================================================


; parse/lab-spec : <lab-spec> -> tfield
(define (parse/lab-spec lab+spec)
  ((parse/web-spec (second lab+spec)) (first lab+spec)))

; parse/web-spec : <web-spec> -> (string -> tfield)
(define (parse/web-spec spec)
  (match spec 
    ['number       new-tfield/number]
    ['boolean      new-tfield/boolean]
    ['string       new-tfield/string]
    ['string+      (λ(lab) (new-tfield/string lab #f #t))]
    ['symbol       new-tfield/symbol]
    [(list 'constant v) (λ(s) (new-tfield/const s v))]
    [(list 'function txt (list func params ... '-> rspec))
     (λ(s) (new-tfield/function s txt func (map parse/lab-spec params)
                                (parse/lab-spec rspec)))]
    [(list 'structure constr params ...)
     (λ(s) (new-tfield/struct s constr (map parse/lab-spec params)))]
    [(list 'oneof ops ...)
     (λ(s) (new-tfield/oneof s (map parse/lab-spec ops)))]
    [(list 'listof t)
     (λ(s) (new-tfield/listof s (parse/lab-spec t)))]
    [(list 'listof+ t)
     (λ(s) (new-tfield/listof s (parse/lab-spec t) empty #t))]))




;; ==============================================================================

; mostly for testing purposes; not really intended for use
(provide parse/web-spec web-spec) 

(provide define/web)  ; this is for users


 
 
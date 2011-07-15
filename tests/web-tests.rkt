#lang racket

(require "../tfield.rkt"
         "../save.rkt"
         "../syntax.rkt"
         "../web.rkt")
(require racket/runtime-path
         web-server/servlet
         web-server/servlet-env
         web-server/managers/lru)
(require (only-in lang/htdp-beginner string-upper-case? string-ith))


(define-runtime-path htdocs "../htdocs")



;; ============================================================================

(define (acronym a-los)
  (cond
    [(empty? a-los) ""]
    [(cons? a-los) (cond 
                     [(string-upper-case? (string-ith (first a-los) 0))
                      (string-append (string-ith (first a-los) 0)
                                     (acronym (rest a-los)))]
                     [else (acronym (rest a-los))])]
    ))

(define acro/tf (value->tfield 
                 ((parse/web-spec
                   (web-spec 
                    (function 
                     "Produces an acronym of the capitalized words in the given list."
                     (acronym ["Words" (listof ["Word" string])]
                              -> ["The acronym" string]))))
                  "Acronym Builder")
                 '(("United" "States" "of"))))

;((start (first (tfield/function-args acro/tf))) #f)

(define (go tf)
  (serve/servlet (start tf)
                 #:extra-files-paths (list htdocs)
                 #:manager (make-threshold-LRU-manager #f (* 512 1024 1024))
                 #:servlet-path "/acronymbuilder.rkt"))


;; ============================================================================

; a grade is either:
;   - a number or
;;  - a string (excusal reason) or
;;  - empty  (missing)
;;  - retake (make-retake number number)
(define-struct retake (fst snd) #:transparent)

; average : listof-grade/ne -> number

(define (average alog)
  (define (sum alog)
    (apply + (map (λ(g) (cond [(number? g) g]
                              [(retake? g) (max (retake-fst g) (retake-snd g))]
                              [else 0])) alog)))
  (define (how-many-count alog)
    (apply + (map (λ(g) (cond [(string? g) 0]
                              [else 1])) alog)))
  (/ (sum alog) (how-many-count alog)))

(equal? (average `(90 () "sick" ,(make-retake 45 88) 85)) (/ (+ 90 85 88) 4))

(define accugrade/tf (value->tfield 
                 ((parse/web-spec
                   (web-spec 
                    (function 
                     "Computes the average of actual grades in the given list."
                     (average ["Grade Results" 
                       (listof ["Grade Result" 
                                (oneof ["Actual grade" number]
                                       ["Retake"
                                        (structure make-retake
                                         ["First attempt" number]
                                         ["Second attempt" number])]
                                       ["Excused" string+]
                                       ["Missing" (constant empty)])])]
                              -> ["Average grade" number]))))
                  "AccuGrader")
                 `((90 () "sick" ,(make-retake 45 88) 85))))

;; ============================================================================

;(go acro/tf)
(go accugrade/tf)



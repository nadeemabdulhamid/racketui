#lang racket

(require "tfield.rkt")
(require srfi/19 ;racket/date
         xml)

#|
  Provides functionality to save/load tfield data to file. Here's about how it 
  works...

  We need to be able to serialize both the structure of a tfield as well as the
  embedded data. For each purpose, provide a way to convert a tfield to/from
  an s-expression (see skel-expr and data-expr below). These s-exps can then
  be serialized as necessary.

  When producing an s-exp corresponding to the *structure* of a tfield, one can
  either include constructor/function names or not. This facilitates a hashing
  mechanism where changes, or differences, in those names does not affect
  retrieval. This also facilitates the use of saved data for distinct functions
  that have the same structure of data that they expect. (e.g. There may be
  many functions that process a list-of-numbers. We want to be able to load,
  if so desired, saved list-of-number data to be used with any one of those 
  functions.)

  The way retrieval works, for efficiency, is: given a skel-expr (an s-exp
  representing the skeleton, or structure, of a tfield), we can generate a
  hash code (large integer).* This hash code is the name of a directory in
  which all data files corresponding to that tfield will be stored. Now,
  the hash code for the directory does *not* include constructor/function
  names. Thus, data for different functions that have the same tfield
  representation will end up in the same directory. We compute a secondary
  hash code on the s-exp that includes the constructor/function names -- 
  this hash code forms part of the filenames of the data files in the 
  directory. Thus, we can easily find the directory in which data files 
  for a tfield/function are stored, and filter, if desired, for exactly
  that tfield, or for all that have an identical structure, but different
  function names.

  * note: I discovered that hash codes for symbols are different between
    program runs, so we actually compute the hash of a string representation
    of the s-exp.


  Data files are saved with filenames that indicate timestamp, full hash
  code, whether auto-saved or user-saved, in addition to a random suffix
  on the name, to eliminate collisions where two saves happen at exactly
  the same time.

  The content of data files is: timestamp, whether user-saved (vs. auto-saved),
  the full skel-expr of the tfield from which the data file was generated,
  and then the data-expr itself, containing the serialized data from the
  tfield.
  
|#


;; TODO: add deletion functionality
;; (remove a single data file, all files for a tfield, 
;;  all files loosely matching, all saved files)


;;=============================================================================
;;=============================================================================
;;=============================================================================

;; Directory to put saved files...
(define TEMP-DIR-BASE (build-path (find-system-path 'pref-dir) "racketui-saves"))

;; Maximum number of auto-saved files to keep (per specific tfield, not
;;   loose match)
(define MAX-AUTO-SAVES 5)

;; A skel-expr is either:
;;   - 'constant
;;   - 'number
;;   - 'string
;;   - 'symbol
;;   - 'boolean
;;   - '(structure (... skel-expr ...))
;;   - '(structure constr-name (... skel-expr ...))  *constr-name is a symbol
;;   - '(oneof ... skel-expr ...)
;;   - '(listof skel-expr)
;;   - '(function (... skel-expr ...))
;;   - '(function func-name (... skel-expr ...))  *func-name is a symbol


;; A data-expr is either:
;;    - #f   (for 'constant)
;;    - <number>
;;    - <string>
;;    - <symbol>
;;    - <boolean>
;;    - (list 'structure <data-expr> ...)   
;;    - (list 'oneof <number/boolean> <data-expr>)   <-- the selected option
;;    - (list 'listof <data-expr> ...)
;;    - (list 'function <data-expr> ...)   <-- the arguments only



;; tfield->skel : tfield boolean -> skel-expr
;; produces an sexpression (made up of lists and symbols) representing
;;  the *structure* of the tfield (not data); if complete? is #t,
;;  includes names of functions and constructors (as symbols), and the
;;  result type also, in the produced expression

(define (tfield->skel-expr tf [complete? #f])
  (match tf
    [(tfield/const label name errors value)
     'constant]
    [(tfield/number label name errors value raw-value)
     'number]
    [(tfield/string label name errors value non-empty?)
     'string]
    [(tfield/symbol label name errors value)
     'symbol]
    [(tfield/boolean label name errors value)
     'boolean]
    [(tfield/struct label name errors constr args)
     `(structure ,@(if complete? (list (object-name constr)) empty)
                 ,(map (λ(f) (tfield->skel-expr f complete?)) args))]
    [(tfield/oneof label name errors options chosen)
     `(oneof ,@(map (λ(f) (tfield->skel-expr f complete?)) options))]
    [(tfield/listof label name errors base elts non-empty?)
     `(listof ,(tfield->skel-expr base complete?))]
    [(tfield/function title name errors text func args result)
     `(function ,@(if complete? (list (object-name func)) '())
                (,@(map (λ(f) (tfield->skel-expr f complete?)) args)
                 ,@(if complete? (list (tfield->skel-expr result complete?)) '())))]
    [_ (error 'tfield->skel (format "somehow got an unknown field type: ~a" tf))])
  )


;; remove-names/skel : skel-expr -> skel-expr
;; produces skel-expr with names removed from structure/function expressions

(define (remove-names/skel-expr se)
  (match se
    [(list 'structure (list flds ...)) 
     (list 'structure (map remove-names/skel-expr flds))]
    [(list 'structure c-name (list flds ...)) 
     (list 'structure (map remove-names/skel-expr flds))]
    [(list 'function (list args ...)) 
     (list 'function (map remove-names/skel-expr args))]
    [(list 'function f-name (list args ...)) 
     (list 'function (map remove-names/skel-expr args))] 
    [(list 'oneof ops ...) (cons 'oneof (map remove-names/skel-expr ops))]
    [(list 'listof t) (list 'listof (remove-names/skel-expr t))]
    [_ se]))


;; tfield-hash : tf boolean -> fixnum

(define (tfield-hash tf [func-names? #f])
  (abs (equal-hash-code (format "~a" (tfield->skel-expr tf func-names?)))))


;; tfield->data-expr : tf -> data-expr

(define (tfield->data-expr tf)
  (match tf
    [(tfield/const label name errors value)
     #f]
    [(tfield/number label name errors value raw-value)
     value]
    [(tfield/string label name errors value non-empty?)
     value]
    [(tfield/symbol label name errors value)
     value]
    [(tfield/boolean label name errors value)
     value]
    [(tfield/struct label name errors constr args)
     (cons 'structure (map tfield->data-expr args))]
    [(tfield/oneof label name errors options chosen)
     (list 'oneof
           (and (number? chosen) (< chosen (length options)) chosen)
           (and chosen (tfield->data-expr (list-ref options chosen))))]
    [(tfield/listof label name errors base elts non-empty?)
     (cons 'listof (map tfield->data-expr elts))]
    [(tfield/function title name errors text func args result)
     (cons 'function (map tfield->data-expr args))]
    [_ (error 'tfield->data-expr (format "somehow got an unknown field type: ~a" tf))]))


;; unify-data-expr/tfield : tf data-expr -> tf

(define (unify-data-expr/tfield otf de)
  (define tf (clear otf))
  (match tf
    [(tfield/const label name errors value)
     tf]
    [(tfield/number label name errors value raw-value)
     (struct-copy tfield/number tf 
                  [value (and (number? de) de)] 
                  [raw-value (or (and (number? de) (number->string de))
                                 (and (string? de) de))])] ; string still goes into raw-value
    [(tfield/string label name errors value non-empty?)
     (struct-copy tfield/string tf [value (and (string? de) de)])]
    [(tfield/symbol label name errors value)
     (struct-copy tfield/symbol tf [value (and (symbol? de) de)])]
    [(tfield/boolean label name errors value)
     (struct-copy tfield/boolean tf [value (equal? #t de)])]
    [(tfield/struct label name errors constr args)
     (if (and (cons? de) (equal? 'structure (first de)) (= (length (rest de)) (length args)))
         (struct-copy tfield/struct tf
                      [args (map unify-data-expr/tfield args (rest de))])
         (clear tf))]
    [(tfield/oneof label name errors options chosen)
     (if (and (cons? de) (equal? 'oneof (first de)) (= (length de) 3) 
              (number? (second de)) (< (second de) (length options)))
         (let* ([cho (second de)]
                [op (third de)]
                [new-ops (map (λ(o i) (if (= i cho) (unify-data-expr/tfield o op) o))
                              options (build-list (length options) values))]
                )
           (struct-copy tfield/oneof tf [chosen cho] [options new-ops]))
         (clear tf))]
    [(tfield/listof label name errors base elts non-empty?)
     (if (and (cons? de) (equal? 'listof (first de)))
         (struct-copy tfield/listof tf
                      [elts (rename/deep* (map (curry unify-data-expr/tfield base) (rest de))
                                          name)])
         (clear tf))]
    [(tfield/function title name errors text func args result)
     (if (and (cons? de) (equal? 'function (first de)) (= (length (rest de)) (length args)))
         (let ([new-tf
                (struct-copy tfield/function tf
                             [args (map unify-data-expr/tfield args (rest de))] 
                             [result (clear result)])])
           (or (apply-tfield/function new-tf) new-tf)) ; try to fill in result
         (clear tf))]
    [_ (error 'unify-data-expr/tfield (format "somehow got an unknown field type: ~a" tf))]))


;;==============================================================================
;;==============================================================================
;;==============================================================================

;; SAVING/READING TO/FROM FILE


;; save-file-prefix : tfield (number) -> string
;; produces the prefix of a temporary file name corresponding to the
;;  given tf 
;; file name prefix format:
;;     "<timestamp>-<fullskelhashoftf>-<usersave0/1>-"
(define (save-file-prefix tf 
                          #:timestamp [timestamp (current-seconds)]
                          #:usersave [usersave? #f])
  (format "~a-~a-~a-" timestamp (tfield-hash tf #t) (if usersave? 1 0))
  )


;; save-directory-name : tfield -> string
;; determines save directory for given tfield (based on short skel-expr hash)
(define (save-directory-name tf)
  (format "~a" (tfield-hash tf #f)))
  

;; check/make-temp-dir : -> none

(define (check/make-dir dir-path)
  (when (not (directory-exists? dir-path))
    (make-directory dir-path)
    ))

(define (check/make-temp-dir)
  (check/make-dir TEMP-DIR-BASE))



;; load-tfield : tfield string -> tfield or #f
(define (load-tfield tf file-name)
  (define save-dir (build-path TEMP-DIR-BASE (save-directory-name tf)))
  (define file-path (build-path save-dir file-name))
  (if (not (file-exists? file-path))
      #f
      (with-input-from-file file-path
        (λ()
          (define data (read))
          (cond 
            [(not (= (length data) 4)) #f]
            [else (unify-data-expr/tfield tf (fourth data))]
            )))))



;; save-tfield : tfield [number] [boolean] -> path or #f

(define (save-tfield tf 
                     #:timestamp [timestamp (current-seconds)]
                     #:usersave [usersave? #f])
  (define save-dir (build-path TEMP-DIR-BASE (save-directory-name tf)))
  (check/make-temp-dir)    ; make-temporary-file needs access to the directory
  (check/make-dir save-dir)
  
  (define write-thunk
    (λ()
      (write (list timestamp
                   usersave?
                   (tfield->skel-expr tf #t)
                   (tfield->data-expr tf)))
      ))
  (define file-name
    (make-temporary-file (string-append (save-file-prefix tf #:timestamp timestamp
                                                            #:usersave usersave?) "~a.sav")
                           #f save-dir))
  
  (with-output-to-file file-name write-thunk #:exists 'truncate)
  file-name
  )



;; saved-files-for : tfield [boolean] -> (listof string)
;; if loose-match?, then produces all saved files in directory,
;;   ignoring function/constructor names

(define (saved-files-for tf [loose-match? #f])
  (define save-dir (build-path TEMP-DIR-BASE (save-directory-name tf)))
  (define file-paths
    (cond [(not (directory-exists? save-dir)) empty]
          [loose-match? (directory-list save-dir)]
          [else 
           (define all-files (directory-list save-dir))
           (define tf-hash (tfield-hash tf #t))
           (filter (λ(f) (= tf-hash (hash-of/tfield-file f))) all-files)
           ]))
  (map (compose path->string file-name-from-path) file-paths))
  

;; decompose-name/tfield-file : string -> (list <timestamp> <long-hash> <usersave>)

(define (decompose-name/tfield-file filename)
  (define pcs (map string->number (take (regexp-split #rx"-" filename) 3)))
  (list (first pcs) (second pcs) (= (third pcs) 1)))


;; timestamp/tfield-file : string -> timestamp

(define (timestamp/tfield-file filename)
  (first (decompose-name/tfield-file filename)))

(define (hash-of/tfield-file filename)
  (second (decompose-name/tfield-file filename)))

(define (user-saved?/tfield-file filename)
  (third (decompose-name/tfield-file filename)))



; saved-files-xml : tfield boolean [boolean #f] -> xexpr
; if not bunched:
;    (filelist (savefile ([name ...] [timestamp ...] [datestring ...]) "") ...)
; if bunched:
;    (filelist (group ([datestring ...]) 
;                (savefile ([name ...] [timestamp ...] [datestring ...]
;                           [timestring ...]) "") ...) ...)
; in either case, may also include [usersaved "true"] attribute if so
;
; the groups are in chronological order
(define (saved-files-xml tf loose-match [bunched? #f])
  (define sfs (sort (saved-files-for tf loose-match) string<?))
  (define day-of
    (compose date-year-day time-utc->date 
             (curry make-time time-utc 0) timestamp/tfield-file))

  (define groups
     (reverse
      (map reverse
           (foldl (λ(fn grps) 
                    (cond [(empty? grps) (list (list fn))]
                          [(= (day-of fn) (day-of (first (first grps))))
                           (cons (cons fn (first grps)) (rest grps))]
                          [else (cons (list fn) grps)]))
                  '() sfs))))
  
  (if bunched?
      ;;; bunched...
      `(filelist ,@(map 
         (λ(grp) 
           (define ts (timestamp/tfield-file (first grp)))
           `(group ([datestring ,(format-seconds ts "~A, ~B ~e, ~Y")])
                   ,@(map (λ(fn)
                            (define ts (timestamp/tfield-file fn))
                            `(savefile ([name ,fn]
                                        [timestamp ,(number->string ts)]
                                        ,@(if (user-saved?/tfield-file fn)
                                              '([usersaved "true"]) '())
                                        [datestring
                                         ,(format-seconds ts "~A, ~B ~e, ~Y ~r")]
                                        [timestring
                                         ,(format-seconds ts "~r")]) ""))
                          grp))) groups))
      
      ;;; not bunched...
      `(filelist
        ,@(map (λ(fn) 
                 (define ts (timestamp/tfield-file fn))
                 `(savefile ([name ,fn]
                             [timestamp ,(number->string ts)]
                             ,@(if (user-saved?/tfield-file fn)
                                              '([usersaved "true"]) '())
                             [datestring 
                              ,(format-seconds ts "~A, ~B ~e, ~Y ~r")]) ""))
                 ;,(date->string (seconds->date ts) #t)]) "")
               sfs))))


; format-seconds : exact-int format-string -> string
(define (format-seconds secs fs)
  (date->string (time-utc->date (make-time time-utc 0 secs)) fs))







;;==============================================================================
;;==============================================================================
;;==============================================================================


;; purge-auto-saves : tfield [number] -> void
;; removes all except the given number of auto-save (non-user-saved) files for
;; the given tfield

(define (purge-auto-saves tf [num MAX-AUTO-SAVES])
  
  ; sort in reverse order, because we want to *keep* the latest
  (define auto-saves 
    (filter (compose not user-saved?/tfield-file)
                      (sort (saved-files-for tf #f) string>?)))
  (define-values (keep throw)
    (split-at auto-saves (min num (length auto-saves))))
  
  ;(printf "KEEPING ~a\nPURGING ~a ~a\n" keep num throw)
  (for ([file-name throw]) (remove-save-file tf file-name)))


;; remove-save-file : tfield string -> void

(define (remove-save-file tf file-name)
  (define save-dir (build-path TEMP-DIR-BASE (save-directory-name tf)))
  (define file-path (build-path save-dir file-name))
  (when (file-exists? file-path) (delete-file file-path)))


;; remove-all-saves : tfield [boolean] -> void

(define (remove-all-saves tf [loose-match? #f])
  (for ([file-name (saved-files-for tf loose-match?)])
    (remove-save-file tf file-name)))


;;==============================================================================
;;==============================================================================
;;==============================================================================

;; Exports and Contracts

; for testing...
;(provide save-file-prefix save-directory-name decompose-name/tfield-file)

(provide/contract

 (tfield->data-expr (-> tfield? any))
 (unify-data-expr/tfield (-> tfield? any/c tfield?))

 (tfield->skel-expr (->* (tfield?) 
                         (boolean?) 
                         any))
 (remove-names/skel-expr (-> any/c any))

 (tfield-hash (->* (tfield?) 
                   (boolean?) 
                   natural-number/c))
 (save-tfield (->* (tfield?)
                   (#:timestamp natural-number/c #:usersave boolean?)
                   (or/c #f path-string?)))
 (load-tfield (-> tfield? path-string? (or/c #f tfield?)))
 
 (saved-files-for (->* (tfield?) (boolean?) (listof string?)))
 (saved-files-xml (->* (tfield? boolean?) (boolean?) xexpr?))
 (timestamp/tfield-file (-> string? natural-number/c))
 (user-saved?/tfield-file (-> string? boolean?))
 (hash-of/tfield-file (-> string? natural-number/c))
 (purge-auto-saves (->* (tfield?) (number?) void))
 (remove-save-file (-> tfield? string? void))
 (remove-all-saves (->* (tfield?) (boolean?) void))
 )
 
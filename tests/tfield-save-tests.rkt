#lang racket

(require racket/date
         rackunit
         rackunit/gui
         rackunit/text-ui
         "../tfield.rkt"
         "../tfield-syntax.rkt"
         "../tfield-save.rkt")


(struct cartpt (x y) #:transparent)
(struct entry (name num) #:transparent)
(struct entry2 (name num) #:transparent)

; samp-func : (listof num) string -> #f or entry
(define (samp-func nums name)
  (if (< (apply + nums) 0) #f
      (entry (apply + nums) name)))

(define (samp-func2 nums name)
  (if (< (apply + nums) 0) #f
      (entry2 (apply + nums) name)))

(define tf1/p
  ((parse/web-spec (web-spec (function "howdy" 
                                (samp-func ["l" (listof ["n" number])]
                                           ["s" string] -> 
                                           ["o" (oneof ["c" (constant #f)]
                                                       ["e" (structure 
                                                             entry
                                                             ["en" number]
                                                             ["es" string])])])))) "f"))
(define tf2/p
  (value->tfield 
   ((parse/web-spec (web-spec (function "howdy" 
                                (samp-func2 ["l" (listof ["n" number])]
                                           ["s" string] -> 
                                           ["o" (oneof ["c" (constant #f)]
                                                       ["e" (structure 
                                                             entry2
                                                             ["en" number]
                                                             ["es" string])])])))) "f")
   (list (list 10 20 30) "yo yo")))


(define tfield->skel-expr-tests
  (test-suite
   "tfield->skel-expr (tfield to sexpr structure) and *hash* Tests"
   
   (let* ([tn1 (new-tfield/number "a number" 4 "4" #:name "a")]
          [ts1 (new-tfield/string "a string" #f #t #:name "b")]
          [tl1 (new-tfield/listof "numbers" tn1 #:name "c")]
          [ty1 (new-tfield/symbol "a symbol" 'hi #:name "d")]
          [ty2 (new-tfield/symbol "a symbol" #f #:name "e")]
          
          [tc1 (new-tfield/const "failure" #f)]
          [te1 (new-tfield/struct "entry" entry (list tn1 ts1) #:name "i")]
          
          [to1 (new-tfield/oneof "#f or entry" (list tc1 te1) #:name "d")]
          
          [title "Entry Generator Program"]
          [text (string-append "Sums list of values to generate person's entry" 
                               " (produces failure if sum less than 0")]
          [tf1 (new-tfield/function title
                                    text
                                    samp-func
                                    (list tl1 ts1)
                                    to1 #:name "g")]
          [tf2 (new-tfield/function 
                title text samp-func
                (list (value->tfield tl1 (list 30 40 24)) (value->tfield ts1 "bob"))
                (value->tfield to1 (entry 94 "bob")) 
                #:name "h")]          
          )
     
     (check-equal? (tfield->skel-expr tn1) 'number)
     (check-equal? (tfield->skel-expr tn1 #t) 'number)
     
     (check-equal? (tfield->skel-expr ts1) 'string)
     (check-equal? (tfield->skel-expr ts1 #t) 'string)
     
     (check-equal? (tfield->skel-expr ty1) 'symbol)
     (check-equal? (tfield->skel-expr ty2 #t) 'symbol)
     
     (check-equal? (tfield->skel-expr tc1) 'constant)
     (check-equal? (tfield->skel-expr tc1 #t) 'constant)
     
     (check-equal? (tfield->skel-expr tl1) '(listof number))
     (check-equal? (tfield->skel-expr tl1 #t) '(listof number))

     (check-equal? (tfield->skel-expr te1) '(structure (number string)))
     (check-equal? (tfield->skel-expr te1 #t) '(structure entry (number string)))
     
     (check-equal? (tfield->skel-expr to1) '(oneof constant (structure (number string))))
     (check-equal? (tfield->skel-expr to1 #t) '(oneof constant (structure entry (number string))))
     
     (check-equal? (tfield->skel-expr tf1) '(function ((listof number) string 
                                               (oneof constant (structure (number string))))))
     (check-equal? (tfield->skel-expr tf1 #t) 
                   '(function samp-func ((listof number) string 
                              (oneof constant (structure entry (number string))))))
     
     (check-equal? (tfield->skel-expr tf1) (tfield->skel-expr tf1/p))
     (check-equal? (tfield->skel-expr tf1 #t) (tfield->skel-expr tf1/p #t))
     
     ;; tfield-hash
     (check-equal? (tfield-hash tf1) (tfield-hash tf1/p))
     (check-equal? (tfield-hash tf1 #t) (tfield-hash tf1/p #t))
     (check-not-equal? (tfield-hash tf1/p) (tfield-hash tf1/p #t))
     
     (check-equal? (tfield-hash tf1/p) (tfield-hash tf2/p))
     (check-not-equal? (tfield-hash tf1/p #t) (tfield-hash tf2/p #t))
     
     )))


(define remove-names-tests
  (test-suite
   "remove-names/skel-expr Tests"
   
   (let ()
     
     (check-equal? (remove-names/skel-expr 'number) 'number)
     (check-equal? (remove-names/skel-expr 'string) 'string)
     (check-equal? (remove-names/skel-expr 'symbol) 'symbol)
     (check-equal? (remove-names/skel-expr '(structure entry (number string)))
                   '(structure (number string)))
     (check-equal? (remove-names/skel-expr '(oneof constant (structure entry (number string))))
                   '(oneof constant (structure (number string))))
     (check-equal? (remove-names/skel-expr '(function samp-func 
                                         ((listof number) string 
                                          (oneof constant (structure entry (number string))))))
                   '(function ((listof number) string 
                              (oneof constant (structure (number string))))))
     (check-equal? (remove-names/skel-expr
                    '(function ((listof number) string 
                                                (oneof constant (structure (number string))))))
                   '(function ((listof number) string 
                                               (oneof constant (structure (number string))))))
     )))


(define tfield->data-expr-tests
  (test-suite
   "tfield->data-expr Tests"
   
   (let* ([tn1 (new-tfield/number "a number" 4 "4" #:name "a")]
          
          [ts1 (new-tfield/string "a string" #f #t #:name "b")]
          [ts2 (new-tfield/string "a string" "howdy" #t #:name "b")]
          
          [tl1 (new-tfield/listof "numbers" tn1 #:name "c")]

          [ty1 (new-tfield/symbol "a symbol" 'hi #:name "d")]
          [ty2 (new-tfield/symbol "a symbol" #f #:name "e")]

          [tc1 (new-tfield/const "failure" #f)]
          [te1 (new-tfield/struct "entry" entry (list tn1 ts1) #:name "i")]
          
          [to1 (new-tfield/oneof "#f or entry" (list tc1 te1) #:name "d")]
          
          [title "Entry Generator Program"]
          [text (string-append "Sums list of values to generate person's entry" 
                               " (produces failure if sum less than 0")]
          [tf1 (new-tfield/function title
                                    text
                                    samp-func
                                    (list tl1 ts1)
                                    to1 #:name "g")]
          [tf2 (new-tfield/function 
                title text samp-func
                (list (value->tfield tl1 (list 30 40 24)) (value->tfield ts1 "bob"))
                (value->tfield to1 (entry 94 "bob")) 
                #:name "h")]          
          )

     (check-equal? (tfield->data-expr tn1) 4)
     (check-equal? (tfield->data-expr ts1) #f)
     (check-equal? (tfield->data-expr ts2) "howdy")
     (check-equal? (tfield->data-expr ty1) 'hi)
     (check-equal? (tfield->data-expr ty2) #f)
     (check-equal? (tfield->data-expr (value->tfield tl1 (list 30 40 24))) 
                   '(listof 30 40 24))
     (check-equal? (tfield->data-expr tl1) '(listof))
     (check-equal? (tfield->data-expr tc1) #f)
     (check-equal? (tfield->data-expr to1) '(oneof #f #f))
     (check-equal? (tfield->data-expr (value->tfield to1 (entry 25 "hi"))) 
                   '(oneof 1 (structure 25 "hi")))
     (check-equal? (tfield->data-expr te1) '(structure 4 #f))
     (check-equal? (tfield->data-expr tf1) '(function (listof) #f))
     
     )))

(define file-name-tests
  (test-suite
   "File name manipulation Tests"
   
   (let ([sfs (saved-files-for tf1/p)]
         [sfs+ (saved-files-for tf2/p #t)])
     
     ;(check-equal? (decompose-name/tfield-file "124324-45452-1-345252.sav") (list 124324 45452 #t))
     (check-equal? (timestamp/tfield-file "124324-45452-1-345252.sav") 124324)
     (check-equal? (user-saved?/tfield-file "124324-45452-1-345252.sav") #t)
     (check-equal? (user-saved?/tfield-file "124324-45452-0-345252.sav") #f)
     (check-equal? (hash-of/tfield-file "124324-45452-0-345252.sav") 45452)
     
     (check-equal? (saved-files-xml tf1/p)
                   `(filelist
                     ;(savefile ([name "1309008325-7149278-0-13090083251309008325732.sav"]
                     ;           [timestamp "1309008325"]
                     ;           [datestring "Saturday, June 25th, 2011 9:25:25am"]))
                     ,@(map (λ(fn)
                              (define ts (timestamp/tfield-file fn))
                              `(savefile ([name ,fn]
                                          [timestamp ,(number->string ts)]
                                          [datestring ,(date->string (seconds->date ts) #t)]) 
                                         ""))
                            sfs)))
     (check-equal? (saved-files-xml tf2/p #t)
                   `(filelist
                     ;(savefile ([name "1309008325-7149278-0-13090083251309008325732.sav"]
                     ;           [timestamp "1309008325"]
                     ;           [datestring "Saturday, June 25th, 2011 9:25:25am"]))
                     ,@(map (λ(fn)
                              (define ts (timestamp/tfield-file fn))
                              `(savefile ([name ,fn]
                                          [timestamp ,(number->string ts)]
                                          [datestring ,(date->string (seconds->date ts) #t)]) 
                                         ""))
                            sfs+)))
     )))

(define unify-tests
  (test-suite
   "Unify data-expr/tfield Tests"
   
   (let* ([tn1 (new-tfield/number "a number" 4 "4" #:name "a")]
          [tcst1 (new-tfield/const "a const" "hello")]
          
          [ts1 (new-tfield/string "a string" #f #t #:name "b")]
          [ts2 (new-tfield/string "a string" "howdy" #f #:name "b")]
          
          [ty1 (new-tfield/symbol "a symbol" 'hi #:name "d")]
          [ty2 (new-tfield/symbol "a symbol" #f #:name "e")]

          [tl1 (new-tfield/listof "numbers" tn1 #:name "c")]
          
          [tc1 (new-tfield/const "failure" #f)]
          [te1 (new-tfield/struct "entry" entry (list tn1 ts1) #:name "i")]
          
          [to1 (new-tfield/oneof "#f or entry" (list tc1 te1) #:name "d")]
          
          [title "Entry Generator Program"]
          [text (string-append "Sums list of values to generate person's entry" 
                               " (produces failure if sum less than 0")]
          [tf1 (new-tfield/function title
                                    text
                                    samp-func
                                    (list tl1 ts1)
                                    to1 #:name "g")]
          [tf2 (new-tfield/function 
                title text samp-func
                (list (value->tfield tl1 (list 30 40 24)) (value->tfield ts1 "bob"))
                (value->tfield to1 (entry 94 "bob")) 
                #:name "h")]          
          )
  
     (check-equal? (unify-data-expr/tfield tcst1 #f) tcst1)
     (check-equal? (unify-data-expr/tfield tn1 #f) 
                   (new-tfield/number "a number" #f #f #:name "a"))
     (check-equal? (unify-data-expr/tfield tn1 7) 
                   (new-tfield/number "a number" 7 "7" #:name "a"))
     (check-equal? (unify-data-expr/tfield ts1 3) ts1)
     (check-equal? (unify-data-expr/tfield ts2 #f) 
                   (new-tfield/string "a string" #f #f #:name "b"))
     (check-equal? (unify-data-expr/tfield ts2 "yippee")
                   (new-tfield/string "a string" "yippee" #f #:name "b"))
     
     (check-equal? (unify-data-expr/tfield ty1 3)
                   (new-tfield/symbol "a symbol" #f #:name "d"))
     (check-equal? (unify-data-expr/tfield ty1 'yippee)
                   (new-tfield/symbol "a symbol" 'yippee #:name "d"))
     
     (check-equal? (unify-data-expr/tfield te1 '(structure 45 "hi"))
                   (value->tfield te1 (entry 45 "hi")))
     (check-equal? (unify-data-expr/tfield te1 6)
                   (clear te1))
     (check-equal? (unify-data-expr/tfield te1 '(structure 1 2 3))
                   (clear te1))
     (check-equal? (unify-data-expr/tfield te1 '(structure "hi" "bye"))
                   (new-tfield/struct "entry" entry 
                                      (list (new-tfield/number "a number" #f "hi" #:name "a") 
                                            (new-tfield/string "a string" "bye" #t #:name "b"))
                                      #:name "i"))
     
     (check-equal? (unify-data-expr/tfield to1 '(oneof 0 #f))
                   (new-tfield/oneof "#f or entry" (list tc1 te1) 0 #:name "d"))
     (check-equal? (unify-data-expr/tfield to1 '(oneof 1 (structure 45 "hi")))
                   (new-tfield/oneof "#f or entry" 
                                     (list tc1 (value->tfield te1 (entry 45 "hi")))
                                     1 #:name "d"))
     (check-equal? (unify-data-expr/tfield to1 '(oneof 2 #f))
                   (clear to1))
  
     (check-equal? (unify-data-expr/tfield tl1 5)
                   (new-tfield/listof "numbers" (clear tn1) #:name "c"))
     (check-equal? (unify-data-expr/tfield tl1 '(listof 5 6 7 8))
                   (new-tfield/listof "numbers" tn1
                                      (rename/deep* (map (curry value->tfield tn1) '(5 6 7 8))
                                                    "c")
                                      #:name "c"))
     (check-equal? (unify-data-expr/tfield tl1 '(listof ))
                   tl1)
     
     (check-equal? (tfield/function-args
                    (unify-data-expr/tfield tf1 '(function (listof 3 2 1) "hello")))
                   (list (new-tfield/listof "numbers" tn1
                                            (list 
                                             (new-tfield/number "a number" 3 "3" #:name "c-0")
                                             (new-tfield/number "a number" 2 "2" #:name "c-1")
                                             (new-tfield/number "a number" 1 "1" #:name "c-2"))
                                            #:name "c")
                         (value->tfield ts1 "hello")))
     (check-equal? (tfield/function-result
                    (unify-data-expr/tfield tf1 '(function (listof 3 2 1) "hello")))    
                   (value->tfield to1 (samp-func '(3 2 1) "hello")))
     
     )))


(define tfield-tests
  (test-suite
   "All tfield-save tests"
   tfield->skel-expr-tests
   remove-names-tests
   tfield->data-expr-tests
   file-name-tests
   unify-tests
   
   ))


(run-tests tfield-tests)


;;==============================================================================
;;==============================================================================
;;==============================================================================

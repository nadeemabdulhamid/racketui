#lang racket

(require rackunit
         rackunit/gui
         rackunit/text-ui
         "../tfield.rkt")  



(define misc-tests
  (test-suite
   "Misc Tests"
   
   (let ()
     ; gen-new-name and reset-name-counter
     (reset-name-counter 0)
     (check-equal? (gen-new-name) "tfield0")
     (check-equal? (gen-new-name) "tfield1")
     (reset-name-counter 12)
     (check-equal? (gen-new-name) "tfield12")
     (check-equal? (gen-new-name) "tfield13")
     
     )))


(define constr-tests
  (test-suite
   "Constructor Tests"
   (let ()
     (reset-name-counter 0)
     (check-equal? (new-tfield "a label") (tfield "a label" "tfield0" #f))
     (check-equal? (new-tfield "a label" #:name "tf1" #:error '(b "wrong"))
                   (tfield "a label" "tf1" '(b "wrong")))
     (check-equal? (new-tfield/number "lab3" 5)
                   (tfield/number "lab3" "tfield1" #f 5 #f))
     (check-equal? (new-tfield/string "lab4" "v" #t #:error '(b "wrong"))
                   (tfield/string "lab4" "tfield2" '(b "wrong") "v" #t))
     (check-equal? (new-tfield/boolean "lab5" #:name "hi")
                   (tfield/boolean "lab5" "hi" #f #f))
     (check-equal? (new-tfield/struct "entry" entry 
                                      (list (new-tfield/string "name" #:name "a")
                                            (new-tfield/number "number" #:name "b"))
                                      #:name "hi")
                   (tfield/struct "entry" "hi" #f entry 
                                  (list (tfield/string "name" "a" #f #f #f)
                                        (tfield/number "number" "b" #f #f #f))))
     (check-equal? (new-tfield/oneof "num or str"
                                     (list (new-tfield/number "num" #:name "c")
                                           (new-tfield/string "str" #:name "d"))
                                     1 #:name "hi")
                   (tfield/oneof "num or str" "hi" #f
                                 (list (tfield/number "num" "c" #f #f #f)
                                       (tfield/string "str" "d" #f #f #f))
                                 1))
     (check-exn exn:fail? (λ() (new-tfield/oneof "num or str"
                                                 (list (new-tfield/number "num" #:name "c")
                                                       (new-tfield/string "str" #:name "d"))
                                                 2 #:name "hi")))
     (check-equal? (new-tfield/oneof "num or str"
                                     (list (new-tfield/number "num" #:name "c")
                                           (new-tfield/string "str" #:name "d"))
                                     #:name "hi")
                   (tfield/oneof "num or str" "hi" #f
                                 (list (tfield/number "num" "c" #f #f #f)
                                       (tfield/string "str" "d" #f #f #f))
                                 #f))
     (check-equal? (new-tfield/listof "numbers"
                                      (new-tfield/number "num" #:name "e")
                                      #:name "f")
                   (tfield/listof "numbers" "f" #f 
                                  (tfield/number "num" "e" #f #f #f) empty))
     )))


(define tfield/base-tests
  (test-suite
   "Tests for tfield"
   (let ([tf1 (tfield "a label" "tfield0" #f)]
         )
     ; filled?
     (check-exn exn:fail? (λ() (filled? tf1)))
     
     ; tfield->value
     (check-exn exn:fail? (λ() (tfield->value tf1)))
     
     ; rename/deep
     (check-exn exn:fail? (λ() (rename/deep tf1 "hi")))
     )))


(define tfield/number-tests
  (test-suite
   "Tests for tfield/number"
   (let ([tfn1 (new-tfield/number "a num" 5 #:name "a")]
         [tfn2 (new-tfield/number "a num" #:name "b")]
         [tfn3 (new-tfield/number "a num" #f "x")]
         [tfn4 (new-tfield/number "a num" 4 "4")]
         
         [f  (λ(n) (match n ["a" "9"] ["b" "hf"] [_ #f]))]
         )
     ; clear
     (check-equal? (clear tfn4) 
                   (new-tfield/number "a num" #:name (tfield-name tfn4)))
     
     ; filled?
     (check-true (filled? tfn1))
     (check-false (filled? tfn2))
     (check-false (filled? tfn3))
     
     ; tfield->value
     (check-equal? (tfield->value tfn1) 5)
     (check-exn exn:fail? (λ() (tfield->value tfn2)))
     (check-exn exn:fail? (λ() (tfield->value tfn3)))
     (check-equal? (tfield->value tfn4) 4)
     
     ; value->tfield
     (check-equal? (value->tfield tfn1 7) (new-tfield/number "a num" 7 "7" #:name "a"))
     (check-false (value->tfield tfn1 "f"))
     (check-equal? (value->tfield tfn2 5) (new-tfield/number "a num" 5 "5" #:name "b"))
     
     ; rename/deep
     (check-equal? (rename/deep tfn1 "x")
                   (new-tfield/number "a num" 5 #:name "x"))
     (check-equal? tfn1 (new-tfield/number "a num" 5 #:name "a"))
     
     ; parse
     (check-equal? (parse tfn1 f)
                   (new-tfield/number "a num" 9 "9" #:name "a"))
     (check-equal? (parse tfn2 f) 
                   (new-tfield/number "a num" #f "hf" 
                                      #:name "b" #:error ERRMSG/NOT-NUMBER))
     (check-equal? (parse tfn2 f #f) (new-tfield/number "a num" #f "hf" #:name "b"))
     (check-equal? (parse tfn3 f)
                   (new-tfield/number "a num" #f #f
                                      #:name (tfield-name tfn3)
                                      #:error ERRMSG/NOT-FILLED))
     (check-equal? (parse tfn3 f #f) 
                   (new-tfield/number "a num" #f #f #:name (tfield-name tfn3))
                   )
     )))


(define tfield/string-tests
  (test-suite
   "Tests for tfield/string"
   (let ([tfs1 (new-tfield/string "a str" #:name "a")]
         [tfs2 (new-tfield/string "a str" "hi" #:name "b")]
         [tfs3 (new-tfield/string "a str" #f #t)]
         [tfs4 (new-tfield/string "a str" "" #t #:name "b")]
         [tfs5 (new-tfield/string "a str" "hi" #t #:name "z")]
         
         [f  (λ(n) (match n ["a" "5"] ["b" "hf"] [_ #f]))]
         [g  (λ(n) (match n ["a" ""] ["b" ""] [_ #f]))]
         )
     
     ; parse
     (check-equal? (parse tfs1 f)
                   (new-tfield/string "a str" "5" #:name "a"))
     (check-equal? (parse tfs2 f)
                   (new-tfield/string "a str" "hf" #:name "b"))
     (check-equal? (parse tfs1 g)
                   (new-tfield/string "a str" "" #:name "a"))
     (check-equal? (parse tfs4 g)
                   (new-tfield/string "a str" "" #t
                                      #:name "b"
                                      #:error ERRMSG/NOT-FILLED))
     (check-equal? (parse tfs4 g #f) tfs4)
     (check-equal? (parse tfs5 f)
                   (new-tfield/string "a str" #f #t 
                                      #:name "z"
                                      #:error ERRMSG/NOT-FILLED))
     (check-equal? (parse tfs5 f #f) (new-tfield/string "a str" #f #t #:name "z"))
     (check-equal? (parse (new-tfield/string "a str" #:name "z") f #f)
                   (new-tfield/string "a str" #f #f #:name "z"))
                                      
                   
     ; filled?
     (check-true (filled? tfs2))
     (check-true (filled? tfs5))
     (check-false (filled? tfs1))
     (check-false (filled? tfs3))
     (check-false (filled? tfs4))
     
     ; tfield->value
     (check-exn exn:fail? (λ() (tfield->value tfs1)))
     (check-exn exn:fail? (λ() (tfield->value tfs3)))
     (check-exn exn:fail? (λ() (tfield->value tfs4)))
     (check-equal? (tfield->value tfs2) "hi")
     (check-equal? (tfield->value tfs5) "hi")
     (check-equal? (tfield->value (new-tfield/string "a string" "")) "")
     
     ; value->tfield
     (check-false (value->tfield tfs1 5))
     (check-equal? (value->tfield tfs1 "hello") 
                   (new-tfield/string "a str" "hello" #:name "a"))
     (check-equal? (value->tfield tfs2 "bye")
                   (new-tfield/string "a str" "bye" #:name "b"))
     (check-false (value->tfield tfs3 ""))
     (check-equal? (tfield->value (value->tfield tfs3 "b")) "b")
     (check-equal? (value->tfield tfs2 "")
                   (new-tfield/string "a str" "" #:name "b"))
     
     ; rename/deep
     (check-equal? (rename/deep tfs5 "x")
                   (new-tfield/string "a str" "hi" #t #:name "x"))
     (check-equal? tfs5  ; unchanged
                   (new-tfield/string "a str" "hi" #t #:name "z"))
     )))


(define tfield/symbol-tests
  (test-suite
   "Tests for tfield/symbol"
   (let ([tfs1 (new-tfield/symbol "a sym" #:name "a")]
         [tfs2 (new-tfield/symbol "a sym" 'hi #:name "b")]
         [tfs3 (new-tfield/symbol "a sym" 'hi #:name "z")]
         
         [f  (λ(n) (match n ["a" "5"] ["b" "hf"] [_ #f]))]
         [g  (λ(n) (match n ["a" ""] ["b" ""] [_ #f]))]
         )
     
     ; parse
     (check-equal? (parse tfs1 f)
                   (new-tfield/symbol "a sym" '|5| #:name "a"))
     (check-equal? (parse tfs2 f)
                   (new-tfield/symbol "a sym" 'hf #:name "b"))
     (check-equal? (parse tfs1 g)
                   (new-tfield/symbol "a sym" #f
                                      #:name "a"
                                      #:error ERRMSG/NOT-FILLED))
     (check-equal? (parse (new-tfield/symbol "a sym" #:name "z") f #f)
                   (new-tfield/symbol "a sym" #f #:name "z"))
     (check-equal? (parse (new-tfield/symbol "a sym" #:name "z") f)
                   (new-tfield/symbol "a sym" #f #:name "z"
                                      #:error ERRMSG/NOT-FILLED))
                                      
     ; filled?
     (check-false (filled? tfs1))
     (check-true (filled? tfs2))
     (check-true (filled? tfs3))
     
     ; tfield->value
     (check-exn exn:fail? (λ() (tfield->value tfs1)))
     (check-equal? (tfield->value tfs2) 'hi)
     
     ; value->tfield
     (check-false (value->tfield tfs1 5))
     (check-false (value->tfield tfs1 "hello"))
     (check-equal? (value->tfield tfs1 'hello)
                   (new-tfield/symbol "a sym" 'hello #:name "a"))
     (check-equal? (value->tfield tfs2 'bye)
                   (new-tfield/symbol "a sym" 'bye #:name "b"))
     (check-false (value->tfield tfs3 ""))
     (check-equal? (tfield->value (value->tfield tfs3 'b)) 'b)
     
     ; rename/deep
     (check-equal? (rename/deep tfs3 "x")
                   (new-tfield/symbol "a sym" 'hi #:name "x"))
     (check-equal? tfs3 ; unchanged
                   (new-tfield/symbol "a sym" 'hi #:name "z"))
     )))


(define tfield/boolean-tests
  (test-suite
   "Tests for tfield/boolean"
   (let ([tfb1 (new-tfield/boolean "a bool" #t #:name "a")]
         [tfb2 (new-tfield/boolean "a bool" #f #:name "b")]
         
         [f  (λ(n) (match n ["b" "on"] [_ #f]))]
         [g  (λ(n) (match n ["a" "off"] [_ #f]))]
         )
     
     ; parse
     (check-equal? (parse tfb1 f)
                   (new-tfield/boolean "a bool" #f 
                                       ;;#:error (list ERRMSG/NOT-FILLED)
                                       #:name "a"))
     (check-equal? (parse tfb1 g)
                   (new-tfield/boolean "a bool" #f #:name "a"))
     (check-equal? (parse tfb2 f)
                   (new-tfield/boolean "a bool" #t #:name "b"))
     (check-equal? (parse tfb2 g #f) tfb2)
     
     ; filled?
     (check-true (filled? tfb1))
     (check-true (filled? tfb2))
     
     ; tfield->value
     (check-equal? (tfield->value tfb1) #t)
     (check-equal? (tfield->value tfb2) #f)
     
     ; value->tfield 
     (check-false (value->tfield tfb1 "hi"))
     (check-equal? (value->tfield tfb1 #f) (new-tfield/boolean "a bool" #f #:name "a"))
     
     ; rename/deep
     (check-equal? (rename/deep tfb1 "z")
                   (new-tfield/boolean "a bool" #t #:name "z"))
     (check-equal? tfb1 (new-tfield/boolean "a bool" #t #:name "a"))
     )))


(struct cartpt (x y) #:transparent)
(struct entry (name num) #:transparent)

(define tfield/struct-tests
  (test-suite
   "Tests for tfield/struct"
   (let* ([tfs1 (new-tfield/string "name" #:name "a")]
          [tfs2 (new-tfield/string "name" "bob" #:name "b")]
          [tfs3 (new-tfield/string "name" "" #t #:name "c")]
          [tfs4 (new-tfield/string "name" "" #f #:name "d")]
          
          [tfn1 (new-tfield/number "number" #:name "e")]
          [tfn2 (new-tfield/number "number" 5 #:name "f")]
          [tfn3 (new-tfield/number "number" #f "x" #:name "g")]
          [tfn4 (new-tfield/number "number" 7 "7" #:name "h")]
          
          [tfe1 (new-tfield/struct "entry" entry (list tfs1 tfn1) #:name "i")]
          [tfe2 (new-tfield/struct "entry" entry (list tfs2 tfn2) #:name "j")]
          [tfe3 (new-tfield/struct "entry" entry (list tfs2 tfn3) #:name "k")]
          [tfe4 (new-tfield/struct "entry" entry (list tfs3 tfn4) #:name "l")]
          [tfe5 (new-tfield/struct "entry" entry (list tfs4 tfn4) #:name "m")]
          
          [f (λ(n) (match n ["a" "hello"] ["e" "12"] ["f" "x"] [_ #f]))]
          )
     
     ; any-error?
     (check-false (any-error? (parse tfe1 f)))
     (check-true (any-error? (parse tfe2 f)))
     
     ; parse
     (check-equal? (parse tfe1 f)
                   (new-tfield/struct "entry" entry 
                                      (list (new-tfield/string "name" "hello" #:name "a")
                                            (new-tfield/number "number" 12 "12" #:name "e")) 
                                      #:name "i"))
     (check-equal? (parse tfe2 f)
                   (new-tfield/struct "entry" entry 
                                      (list (new-tfield/string "name" #f #f #:name "b"
                                                               #:error
                                                               ERRMSG/NOT-FILLED)
                                            (new-tfield/number "number" #f "x" #:name "f"
                                                               #:error 
                                                               ERRMSG/NOT-NUMBER))
                                      #:name "j"))
     (check-equal? (parse tfe3 f #f) 
                   (new-tfield/struct "entry" entry 
                                      (list (new-tfield/string "name" #f #:name "b") 
                                            (new-tfield/number "number" #f #f #:name "g"))
                                      #:name "k")
                   )
          
     ; filled?
     (check-false (filled? tfe1))
     (check-true (filled? tfe2))
     (check-false (filled? tfe3))
     (check-false (filled? tfe4))
     (check-true (filled? tfe5))
     
     ; tfield->value 
     (check-exn exn:fail? (λ() (tfield->value tfe1)))
     (check-equal? (tfield->value tfe2) (entry "bob" 5))
     (check-equal? (tfield->value tfe5) (entry "" 7))
     
     ; value->tfield
     (check-false (value->tfield tfe1 5))
     (check-false (value->tfield tfe1 "entry"))
     (check-false (value->tfield tfe1 (cartpt "hi" 6)))
     (check-equal? (value->tfield tfe1 (entry "bob" 75))
                   (new-tfield/struct "entry" entry 
                                      (list (new-tfield/string "name" "bob" #:name "a")
                                            (new-tfield/number "number" 75 "75" #:name "e"))
                                      #:name "i"))
                                          
     ; rename/deep
     (check-equal? (rename/deep tfe1 "z")
                   (new-tfield/struct "entry" entry 
                                      (list (new-tfield/string "name" #:name "z-0")
                                            (new-tfield/number "number" #:name "z-1"))
                                      #:name "z"))
      (check-equal? (rename/deep tfe1)
                   (new-tfield/struct "entry" entry 
                                      (list (new-tfield/string "name" #:name "i-0")
                                            (new-tfield/number "number" #:name "i-1"))
                                      #:name "i"))
     (check-equal? tfe1
                   (new-tfield/struct "entry" entry (list tfs1 tfn1) #:name "i"))
     )))


(define tfield/oneof-tests
  (test-suite
   "Tests for tfield/oneof"
   
   (let* ([t1 (new-tfield/number "a number" 4 "4" #:name "a")]
          [t2 (new-tfield/string "a string" #:name "b")]
          
          [t3 (new-tfield/oneof "number or string" (list t1 t2) 0 #:name "c")]
          [t4 (new-tfield/oneof "number or string" (list t1 t2) #:name "d")]
          
          [f   (λ(n) (match n ["a" "5"] ["b" "hf"] ["c" "0"] [_ #f]))]
          [f1  (λ(n) (match n ["a" "5"] ["b" "hf"] ["c" "1"] [_ #f]))]
          [f2  (λ(n) (match n ["a" "x"] ["b" "hf"] ["c" "0"] [_ #f]))]
          [f3  (λ(n) (match n ["a" "x"] ["b" "hf"] ["c" "1"] [_ #f]))]
          [g0  (λ(n) (match n ["a" "5"] ["b" "hf"] ["c" "-"] [_ #f]))]
          [g1  (λ(n) (match n ["a" "5"] ["b" "hf"] [_ #f]))]
          [h0 (λ(n) (match n ["a" "5"]  ["d" "1"]  [_ #f]))]
          [h1 (λ(n) (match n ["d" "0"]  [_ #f]))]
          )
     
     ; any-error?
     (check-true (any-error? (parse t3 f2)))
     (check-true (any-error? (parse t3 g0)))
     (check-false (any-error? (parse t3 f1)))
     
     ; parse
     (check-equal? (parse t3 f)
                   (new-tfield/oneof "number or string" 
                                     (list (new-tfield/number "a number" 5 "5" #:name "a") 
                                           t2) 0 #:name "c"))
     (check-equal? (parse t3 f1)
                   (new-tfield/oneof "number or string" 
                                       ; note: updates both old and new chosen
                                     (list (new-tfield/number "a number" 5 "5" #:name "a") 
                                           (new-tfield/string "a string" "hf" #:name "b"))
                                     1 #:name "c"))
     (check-equal? (parse t3 f2 #f) 
                   (new-tfield/oneof "number or string" 
                                     (list (new-tfield/number "a number" #f "x" #:name "a")
                                           t2) 0 #:name "c")
                   )
     (check-equal? (parse t3 f2)
                   (new-tfield/oneof "number or string" 
                                     (list (new-tfield/number "a number" #f "x" #:name "a"
                                                              #:error
                                                              "Should be a number")
                                           t2)
                                     0 #:name "c"))
     (check-equal? (parse t3 f3 #f)
                   (new-tfield/oneof "number or string" 
                                     (list (new-tfield/number "a number" #f "x" #:name "a")
                                           (new-tfield/string "a string" "hf" #:name "b")
                                           ) 1 #:name "c"))
     (check-equal? (parse t3 g0 #f)
                   (new-tfield/oneof "number or string" (list t1 t2) #f #:name "c"))
     (check-equal? (parse t3 g0)
                   (new-tfield/oneof "number or string" (list t1 t2) #:name "c"
                                     #:error ERRMSG/SELECT-OPTION))
     (check-equal? (parse t3 g1 #f)
                   (new-tfield/oneof "number or string" (list t1 t2) #f #:name "c"))
     (check-equal? (parse t3 g1)
                   (parse t3 g0))
     (check-equal? (parse t4 h0 #f)
                   (new-tfield/oneof "number or string" (list t1 t2) 1 #:name "d"))
     (check-equal? (parse t4 h0)
                   (new-tfield/oneof "number or string" 
                                     (list t1
                                           (new-tfield/string "a string" #:name "b"
                                                              #:error
                                                              ERRMSG/NOT-FILLED))
                                     1 #:name "d"))
     (check-equal? (parse t4 h1 #f) 
                   (new-tfield/oneof "number or string" 
                                     (list (new-tfield/number "a number" #f #f #:name "a")
                                           t2) 0 #:name "d"))
     (check-equal? (parse t4 h1)
                   (new-tfield/oneof "number or string" 
                                     (list (new-tfield/number "a number" #f #f #:name "a"
                                                              #:error 
                                                              ERRMSG/NOT-FILLED)
                                           t2) 
                                     0 #:name "d"))

     ; filled?
     (check-true (filled? t3))
     (check-false (filled? t4))
     
     ; tfield->value
     (check-equal? (tfield->value t3) 4)
     (check-exn exn:fail? (λ() (tfield->value t4)))
     
     ; value->tfield
     (check-false (value->tfield t3 #t))
     (check-equal? (value->tfield t4 5)
                   (new-tfield/oneof "number or string" 
                                     (list (new-tfield/number "a number" 5 "5" #:name "a")
                                           t2) 
                                     0 #:name "d"))
     (check-equal? (value->tfield t4 "hello")
                   (new-tfield/oneof "number or string" 
                                     (list t1
                                           (new-tfield/string "a string" "hello" #:name "b")) 
                                     1 #:name "d"))
     
     ; rename/deep
     (check-equal? (rename/deep t3 "z")
                   (new-tfield/oneof "number or string" 
                                     (list (new-tfield/number "a number" 4 "4" #:name "z-0")
                                           (new-tfield/string "a string" #:name "z-1")) 
                                     0 #:name "z"))
     (check-equal? t3 (new-tfield/oneof "number or string" (list t1 t2) 0 #:name "c"))
     )))




(define tfield/listof-tests
  (test-suite
   "Tests for tfield/listof"
   
   (let* ([t1 (new-tfield/number "a number" 4 "4" #:name "a")]
          [t2 (new-tfield/string "a string" #:name "b")]
          
          [tfe1 (new-tfield/struct "entry" entry (list t1 t2) #:name "i")]
          
          [tf1 (new-tfield/listof "numbers" t1 #:name "c")]
          [tf2 (new-tfield/listof "numbers" t1 (list t1) #:name "c")]
          [tf3 (new-tfield/listof "strings" t2 (list t2 t2) #:name "d")]
          [tf4 (new-tfield/listof "entries" tfe1 empty #:name "e")]
          
          [f (λ(n) (match n
                     ["c" "3"] ["c-0" "11"] ["c-1" "13"] ["c-2" "15"] [_ #f]))]
          [g (λ(n) (match n
                     ["d" "2"] ["d-0" "hi"] ["d-1" #f] [_ #f]))]
          [h (λ(n) (match n
                     ["e" "2"]
                     ["e-0-0" "5"] ["e-0-1" "bob"]
                     ["e-1-0" "7"] ["e-1-1" "alice"] [_ #f]))]
          )
     
     ; update-named
     (check-equal? (update-named tf4 "e"
                                        (λ(f) (struct-copy tfield/listof f [elts (list tfe1)])))
                   (new-tfield/listof "entries" tfe1 (list tfe1) #:name "e"))
     
     ; parse
     (check-equal? (parse (rename/deep tf2) f)
                   (new-tfield/listof 
                    "numbers" (rename/deep t1 "c-base")
                    (list (new-tfield/number "a number" 11 "11" #:name "c-0")
                          (new-tfield/number "a number" 13 "13" #:name "c-1")
                          (new-tfield/number "a number" 15 "15" #:name "c-2"))
                    #:name "c"))
     (check-equal? (parse tf2 g)
                   (new-tfield/listof "numbers" t1 empty #:name "c"))
     (check-equal? (parse (rename/deep tf3) g)
                   (new-tfield/listof 
                    "strings" (rename/deep t2 "d-base")
                    (list (new-tfield/string "a string" "hi" #:name "d-0")
                          (new-tfield/string "a string" 
                                             #:error ERRMSG/NOT-FILLED
                                             #:name "d-1"))
                    #:name "d"))
     (check-equal? (tfield->value (parse (rename/deep tf4) h))
                   (list (entry 5 "bob") (entry 7 "alice")))
     
     
     ; filled?
     (check-true (filled? tf1))
     (check-true (filled? tf2))
     (check-false (filled? tf3))
     
     ; tfield->value
     (check-equal? (tfield->value tf1) empty)
     (check-equal? (tfield->value tf2) (list 4))
     (check-exn exn:fail? (λ() (tfield->value tf3)))
     
     ; value->tfield
     (check-false (value->tfield tf2 "hi"))
     (check-false (value->tfield tf2 (list 5 "hi")))
     (check-equal? (value->tfield tf2 empty)
                   (new-tfield/listof "numbers" t1 (list ) #:name "c"))
     (check-equal? (value->tfield tf2 (list 13 7 9))
                   (new-tfield/listof "numbers" t1
                                      (list 
                                       (new-tfield/number "a number" 13 "13" #:name "c-0")
                                       (new-tfield/number "a number" 7 "7" #:name "c-1")
                                       (new-tfield/number "a number" 9 "9" #:name "c-2"))
                                      #:name "c"))
     
     ; rename/deep
     (check-equal? (rename/deep tf2 "z")
                   (new-tfield/listof "numbers"
                                      (new-tfield/number "a number" 4 "4" #:name "z-base") 
                                      (list (new-tfield/number "a number" 4 "4" #:name "z-0"))
                                      #:name "z"))
     (check-equal? tf2 (new-tfield/listof "numbers" 
                                          (new-tfield/number "a number" 4 "4" #:name "a")
                                          (list (new-tfield/number "a number" 4 "4"
                                                                   #:name "a")) 
                                          #:name "c"))
     )))


; samp-func : (listof num) string -> #f or entry
(define (samp-func nums name)
  (if (< (apply + nums) 0) #f
      (entry (apply + nums) name)))


(define tfield/function-tests
  (test-suite
   "Tests for tfield/function"
   (let* ([tn1 (new-tfield/number "a number" 4 "4" #:name "a")]
          [ts1 (new-tfield/string "a string" #f #t #:name "b")]
          [tl1 (new-tfield/listof "numbers" tn1 #:name "c")]
          
          [tc1 (new-tfield/const "failure" #f)]
          [te1 (new-tfield/struct "entry" entry (list tn1 ts1) #:name "i")]
          
          [to1 (new-tfield/oneof "#f or string" (list tc1 te1) #:name "d")]
          
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
     
      ; update-named
     #;(pretty-print
       (update-named 
                      (rename/deep tf2) "h-0"
                      (λ(f) (struct-copy tfield/listof f
                                         [elts (bump-up (list-ref (tfield/listof-elts f) 1)
                                                        (tfield/listof-elts f))]))))
     (check-equal? (tfield->value
                    (first (tfield/function-args
                     (update-named 
                      (rename/deep tf2) "h-0"
                      (λ(f) (struct-copy tfield/listof f
                                         [elts (bump-up (list-ref (tfield/listof-elts f) 1)
                                                        (tfield/listof-elts f))]))))))
                   (list 40 30 24))     
     
     ; clear
     (check-equal? (clear tf1)
                   (new-tfield/function 
                    title text samp-func 
                    (list (new-tfield/listof "numbers" 
                                             (new-tfield/number 
                                              "a number" #:name "a")
                                             #:name "c")
                          ts1)
                    (new-tfield/oneof
                     "#f or string" 
                     (list tc1 
                           (new-tfield/struct 
                            "entry" entry (list (new-tfield/number 
                                                 "a number" #:name "a")
                                                ts1) #:name "i"))
                     #:name "d")
                    #:name "g"))
     
     ; filled?
     (check-false (filled? tf1))
     (check-true (filled? tf2))
                   
     ; tfield->value
     (check-exn exn:fail? (λ() (tfield->value tf1)))
     (check-equal? (tfield->value tf2) (entry 94 "bob"))
     ; (pretty-print tf2) wow!
     
     ; value->tfield
     (check-false (value->tfield tf1 "hi"))
     (check-equal? (value->tfield tf1 (list (list 30 40 24) "bob"))
                   (new-tfield/function 
                    title text samp-func
                    (rename/deep*
                     (list (value->tfield tl1 (list 30 40 24)) (value->tfield ts1 "bob"))
                     "g")
                    (clear to1)
                    #:name "g"))
     
     ; extract&apply-args
     (check-equal? (extract&apply-args samp-func (list tl1 (value->tfield ts1 "bob")) to1)
                   `(success ,(value->tfield to1 (samp-func (list ) "bob"))))
     (check-equal? (extract&apply-args samp-func (list (value->tfield tl1 (list -4 -3 5))
                                                          (value->tfield ts1 "bob")) to1)
                   `(success ,(value->tfield to1 #f)))
     (check-equal? (extract&apply-args samp-func (list tl1 ts1) to1)
                   `(failure ,ERRMSG/MISSING-INPUT))
     (check-equal? (extract&apply-args samp-func (list (value->tfield ts1 "bob")) to1)
                   `(failure ,(string-append ERRMSG/FUNC-APP ": procedure samp-func: expects 2 arguments, given 1: \"bob\"")))
     (check-equal? (extract&apply-args samp-func (list tl1 (value->tfield ts1 "bob")) ts1)
                   `(failure ,ERRMSG/MISMATCH))
     
     )))


(define find-tests
  (test-suite
   "Tests for the find function"
   (let* ([tn1 (new-tfield/number "a number" 4 "4" #:name "a")]
          [ts1 (new-tfield/string "a string" #f #t #:name "b")]
          [tl1 (new-tfield/listof "numbers" tn1 #:name "c")]
          
          [tc1 (new-tfield/const "failure" #f)]
          [te1 (new-tfield/struct "entry" entry (list tn1 ts1) #:name "i")]
          
          [to1 (new-tfield/oneof "#f or string" (list tc1 te1) #:name "d")]
          
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
     
     (check-equal? (find-named tn1 "a") tn1)
     (check-equal? (find-named tn1 "b") #f)
     (check-equal? (find-named to1 "b") ts1)
     (check-equal? (find-named tf1 "zz") #f)
     (check-equal? (find-named tf1 "c") tl1)
     (check-equal? (find-named tf1 "a") tn1)

     )))


(define tfield-tests
  (test-suite
   "All tfield tests"
   misc-tests
   constr-tests
   tfield/base-tests
   tfield/number-tests
   tfield/string-tests
   tfield/symbol-tests
   tfield/boolean-tests
   tfield/struct-tests
   tfield/oneof-tests
   tfield/listof-tests
   tfield/function-tests
   
   find-tests
   ))


(run-tests tfield-tests)


;;==============================================================================
;;==============================================================================
;;==============================================================================

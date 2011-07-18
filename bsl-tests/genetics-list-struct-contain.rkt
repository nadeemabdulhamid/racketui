;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname genetics-list-struct-contain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        CSC120 Lab 7 - Lists of Structs with application in Genetics        ;;
;;                        (adopted from Kathi Fisler, WPI)                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; IMAGES HAVE BEEN DELETED FROM THIS SOLUTION FILE


;;
;; There are four possible outcomes from combining these genes. In the figure 
;; below, genes represented by a capital letter are dominant, lowercase 
;; letters are recessive, and an underscore (_) means that it can be either 
;; dominant or recessive.
;;
;;                ee                               E_
;;     (no dark pigment in fur)           (dark pigment in fur)
;;          /            \                    /           \
;;         /              \                  /             \
;;       eebb            eeB_              E_bb           E_B_ 
;;    yellow coat,   yellow coat,       brown coat,    black coat,
;;     brown nose     black nose        brown nose     black nose
;;
;; In order to keep track of the dogs in your kennel, you will use a structure
;; called "lab" that stores the name, gender, age, the E gene sequence, and the
;; B gene sequence of each dog.


;; a lab is a 
;; (make-lab string string number boolean gene-seq gene-seq)
(define-struct lab (name gender age local? E-seq B-seq))
;;  just throwing in a boolean field 'local?' for testing the web interface

;; where a gene-seq is a
;; (make-gene-seq symbol symbol)
(define-struct gene-seq (first second))




;; You will also need a structure called "kennel" which contains all the dogs
;; you own.

;; a kennel is either
;;  - empty, or
;;  - cons(lab kennel)


;; Your current kennel contains 8 dogs (4 males and 4 females). It is called
;; "my-kennel" and is defined below. You should use this kennel to test all the
;; functions you write for this lab exercise.

(define my-kennel (list (make-lab "ariel" "female" 5 false
                                  (make-gene-seq 'E 'E) (make-gene-seq 'B 'B))
                        (make-lab "belle" "female" 3 true
                                  (make-gene-seq 'E 'e) (make-gene-seq 'b 'b))
                        (make-lab "cinderella" "female" 6 false
                                  (make-gene-seq 'e 'e) (make-gene-seq 'B 'b))
                        (make-lab "jasmine" "female" 4 false
                                  (make-gene-seq 'e 'e) (make-gene-seq 'b 'b))
                        (make-lab "goofy" "male" 3 true
                                  (make-gene-seq 'E 'e) (make-gene-seq 'B 'b))
                        (make-lab "hercules" "male" 4 true
                                  (make-gene-seq 'E 'E) (make-gene-seq 'b 'b))
                        (make-lab "mickey" "male" 1 true
                                  (make-gene-seq 'e 'e) (make-gene-seq 'B 'B))
                        (make-lab "aladdin" "male" 6 false
                                  (make-gene-seq 'e 'e) (make-gene-seq 'b 'b))))

;; Exercises:
;; 1. Write the templates for functions over kennels and labs

;; lab-func : lab ... -> ...
;; purpose ....
;(check-expect (lab-func ...) ...)

#;(define (lab-func a-lab ...)
  ... (lab-name a-lab) ...
  ... (lab-gender a-lab) ...
  ... (lab-age a-lab) ...
  ... (lab-E-seq a-lab) ...
  ... (lab-B-seq a-lab) ...
  )

;; kennel-func : kennel ... -> ...
;; purpose ...
; (check-expect (kennel-func ...) ...)

#;(define (kennel-func a-kennel ...)
    (cond [(empty? a-kennel) ...]
          [(cons? a-kennel) ... (first a-kennel) ...
                            ... (kennel-func (rest a-kennel) ...) ...
                            ]))


;; 2. Write a function count-males that consumes a kennel and returns the number
;;    of males in that kennel.

(check-expect (count-males my-kennel) 4)

(define (count-males a-kennel)
    (cond [(empty? a-kennel) 0]
          [(cons? a-kennel) (+ (if (string=? "male" (lab-gender (first a-kennel))) 1 0)
                               (count-males (rest a-kennel)))
                            ]))

;; 3. Write a function breeding-age-dogs that consumes a kennel and returns that
;;    kennel with just the dogs that are of breeding age (between 2 and 5 years
;;    old, inclusive)

(check-expect (breeding-age-dogs my-kennel) 
              (list (make-lab "ariel" "female" 5 false
                                  (make-gene-seq 'E 'E) (make-gene-seq 'B 'B))
                        (make-lab "belle" "female" 3 true
                                  (make-gene-seq 'E 'e) (make-gene-seq 'b 'b))
                        (make-lab "jasmine" "female" 4 false
                                  (make-gene-seq 'e 'e) (make-gene-seq 'b 'b))
                        (make-lab "goofy" "male" 3 true
                                  (make-gene-seq 'E 'e) (make-gene-seq 'B 'b))
                        (make-lab "hercules" "male" 4 true
                                  (make-gene-seq 'E 'E) (make-gene-seq 'b 'b))))

(define (breeding-age-dogs a-kennel)
    (cond [(empty? a-kennel) empty]
          [(cons? a-kennel)  (if (<= 2 (lab-age (first a-kennel)) 5)
                                 (cons (first a-kennel) (breeding-age-dogs (rest a-kennel)))
                               (breeding-age-dogs (rest a-kennel)))
                            ]))



;; 4. Write a function breed that consumes two labs and returns "light" if the 
;;    puppies of those two dogs would be light-colored, "dark" if they would be
;;    dark-colored, or "either" if they could be either light or dark. The color
;;    of the puppies is determined by one version of a gene from the mother and 
;;    one from the father. Both versions are equally likely to be chosen. 
;;
;;    Assume the dogs passed in will be male and female.
;;
;;    Hint 1: It may help to sketch out several tables like this first
;;                     Dog1
;;               ---------------
;;               |   |  E |  e |
;;            D  ---------------
;;            o  | E | EE | Ee |
;;            g  ---------------
;;            2  | e | eE | ee |
;;               ---------------
;;
;;                     Dog1
;;               ---------------
;;               |   |  E |  e |
;;            D  ---------------
;;            o  | e | Ee | ee |
;;            g  ---------------
;;            2  | e | Ee | ee |
;;               ---------------
;;
;;                     Dog1
;;               ---------------
;;               |   |  E |  E |
;;            D  ---------------
;;            o  | E | EE | EE |
;;            g  ---------------
;;            2  | E | EE | ee |
;;               ---------------
;;
;;   ee/ee  ee/Ee   ee/EE   Ee/ee  Ee/Ee   Ee/EE    EE/..
;; both ee --> light
;; any EE  --> dark
;; any Ee  --> either


(define (matches e-seq first-sym second-sym)
  (and (symbol=? (gene-seq-first e-seq) first-sym)
       (symbol=? (gene-seq-second e-seq) second-sym)))

;; Everybody should be able to finish up to this point.

(check-expect (breed (first my-kennel) (fifth my-kennel)) "dark")
(check-expect (breed (first my-kennel) (seventh my-kennel)) "dark")
(check-expect (breed (second my-kennel) (seventh my-kennel)) "either")
(check-expect (breed (second my-kennel) (sixth my-kennel)) "dark")
(check-expect (breed (third my-kennel) (eighth my-kennel)) "light")
(check-expect (breed (third my-kennel) (sixth my-kennel)) "dark")

(define (breed male-lab female-lab)
  (cond [(and (matches (lab-E-seq male-lab) 'e 'e)
              (matches (lab-E-seq female-lab) 'e 'e)) "light"]
        [(or (matches (lab-E-seq male-lab) 'E 'E)
             (matches (lab-E-seq female-lab) 'E 'E)) "dark"]
        [else "either"]))





;; 5. You want to mate Belle with another dog, but you want to make sure all the
;;    puppies come out dark (either brown or black). Write a function find-mate
;;    that takes in a lab (in this case, Belle), a desired coat shade (dark, 
;;    light, or either), and a kennel and finds a mate for the given dog. You 
;;    only need to return one mate. Don't forget to take gender into 
;;    consideration.

(check-expect (find-mate (make-lab "belle" "female" 3 true
                                   (make-gene-seq 'E 'e) (make-gene-seq 'b 'b))
                         my-kennel)
              (list (make-lab "hercules" "male" 4 true
                              (make-gene-seq 'E 'E) (make-gene-seq 'b 'b))
                    ))

(define (find-mate fem ken)
  (cond [(empty? ken) empty]
        [(cons? ken) (if (and (string=? "male" (lab-gender (first ken)))
                              (string=? "dark" (breed (first ken) fem)))
                         (cons (first ken) (find-mate fem (rest ken)))
                         (find-mate fem (rest ken)))]))
              




(require racket/base)
(require (planet nah22/racketui/web-launch))

(define/web gene-seq/web
  (structure make-gene-seq ["First gene" symbol] ["Second gene" symbol]))

(define/web gender/web
  "Gender"
  (oneof ["Female" (constant "female")] ["Male" (constant "male")]))

(define/web lab/web
  (structure make-lab ["Name" string] gender/web ["Age" number] ["Local?" boolean]
             ["E-sequence" gene-seq/web] ["B-sequence" gene-seq/web]))

(define/web female-lab/web
  (structure make-lab ["Name" string] ["Gender" (constant "female")] ["Age" number]
             ["Local?" boolean]
             ["E-sequence" gene-seq/web] ["B-sequence" gene-seq/web]))
             
(define/web matefinder
  "Mate Finder"
  (function "Determines which labs in the kennel will mate to produce dark puppies (guaranteed)."
           (find-mate ["Female lab" female-lab/web]
                      ["Kennel (all labs)" (listof ["Lab" lab/web])]
                      -> ["Compatible males" (listof ["Lab" lab/web])])))

(web-launch matefinder)


;; 6. You would like to try to breed white lab puppies. These are very rare, and
;;    are actually very light yellow in color. They have the same genetic 
;;    sequence as yellow puppies with brown noses (the recessive combination 
;;    eebb). You read in a dog breeder's magazine that approximately 10% of all
;;    puppies with the eebb sequence come out white. You would like to use this
;;    information to find the best two dogs to mate to increase the chance of 
;;    having white puppies.
;;
;;
;;    a.) Write a function prob-recessive which consumes two genetic sequences
;;        from parents and a recessive gene value, such as "e", and returns the 
;;        probability of getting a completely recessive sequence (i.e. ee or bb)
;;        from combining the two given ones.
;;
;;    b.) Using the function you just wrote, write another function prob-white
;;        that takes two labs and returns the probability that, if bred, they
;;        would produce a white puppy.
;;
;;    c.) You want to find a mate for Jasmine from your kennel that would have the
;;        greatest chance of producing white puppies with her. Write a function 
;;        find-white-pup-mate that takes in a lab and a kennel and returns the
;;        dog of breeding age (and opposite sex) that has the greatest chance of 
;;        breeding white puppies with the given lab. If there is no possibility that
;;        that would produce white puppies, return "none".

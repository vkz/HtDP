;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstractions) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; [X Y] [List-of X] -> [List-of Y]
; given a list of positive numbers (US dollar amounts)
; converts them into EUR assuming 1 USD = 0.90 EUR
(define (translate lox)
  (local (;[X Y] [Posn (make-posn X Y)] -> [List-of X Y]
          (define (posn-to-list p) (list (posn-x p) (posn-y p))))
  (map posn-to-list lox)))


(define-struct toy (name descr cost price))
; toy is a struct

; [List-of Toy] (Number Number -> Boolean) -> List-of Toy
; returns the list of toys sorted by the (- price cost)
(define (sort-by-margin lot order)
  (local (;Toy -> Number
          (define (toy-margin t) (- (toy-price t) (toy-cost t)))
          
          ; Toy Toy -> Boolean
          (define (cmp t1 t2) (order (toy-margin t1) (toy-margin t2))))
    
    (sort lot cmp)))

; [List-of Toy] Number -> [List-of Toy]
; filters out those toys with prices above ua (Number)
(define (eliminate-exp lot ua)
  (local (;Toy -> Boolean
          ;true if (<= toy-price ua)
          (define (inexpensive? t) (<= (toy-price t) ua)))
    (filter inexpensive? lot)))

(check-expect (eliminate-exp TOYS 4) empty)
(check-expect (eliminate-exp TOYS 6) (list (third TOYS)))

(define TOYS (list (make-toy "car" "a toy car" 5 7.5)
                   (make-toy "doll" "a toy doll" 6 7)
                   (make-toy "pistol" "a toy pistol" 3 6)))
  
(check-expect (sort-by-margin TOYS >) (list (third TOYS)
                                            (first TOYS)
                                            (second TOYS)))
(check-expect (sort-by-margin TOYS <) (reverse (list (third TOYS)
                                            (first TOYS)
                                            (second TOYS))))

(check-expect (translate (list (make-posn 1 2) (make-posn "a" "b"))) 
              (list (list 1 2) (list "a" "b")))

; [X Y] [List-of X] [List-of Y] -> [List-of Y]
; returns a list of only those names in the second list that are also in the first
(define (selection lon1 lon2) 
  (local (; X -> Boolean
          ; true if X is in the lon1
          (define (in-first? x)
            (local (; X -> Boolean
                    ; checks if the first argument equals x
                    (define (is-x? y) (equal? y x)))
                    
            (ormap is-x? lon1))))
          
    (filter in-first? lon2)))

(check-expect (selection (list "alex" "sara")
                         (list "fergie" "holly" "sara")) (list "sara"))
(check-expect (selection (list "alex" "sara")
                         (list "fergie" "holly")) empty)


(define (return-self n) n)
; Lenght Constant -> List-of Constant of a given Length
(define (list-of-const len c)
  (local (;Number -> Const
          ;return a constant
          (define (return-const x) c))
    (build-list len return-const)))
          

;(list 1 1/10 1/100 1/1000 ... 1/10^n)
; n -> 10^n
; 5 -> 100000
; (foldr lon 10 *)
(define (list-div-10 n)
  (local (;Number -> 10
          (define (const10 n) 10)
          
          ;Number -> Number
          ;returns 10^n
          (define (div-pow10 N)
            (/ 1 (foldr * 1 (build-list N const10)))))
          
    (build-list n div-pow10)))

(check-expect (list-div-10 3) (list 1 1/10 1/100))


; Number -> List-of Number
; returns the list of first n even numbers
(define (list-of-even N)
  (local ((define (get-even n) (* 2 n)))
    (build-list N get-even)))

; Number -> List-of [List-of Bits]
; Given n returns and nxn diagonal matrix with 1 on the diagonal and 0 elsewhere
; given 2 -> (list (list 1 0) (list 0 1))
; given 3 -> (list (list 1 0 0) (list 0 1 0) (list 0 0 1))
(define (diagonal N)
  (local (;Number -> List-of Number
          ;given n produce a list of length N with 1 set at place n
          (define (set-bit n)
            (local ((define (set-1 m) (if (equal? m n) 1 0)))
              (build-list N set-1))))
    
    (build-list N set-bit)))



; String String -> Boolean
; checks if second string equals or extends the first one
(define (str-extends? s1 s2) 
  (and (<= (string-length s1) (string-length s2)) 
       (string=? s1 (substring s2 0 (string-length s1)))))

(check-expect (str-extends? "alex" "alexis") true)
(check-expect (str-extends? "alex" "ale") false)
(check-expect (str-extends? "alex" "alex") true)
(check-expect (str-extends? "alex" "alesandr") false)


(define (name-match? s los)
  (local ((define (s-ext nm) (str-extends? s nm)))
    (ormap s-ext los)))

; [List-of Any] [List-of Any] -> [List-of Any]
; appends two lists
(define (append-from-fold prel postl)
  (foldr cons postl prel))

(check-expect (append-from-fold (list 1 2 3 4) (list 5 6 7)) (list 1 2 3 4 5 6 7))

; (X -> Y) Y [List-of X] -> [List-of Y]
; map function using fold
(define (map-from-fold f lox)
  (local (; X X -> List-of Y
          (define (cons-for-map pre post) (cons (f pre) post))
          )
    (foldr cons-for-map empty lox)))

(check-expect (map-from-fold add1 (list 1 2 3)) (map add1 (list 1 2 3)))



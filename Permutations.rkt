;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Permutations) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; A Word is one of:
; - empty or
; - (cons 1String Word)

; A List-of-words is one of:
; - empty or
; - (cons Word List-of-words)

; 1String List-of-words -> List-of-words
; appends 1String at the beginning of every word in the List-of-words
(define (append/at0 s low)
  (cond 
    [(empty? low) empty]
    [else (append (list (append (list s) (first low))) 
                  (append/at0 s (rest low))) ]))

(check-expect (append/at0 "d" empty) empty)
(check-expect (append/at0 "d" (list (list "e"))) (list (list "d" "e")))
(check-expect (append/at0 "d" (list (list "e") (list "r"))) 
              (list (list "d" "e") (list "d" "r")))

; 1String Word -> List-of-words
; insert a given letter between all letters in the Word, at the beginning, at the end
; and return a List-of-words produced that way
(define (insert-in-word s w)
  (cond
    [(empty? w) (list (list s))]
    [else (append (list (append (list s) w)) 
                  (append/at0 (first w) (insert-in-word s (rest w))))]))
     
(check-expect (insert-in-word "d" empty)
              (list (list "d")))
(check-expect (insert-in-word "d" (list "e"))
              (list (list "d" "e")
                    (list "e" "d")))
(check-expect (insert-in-word "d" (list "e" "r"))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")))


; 1String List-of-words -> List-of-words
; where 1String is placed in all possible positions in all Words in the List-of-words
; places include 0 and (length Word)
(define (insert-everywhere/in-all-words s low)
  (cond
    [(empty? low) empty]
    [else (append (insert-in-word s (first low)) 
                  (insert-everywhere/in-all-words s (rest low)))]))


(check-expect (insert-everywhere/in-all-words "d" empty)
              empty)

(check-expect (insert-everywhere/in-all-words "d" (list (list "e")))
              (list (list "d" "e")
                    (list "e" "d")))

(check-expect (insert-everywhere/in-all-words "d" (list (list "e" "r")
                                                        (list "r" "e")))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))

; Word -> List-of-words
; Return a list of words that are all possible permutations of the letters in the Word
(define (arrangements w) 
  (cond
    [(empty? w) (list empty)]
    [else (insert-everywhere/in-all-words (first w) (arrangements (rest w)))]))


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |descendant-family-tree with mutators|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "gui.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "gui.ss" "teachpack" "htdp")))))
;; Mutually recursive definitions
;; A list-of-children is either
;; - empty or
;; - (cons p loc) where p is a parent and loc is a list of children.
;;
;; A parent is a structure
;; - (make-parent list-of-children symbol number symbol number)
;; where the last field (nd) is the total number of descendants for that family member
(define-struct parent (children name date eyes nd))

(define Gustav (make-parent empty 'Gustav 1988 'brown 0))
(define Fred&Eva (list Gustav))
;; Middle Generation:
(define Adam (make-parent empty 'Adam 1950 'yellow 0))
(define Dave (make-parent empty 'Dave 1955 'black 0))
(define Eva (make-parent Fred&Eva 'Eva 1965 'blue 0))
(define Fred (make-parent Fred&Eva 'Fred 1966 'pink 0))
(define Carl&Bettina (list Adam Dave Eva))
;; Oldest Generation:
(define Carl (make-parent Carl&Bettina 'Carl 1926 'green 0))
(define Bettina (make-parent Carl&Bettina 'Bettina 1926 'green 0))

;; descendants#-for-parent: parent -> number
;; traverse the descendant family tree and count the total number of descendants for each member
;; return the total number of descendants for the given parent
;; effect : replace the parent-nd field of each family member with its number of descendants
(define (descendants#-for-parent! p)
  (begin 
    (set-parent-nd! p (+ (length (parent-children p))
                         (descendants#-for-children (parent-children p))))
    (parent-nd p)))

;; descendants#-for-children: list-of-children -> number
;; produce a sum of descendants for all children 
(define (descendants#-for-children aloc)
  (cond 
    [(empty? aloc) 0]
    [else (+ (descendants#-for-parent! (first aloc))
             (descendants#-for-children (rest aloc)))]))

(check-expect (descendants#-for-parent! Fred) 1)
(check-expect (descendants#-for-parent! Gustav) 0)
(check-expect (descendants#-for-parent! Carl) 4)
    
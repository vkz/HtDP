;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ListReverse) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define LST (cons "a" (cons "b" (cons "c" empty))))
(define LST2 (cons 1 (cons 2 LST)))

; List-of-strings -> List-of-strings
; reverses the list of strings
(check-expect (rev LST) (reverse LST))
(check-expect (rev LST2) (reverse LST2))
(define (rev l)
  (cond
    [(empty? l) empty]
    [else (add-to-end (first l) (rev (rest l)))]))

; String List-of-strings -> List-of-strings
; appends a given string at the end of the list
(check-expect (add-to-end "c" (cons "a" (cons "b" empty))) LST)

(define (add-to-end s l) 
  (cond
    [(empty? l) (cons s empty)]
    [else (cons (first l) (add-to-end s (rest l))) ]))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname qsort) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; qsort: List-of Number -> List-of Number
;; divide and conquer sorting
(define (qsort alon)
  (cond
    [(empty? alon) empty]
    [(empty? (rest alon)) alon]
    [else (append (qsort (list-of-smaller (rest alon) (first alon)))
                  (list (first alon))
                  (qsort (list-of-larger (rest alon) (first alon))))]))

;; list-of-smaller: List-of Number Number -> List-of Number
;; produce the list of only those <= Threshhold
(define (list-of-smaller alon th)
  (filter (lambda (x) (<= x th)) alon))

(check-expect (list-of-smaller '(1 3 2 7 5) 3) '(1 3 2))

;; list-of-larger: List-of Number Number -> List-of Number
;; produce the list of only those > Threshhold
(define (list-of-larger alon th)
  (filter (lambda (x) (> x th)) alon))

(check-expect (list-of-larger '(1 3 2 7 5) 3) '(7 5))

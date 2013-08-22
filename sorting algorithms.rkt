;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |sorting algorithms|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; ========================== sort =========================
;; simple-sort: List-of Number -> List-of Number
;; simplest possible sorting algorithm that makes use of natural recursion
;; and sorts numbers in ascending order
(define (simple-sort alon)
  (cond 
    [(empty? alon) empty]
    [else (local ((define (insert-one anum slon)
                   (cond 
                     [(empty? slon) (list anum)]
                     [else (if (<= anum (first slon))
                               (cons anum slon)
                               (cons (first slon) (insert-one anum (rest slon))))])))
            (insert-one (first alon) (simple-sort (rest alon))))]))

(check-expect (simple-sort (list 3)) (list 3))
(check-expect (simple-sort (list 3 4 2 7)) (list 2 3 4 7))


;; ========================== qsort =========================
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

;; ========================== merge-sort =========================
;; merge-sort: List-of Number -> List-of Number 
;; given a list of numbers first turns it into a list of singles (make-singles)
;; then calls (merge-all-neighbors) that should eventually produce a 
;; (list (list sorted)) making (first merge-all-neighbours) a sorted list
(define (merge-sort lon)
  (first (merge-all-neighbours (make-singles lon))))

;; make-singles: List-of Number -> List-of (List-of Number)
;; given a list turns it into a list of list of single numbers
;; given: (list 1 3 2 6) -> (list (list 1) (list 3) (list 2) (list 6))
(define (make-singles lon)
  (map list lon))

(check-expect (make-singles (list 1 3 2 6)) 
              (list (list 1) (list 3) (list 2) (list 6)))

;; merge-all-neighbours: List-of List-of Number -> List-of List-of Number
;; merge all neighbouring pairs of lists till and repeat that till 
;; what you have is a single (list (list Numbers))

(define (merge-all-neighbours llon)
  (cond
    [(= (length llon) 1) llon]
    [(= (length llon) 2) (list (merge (first llon) (second llon)))]
    [else (merge-all-neighbours (cons (merge (first llon) (second llon))
                    (merge-all-neighbours (rest (rest llon)))))]))

;; merge: List-of Number List-of Number -> List-of Number
;; Merging two lists in ascending order
;; given: '(2) '(1) -> '(1 2)
;; given: '(3 6) '(1 2) -> '(1 . . .| '(3 6) '(2)
;;        '(3 6) '(2)   -> '(1 2 . .| '(3 6) empty
;;        '(3 6) empty  -> '(1 2 3 6)
(define (merge lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [(empty? lon2) lon1]
    [else (cond
            [(< (first lon1) (first lon2)) (cons (first lon1) (merge (rest lon1) lon2))]
            [else (cons (first lon2) (merge lon1 (rest lon2)))])]))
       

;; times-to-sort: Function [List-of [List-of Numbers]] -> List-of Numbers
;; apply the Function to every list of numbers timing it and return the
;; list of times it took
(define (times-to-sort sort-f llon)
  (map (lambda (lon) (time (sort-f lon))) llon))

;; [List-of Functions] [List-of [List-of Numbers]] -> [List-of [List-of Times]]
;; applies every given sort to every list of numbers in the given list
;; returning a list of times
(define (main to-test on-tests) on-tests)
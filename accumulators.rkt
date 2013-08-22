;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname accumulators) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; sum : (listof number)  ->  number
;; to compute the sum of the numbers on alon0
(define (sum-a alon0) 
  (local (;; accumulator is the sum of the numbers that preceded
          ;; those in alon on alon0
          (define (sum-a alon accumulator)
            (cond
              [(empty? alon) accumulator]
              [else (sum-a (rest alon) (+ (first alon) accumulator))])))
    (sum-a alon0 0)))

;; sum : (listof number)  ->  number
;; to compute the sum of the numbers on alon
;; structural recursion 
(define (sum alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum (rest alon)))]))

(define (g-series n)
  (cond
    [(zero? n) empty]
    [else (cons (expt -0.99 n) (g-series (sub1 n)))]))


(define G-sum (sum (g-series #i1000)))
(define G-sum-a (sum-a (g-series #i1000)))

;; ! : N  ->  N
;; to compute n  ·  (n - 1)  ·  ...  ·  2  ·  1
;; structural recursion 
(define (! n)
  (cond
    [(zero? n) 1]
    [else (* n (! (sub1 n)))]))

;; ! : N  ->  N
;; to compute n  ·  (n - 1)  ·  ...  ·  2  ·  1
(define (!-acc n0)
  (local (;; accumulator is the product of all natural numbers in [n0, n)
          (define (!-a n accumulator)
            (cond
              [(zero? n) accumulator]
              [else (!-a (sub1 n) (* n accumulator))])))
    (!-a n0 1)))


(define (test1000 f iterations)
  (cond
    [(zero? iterations) 0]
    [else (+ (f 50) (test1000 f (sub1 iterations)))]))

;; product : listof numbers -> number
;; product of all the numbers in the list
(define (product alon0)
  (local (;; accumulator is a product of all items encountered before (rest alon)
          (define (product-a alon accumulator)
            (cond 
              [(empty? alon) accumulator]
              [else (product-a (rest alon) (* accumulator (first alon)))])))
    (product-a alon0 1)))

(check-expect (product (list 2 2 2 3)) 24)

;; how-many : listof Any -> number
;; how many items on the list
(define (how-many a-list0)
  (local (;count-so-far : listof Any accumulator -> number of items on the list
          (define (count-so-far a-list accumulator)
            (cond 
              [(empty? a-list) accumulator]
              [else (count-so-far (rest a-list) (add1 accumulator))])))
    (count-so-far a-list0 0)))

(check-expect (how-many '(a b c)) 3)
(check-expect (how-many empty) 0)
(check-expect (how-many '(a)) 1)

;; add-to-pi : number -> number
;; pi + number
(define (add-to-pi n0)
  (local ((define (pi-add1 n accumulator)
            (cond 
              [(zero? n) accumulator]
              [else (pi-add1 (sub1 n) (add1 accumulator))])))
    (pi-add1 n0 pi)))
               
;(check-expect (add-to-pi 2) (add1 (add1 pi)))

; make-palindrome : listof any -> listof any
; construct a palindrome by mirroring the list around the last item
(define (make-palindrome a-list0)
  (local ((define (stack-it a-list accumulator)
            (cond
              [(empty? (rest a-list)) accumulator]
              [else (stack-it (rest a-list) (cons (first a-list) accumulator))])))
    (append a-list0 (stack-it a-list0 empty))))

(check-expect (make-palindrome '(a b c)) '(a b c b a))


;; to-ten : listof digits number -> number
;; given a list of digits and base produce the corresponding number
(define (to10 alod0 base)
  (local ((define (next-d alod accumulator)
            (cond
              [(empty? (rest alod)) (+ accumulator (first alod))]
              [else (next-d (rest alod) (* (+ accumulator (first alod)) base))])))
    (next-d alod0 0)))

(check-expect (to10 (list 1 0 2) 10) 102)
(check-expect (to10 (list 1 0 2) 8) 66)
               
               
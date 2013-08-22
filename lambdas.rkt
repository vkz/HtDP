;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lambdas) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define (convert-eur eur/usd lod)
  (map (lambda (eur) (* eur/usd eur)) lod))

(define (translate-posn lop)
  (map (lambda (p) (list (posn-x p) (posn-y p))) lop))

(define-struct toy (name cost price))

(define (sort-toys cmp lot) 
  (sort lot (lambda (ta tb) (cmp (- (toy-price ta) (toy-cost ta))
                             (- (toy-price tb) (toy-cost tb))))))

(define (selection against lon)
  (filter (lambda (name) (member? name against)) lon))

(define (list-of-10s N) 
  (build-list N (lambda (n) 10)))

(define (list-1-n N)
  (build-list N (lambda (n) (add1 n))))

(define (list-1/10 N)
  (build-list N (lambda (n) (/ 1 (foldr * 1 (list-of-10s n))))))

(define (list-of-even N)
  (build-list N (lambda (n) (* 2 (add1 n)))))

(define (diagonal N)
  (build-list N (lambda (row) (build-list N (lambda (n) (if (= n row) 1 0))))))

(define (extensions? name lon)
  (ormap (lambda (nm) (and (>= (string-length nm) (string-length name))
                           (string=? (substring nm 0 (string-length name))
                                     name))) lon))

(define (append-from-fold pre post)
  (foldr cons post pre))

       
(define (map-from-fold f loany)
  (foldr (lambda (pre post) (cons (f pre) post)) empty loany))


; Even = [Number -> Boolean]

; Base = [Number -> Boolean]

; Number -> Boolean
; checks if the number is between 0 and N-1
(define (make-base N) 
  (lambda (n) (and (<= 0 n) (< n N))))

; Number Base -> Boolean
(define (is-base? n bs)
  (bs n))


; Even = [Number -> Boolean]

(define even 
  (lambda (n) (not (odd? n))))

(define (is-even? n)
  (even n))

; Number Set -> Set
; Where Set = [Number -> Boolean]
(define (add-element n S)
  (lambda (x) (or (equal? x n) (S x))))

(define (union set1 set2)
  (lambda (x) (or (set1 x) (set2 x))))

(define (intersect set1 set2)
  (lambda (x) (and (set1 x) (set2 x))))


; Examples
(define base10 (make-base 10))
(define base3 (make-base 3))
(define even+1 (add-element 1 even))
(define even-upto10 (intersect base10 even))
(define even-or-base10 (union base10 even))
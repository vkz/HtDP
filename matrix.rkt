;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define R1 (cons 11 (cons 12 (cons 13 empty))))
(define R2 (cons 21 (cons 22 (cons 23 empty))))
(define R3 (cons 31 (cons 32 (cons 33 empty))))
(define MAT (cons R1 (cons R2 (cons R3 empty))))

(define T1 (cons 11 (cons 21 (cons 31 empty))))
(define T2 (cons 12 (cons 22 (cons 32 empty))))
(define T3 (cons 13 (cons 23 (cons 33 empty))))
(define TAM (cons T1 (cons T2 (cons T3 empty))))

(define SUBR1 (cons 12 (cons 13 empty)))
(define SUBR2 (cons 22 (cons 23 empty)))
(define SUBR3 (cons 32 (cons 33 empty)))
(define SUBMAT (cons SUBR1 (cons SUBR2 (cons SUBR3 empty))))


; A Matrix is one of:
;  – empty
;  – (cons LN Matrix)
 
; An LN is one of:
;  – empty
;  – (cons Number LN)
 
; interp. a matrix is a list of rows, a row is a list of numbers
; constraint: all rows are of the same length

; LN -> Number
; given a list of numbers produce the first one
(check-expect (1number R2) 21)
(check-expect (1number T2) 12)

(define (1number ln)
  (first ln))


; Matrix -> LN 
; consumes a matrix and produces the first column as a list of numbers (LN)
(check-expect (first* MAT) T1)
(check-expect (first* TAM) R1)

(define (first* m) 
  (cond
    [(empty? m) empty]
    [else (cons (1number (first m)) (first* (rest m)))]))

; LN -> LN
; consume a list of numbers and return the same list sans the first element
(check-expect (allbutone R1) SUBR1)

(define (allbutone ln)
  (rest ln))


; Matrix -> Matrix
; consumes a matrix and produces a new one sans the 1st column
(check-expect (rest* MAT) SUBMAT) 

(define (rest* m) 
  (cond
    [(empty? m) empty]
    [else (cons (allbutone (first m)) (rest* (rest m)))]))



; Matrix -> Matrix
; transpose the items on the given matrix along the diagonal
(check-expect (transpose MAT) TAM)

(define (transpose lln)
  (cond
    [(empty? (first lln)) empty]
    [else (cons (first* lln) (transpose (rest* lln)))]))


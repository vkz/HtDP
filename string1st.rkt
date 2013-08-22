;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string1st) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;Exercise 26: Design the function string-first, 
;which extracts the first character from a non-empty string. 
;Don’t worry about empty strings.

;String -> Char
;extract the first character from anon-empty string
;given "hello", expect "h"
(define (string-first str) 
  (string-ref str 0))

;String -> Char?
;extract the last character from a non-empty string str
;given "world", expect "d"
(define (string-last str) 
  (string-ref str (-(string-length str) 1)))

;String -> String
;given a string return it with the first character removed
;given "hello", expect "ello"
;given "world", expect "orld"                     
(define (string-rm str) 
      (substring str 1))

;String -> String
;given a string return it with the last character removed                                                         
;given "hello", expect "hell"
;given "world", expect "worl"                                                         
(define (string-rml str)
  (substring str 0 (- (string-length str) 1)))

(string-first "hello")
(string-last "hello")
(string-first "world")
(string-last "world")
(string-rm "hello")
(string-rm "world")
(string-rml "hello")
(string-rml "world")

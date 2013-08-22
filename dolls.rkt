;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dolls) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define-struct layer (color doll))
; An RD is one of:
; - string
; - (make-layer String RD)

; RD -> String
; print all colors of a given russian doll as one comma-separated string
(check-expect (print-color (make-layer "green" (make-layer "yellow" "red"))) "green, yellow, red")
(check-expect (print-color "red") "red")

(define (print-color a-doll) 
  (cond 
    [(string? a-doll) a-doll]
    [(layer? a-doll) (string-append (layer-color a-doll) 
                                    ", " 
                                    (print-color (layer-doll a-doll)))]
    )
  )

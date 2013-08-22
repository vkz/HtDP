;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname serpinski) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; Constants and Data
;; Serpinski is a struct (make-spi Posn Posn Posn)
(define-struct spi (a b c))
(define CS (empty-scene 400 400))
(define A (make-posn 200 50))
(define B (make-posn 373 350))
(define C (make-posn 27 350))

;; too-small? : Spi  ->  Boolean
(define (too-small? s)
  (<= (- (posn-x (spi-b s)) (posn-x (spi-c s))) 5))

;; mid-point : Posn Posn -> Posn
;; calculates the middle point between the two given points
;; x = (x1+x2)/2
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2)
             (/ (+ (posn-y a) (posn-y b)) 2)))

; (check-expect (mid-point (make-posn 0 6) (make-posn 0 0))
;               (make-posn 0 3))
; (check-expect (mid-point (make-posn 6 0) (make-posn 0 0))
;               (make-posn 3 0))
; (check-expect (mid-point (make-posn 0 6) (make-posn 6 0))
;               (make-posn 3 3))
; 
;; draw-side : Posn Posn Scene -> Image
(define (draw-side a b scene)
  (add-line scene (posn-x a) (posn-y a) (posn-x b) (posn-y b) "red"))


;; draw-triangle : Spi Image -> Image
(define (draw-triangle s scene)
  (draw-side (spi-a s)
             (spi-b s)
             (draw-side (spi-b s)
                        (spi-c s)
                        (draw-side (spi-c s)
                                   (spi-a s) scene))))

;; serpinski : Spi Image -> Image
(define (serpinski s scene)
  (cond
    [(too-small? s) scene]
    [else (local ((define a-b (mid-point (spi-a s) (spi-b s)))
                  (define b-c (mid-point (spi-b s) (spi-c s)))
                  (define c-a (mid-point (spi-c s) (spi-a s)))
                  (define spi-up (make-spi (spi-a s) a-b c-a))
                  (define spi-right (make-spi a-b (spi-b s) b-c))
                  (define spi-left (make-spi c-a b-c (spi-c s)))
                  )
            (serpinski spi-up 
                     (serpinski spi-right
                                (serpinski spi-left
                                            (draw-triangle s scene)))))]))

;(serpinski (make-spi A B C) CS)
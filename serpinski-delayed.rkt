;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname serpinski-delayed) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;;=============== RESOURCE INTENSIVE =============
;;=============== SMTH IS VERY WRONG =============



;; Constants and Data
;; Serpinski is a struct (make-spi Posn Posn Posn)
(define-struct spi (a b c))
(define CS (empty-scene 400 400))
(define A (make-posn 200 50))
(define B (make-posn 373 350))
(define C (make-posn 27 350))
(define SMALLEST 10)

;; too-small? : Spi  ->  Boolean
(define (too-small? s)
  (<= (- (posn-x (spi-b s)) (posn-x (spi-c s))) SMALLEST))

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

;; serpinski : Spi List-of Spies -> List-of Spies
(define (serpinski s slist)
  (cond
    [(too-small? s) slist]
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
                                            (cons s slist)))))]))

(define SERP (serpinski (make-spi A B C) empty))

(define-struct w (spil canvas))

;; world-lives : W -> W
;; given (make-w List-of-Spies Image) puts the first spi onto the canvas and returns
;; (make-w (rest (w-spil W)) Canvas)
(define (world-lives ws)
  (make-w (rest (w-spil ws)) (draw-triangle (first (w-spil ws)) (w-canvas ws))))

;; world-canvas : W -> Image
(define (world-canvas ws)
  (place-image (w-canvas ws) 200 200 CS))

;; True if List-of Spies is empty
(define (w-empty? ws)
  (empty? (w-spil ws)))

(time (big-bang (make-w SERP CS)
          (on-tick world-lives (/ 1 1000))
          (to-draw world-canvas)
          (stop-when w-empty?)))
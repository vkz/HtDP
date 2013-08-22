;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |car scene|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define WHEEL-RADIUS 10)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define BODY-LENGTH (+ WHEEL-DISTANCE (* 6 WHEEL-RADIUS)))
(define BODY-HEIGHT (* WHEEL-RADIUS 2))
(define SLENGTH (* BODY-LENGTH 10))
(define SHEIGHT (* BODY-HEIGHT 10))
(define BACKGROUND (empty-scene SLENGTH SHEIGHT))

(define WHL (circle WHEEL-RADIUS "solid" "black"))
(define BDY
  (above
    (rectangle (/ BODY-LENGTH 2) (/ BODY-HEIGHT 2)
               "solid" "red")
    (rectangle BODY-LENGTH BODY-HEIGHT "solid" "red")))
(define SPC (rectangle WHEEL-DISTANCE 1 "solid" "white"))
(define WH* (beside WHL SPC WHL))
(define CAR (underlay/xy BDY WHEEL-RADIUS BODY-HEIGHT WH*))
(define Y-CAR (- SHEIGHT (/ (image-height CAR) 2)))

;CarState -> CarState
;The clock ticked; move car by 3 pixels to the right
;Given: 20, expect: 23
;Given: 78, expect: 81
(define (tock ws) (+ ws 3))

;CarState -> Image
;place a car into a scene according to the current state of the world
(define (render ws)
  (place-image CAR ws Y-CAR BACKGROUND))

;main: CarState -> CarState
;launch the program from some initial state
(define (main ws)
  (big-bang ws (on-tick tock) (to-draw render)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Tetris) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; A Tetris is (make-tetris Block Landscape)
; A Landscape is one of:
; – empty
; – (cons Block Landscape)
; Block is (make-block N N)
 
; interpretation: given (make-tetris (make-block x y) (list b1 b2 ...))
; (x,y) is the logical position of the dropping block, while
; b1, b2, etc are the positions of the resting blocks
; a logical position (x,y) determines how many SIZEs the block is
; from the left - x, and from the top - y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct tetris (block landscape))

; physical constants
(define WIDTH 10) ; the maximal number of blocks horizontally
(define HEIGHT WIDTH)

; graphical constants
(define SIZE 20) ; blocks are square
(define BLOCK ; they are rendered as red squares with black rims
  (overlay (rectangle (- SIZE 1) (- SIZE 1) "solid" "red")
           (rectangle SIZE SIZE "outline" "black")))
 
(define SCENE-SIZE (* WIDTH SIZE))
(define CS (empty-scene SCENE-SIZE SCENE-SIZE))

; Sample data instances
(define block-top-left (make-posn 0 0))
(define block-top-center (make-posn 4 0))
(define block-top-right (make-posn (- WIDTH 1) 0))
(define block-bottom-left (make-posn 0 (- WIDTH 1)))
(define block-bottom-center (make-posn 4 (- WIDTH 1)))
(define block-bottom-right (make-posn (- WIDTH 1) (- WIDTH 1)))

(define landscape0 empty)
(define landscape1 (cons block-bottom-left landscape0))
(define landscape2 (cons block-bottom-center landscape1))
(define landscape3 (cons block-bottom-right landscape2))
(define landscape-block-on-block (cons (make-posn 0 (- WIDTH 2)) landscape1))

;;;;;;;;;; AUXILARY ;;;;;;;;;
; Number Number List -> list of length N [n=0 ... N-1]
; filling the bottom row using generative recursion
(define (check-bottom-fill N n l)
  (if (< n N)
      (bottom-fill N n l)
      l))

(define (bottom-fill N n l)
  (check-bottom-fill N (+ 1 n) (append l (cons (make-posn n (- HEIGHT 1)) empty))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tetris0 (make-tetris block-top-center landscape2))
(define tetris1 (make-tetris block-bottom-right landscape2))
(define tetris-1 (make-tetris block-top-center (check-bottom-fill WIDTH 1 empty)))

; Block -> Number
; return logical coordinates of the block
; x SIZEs away from the left, y SIZEs away from the right
; (<= 0 [x,y]) (< [x,y] WIDTH)
(define (block-x b) 
  (if (and (<= 0 (posn-x b)) (< (posn-x b) WIDTH))
      (* (posn-x b) SIZE)
      (error "block-coordinate: " "out of boundaries (0 <= x < " WIDTH ")")))

(define (block-y b) 
  (if (and (<= 0 (posn-y b)) (< (posn-y b) WIDTH))
      (* (posn-y b) SIZE)
      (error "block-coordinate: " "out of boundaries (0 <= y < " WIDTH ")")))

; Block Image? -> Image 
; Render a block, anchor it at the top left corner
(define (render-block b scene)
  (place-image/align BLOCK (block-x b) (block-y b) "left" "top" scene))

; Landscape -> Image
; put the landscape (list of blocks) on the canvas 
(define (render-landscape l)
  (cond
    [(empty? l) CS]
    [else (render-block (first l) (render-landscape (rest l)))]))

; Tetris -> Image
; put a tetris (make-tetris block landscape) on the canvas
(define (render-tetris t)
  (render-block (tetris-block t) (render-landscape (tetris-landscape t))))

; Block -> Block
; Given block moves down by one SIZE
(define (block-falls b)
  (make-posn (posn-x b) (+ 1 (posn-y b))))
  
(check-expect (block-falls block-top-left) (make-posn 0 1))
(check-expect (block-falls block-top-center) (make-posn 4 1))
(check-expect (block-falls (block-falls block-top-center)) (make-posn 4 2))

; Tetris -> Boolean
; True if (tetris-block) has landed i.e. member? of the landscape or
; y-coordinate = HEIGHT-1
(define (has-landed? t)
  (or (equal? (- HEIGHT 1) (posn-y (tetris-block t))) ;on the ground
      (member? (block-falls (tetris-block t)) (tetris-landscape t)))) ;on the landscape

(check-expect (has-landed? tetris0) false)
(check-expect (has-landed? tetris1) true)
(check-expect (has-landed? (make-tetris (make-posn 4 (- HEIGHT 2))
                                        landscape3)) true)
(check-expect (has-landed? (make-tetris (block-falls block-top-right) landscape3)) false)
(check-expect (has-landed? (make-tetris (make-posn 0 (- HEIGHT 3))
                                        landscape-block-on-block)) true)
; Block -> Boolean
; checks if Block's y-coordinate = Height-1 (at the bottom)
(define (bottom-block? b)
  (equal? (posn-y b) (- HEIGHT 1)))

(define (not-bottom-block? b)
  (not (bottom-block? b)))

; Tetris -> Boolean
; True if the bottom row is full
(define (bottom-row-full? t) 
  (equal? WIDTH (length (filter bottom-block? (tetris-landscape t)))))

; Landscape -> Landscape
; brings the entire landscape 1 SIZE down (applied when the bottom row is cleaned)
(define (landscape-down l)
  (cond 
    [(empty? l) empty]
    [else (cons (make-posn (posn-x (first l)) (+ 1 (posn-y (first l))))
                (landscape-down (rest l)))]))

; Tetris -> Tetris
; deletes the bottom row if its full
(define (bottom-clean t)
  (if (bottom-row-full? t)
      (make-tetris (tetris-block t) (landscape-down (filter not-bottom-block? (tetris-landscape t))))
      t))

; Tetris -> Tetris
; tetris evolves: block falls one SIZE down and maibe lands on the bottom or 
; the landscape
(define (tock t)
  (if (has-landed? t)
      (bottom-clean (make-tetris (make-posn (random WIDTH) 0) (cons (tetris-block t) (tetris-landscape t))))
      (make-tetris (block-falls (tetris-block t)) (tetris-landscape t))))

; Tetris -> Boolean
; checks if the falling block can be shifted left (no landscape on the left and
; not the left edge of the scene)
(define (can-shift-left? t) 
  (and (<= 0 (- (posn-x (tetris-block t)) 1))
       (not (member? (make-posn (- (posn-x (tetris-block t)) 1) (posn-y (tetris-block t)))
                     (tetris-landscape t)))))

(check-expect (can-shift-left? (make-tetris block-top-left landscape2)) false)
(check-expect (can-shift-left? (make-tetris (make-posn 5 (- HEIGHT 1)) landscape3))
              false)
(check-expect (can-shift-left? tetris0) true)

; Tetris -> Boolean
; checks if the falling block can be shifted right (no landscape on the right and
; not the right edge of the scene)
(define (can-shift-right? t) 
  (and (< (+ (posn-x (tetris-block t)) 1) WIDTH)
       (not (member? (make-posn (+ (posn-x (tetris-block t)) 1) (posn-y (tetris-block t)))
                     (tetris-landscape t)))))

(check-expect (can-shift-right? (make-tetris block-top-right landscape2)) false)
(check-expect (can-shift-right? (make-tetris (make-posn 3 (- HEIGHT 1)) landscape3))
              false)
(check-expect (can-shift-right? tetris0) true)

  
; Tetris -> Tetris
; shifts the falling block one SIZE left (right)
(define (shift-left t)
  (make-tetris (make-posn (- (posn-x (tetris-block t)) 1) (posn-y (tetris-block t)))
               (tetris-landscape t)))

(define (shift-right t)
  (make-tetris (make-posn (+ (posn-x (tetris-block t)) 1) (posn-y (tetris-block t)))
               (tetris-landscape t)))

; Tetris Key? -> Tetris
; on "left" key shift left if no obstacle
; on "right" key shift right if no obstacle
(define (shift-sideways t a-key)
  (cond
   [(and (key=? a-key "left") (can-shift-left? t)) (shift-left t)]
   [(and (key=? a-key "right") (can-shift-right? t)) (shift-right t)]
   [else t]))

; Landscape -> Boolean
; checks if the landscape reached the ceiling i.e. there's a block in the landscape
; whose y-coordinate is 0
(define (block-at-the-ceiling? l)
  (cond
    [(empty? l) false]
    [else (or (equal? (posn-y (first l)) 0)
              (block-at-the-ceiling? (rest l)))]))

(check-expect (block-at-the-ceiling? (cons block-top-left landscape2)) true)
(check-expect (block-at-the-ceiling? (append landscape2 (cons block-top-center landscape0))) true)
(check-expect (block-at-the-ceiling? landscape3) false)

; Tetris -> Boolean
; true if at least one column is high as the ceiling
(define (hit-the-ceiling? t)
  (block-at-the-ceiling? (tetris-landscape t)))

; Tetris -> Image
; produces the final scene
(define (game-over t)
  (overlay (text "Game over!" 16 "blue")
           (render-tetris t)))

; Type any of these in the interactions area to test run:
;(tetris-main tetris-1 (/ 1 4))
;(tetris-main tetris0 (/ 1 4))
(define (tetris-main t0 rate)
  (big-bang t0
            (on-tick tock rate)
            (to-draw render-tetris)
            (on-key shift-sideways)
            (stop-when hit-the-ceiling? game-over)
            ))

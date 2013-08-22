;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct segment (position direction))
; Segment is a struct (make-segment (make-posn x y) d). (Also a WorldState for now)
; Position is a struct (make-posn Number Number) 
; Direction is a String which can be one of
; - "left"
; - "right"
; - "up"
; - "down"
;
; Snake is one of:
; - empty
; - (cons Segment Snake)
; where adjacent segements 
; -- differ by no more than one direction (90deg)
; -- touch each other
;
(define-struct ws (snake food))
; WordState (ws) is a struct where:
; - snake is a list of Segments
; - food is a struct (make-posn x y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Physical const
(define RADIUS 5)
(define DIAMETER (* RADIUS 2))
(define HEIGHT 20)
(define WIDTH 20)

; Graphical const
(define CS (empty-scene (* WIDTH DIAMETER) (* HEIGHT DIAMETER)))
(define SEGM (circle RADIUS "solid" "red"))
(define FOOD (circle RADIUS "solid" "green"))

; Test world
(define S0 (make-segment (make-posn DIAMETER DIAMETER) "right"))
(define WS0 (make-ws (cons S0 empty) (make-posn (* 5 DIAMETER) (* 5 DIAMETER))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Segment -> Number
; Returns x coordinate of the Segment
(define (segment-x s) (posn-x (segment-position s)))

; Segment -> Number
; Returns y coordinate of the Segment
(define (segment-y s) (posn-y (segment-position s)))

; Snake -> Snake
; "Cut the tail" - delete the last segment of the snake.
(define (cut-last-segment s) 
  (cond
    [(empty? (rest s)) empty]
    [else (cons (first s) (cut-last-segment (rest s)))]))

; Segment -> Image
; Produces an Image of the snake on the canvas
(define (render-snake s) 
  (cond 
    [(empty? s) CS]
    [else (place-image SEGM 
                       (segment-x (first s)) 
                       (segment-y (first s)) 
                       (render-snake (rest s)))]))

; WS -> Image
; Produces the world image
(define (render-world w) 
  (place-image FOOD (posn-x (ws-food w)) (posn-y (ws-food w)) (render-snake (ws-snake w))))

; Test snake
(define HEAD (make-segment (make-posn (* DIAMETER 10)
                                           (* DIAMETER 10))
                            "right"))
(define TEST-SNAKE 
  (list HEAD 
      (make-segment (make-posn (- (segment-x HEAD) DIAMETER) (segment-y HEAD)) "right")
      (make-segment (make-posn (- (segment-x HEAD) DIAMETER DIAMETER) (segment-y HEAD)) "right")
      (make-segment (make-posn (- (segment-x HEAD) DIAMETER DIAMETER DIAMETER) (segment-y HEAD)) "right")
      (make-segment (make-posn (- (segment-x HEAD) DIAMETER DIAMETER DIAMETER DIAMETER) (segment-y HEAD)) "right")
      (make-segment (make-posn (- (segment-x HEAD) DIAMETER DIAMETER DIAMETER DIAMETER DIAMETER) (segment-y HEAD)) "right")
      (make-segment (make-posn (- (segment-x HEAD) DIAMETER DIAMETER DIAMETER DIAMETER DIAMETER DIAMETER) (segment-y HEAD)) "right")
      ))

; Segment -> Segment
; increment/decrement x/y coordinate
(define (segment-moves/left s) (make-segment (make-posn (- (segment-x s) DIAMETER) (segment-y s)) (segment-direction s)))
(define (segment-moves/right s) (make-segment (make-posn (+ (segment-x s) DIAMETER) (segment-y s)) (segment-direction s)))
(define (segment-moves/up s) (make-segment (make-posn (segment-x s) (- (segment-y s) DIAMETER)) (segment-direction s)))
(define (segment-moves/down s) (make-segment (make-posn (segment-x s) (+ (segment-y s) DIAMETER)) (segment-direction s)))

(check-expect (segment-moves/left (make-segment (make-posn 30 30) "left"))(make-segment (make-posn (- 30 DIAMETER) 30) "left"))
(check-expect (segment-moves/right (make-segment (make-posn 30 30) "right"))(make-segment (make-posn (+ 30 DIAMETER) 30) "right"))
(check-expect (segment-moves/up (make-segment (make-posn 30 30) "up")) (make-segment (make-posn 30 (- 30 DIAMETER)) "up"))
(check-expect (segment-moves/down (make-segment (make-posn 30 30) "down")) (make-segment (make-posn 30 (+ 30 DIAMETER)) "down"))

; Segment -> Segment
; Given the snake's current position s moves it one diameter in the direction 
(define (segment-moves s) 
  (cond 
    [(string=? "left" (segment-direction s)) (segment-moves/left s)]
    [(string=? "right" (segment-direction s)) (segment-moves/right s)]
    [(string=? "up" (segment-direction s)) (segment-moves/up s)]
    [(string=? "down" (segment-direction s)) (segment-moves/down s)]
    [else s]))

; Snake -> Snake
; cut the last segment and append it at the front changing its direction and 
; position to that of the current head and calling (segment-moves ) on it
(define (snake-moves s)
  (cons (segment-moves (first s)) (cut-last-segment s))
  )

; WS -> Boolean
; Checks if Head's coordinates are equal to the food coordinates
(define (food-eaten? w)
  (equal? (ws-food w) (segment-position (first (ws-snake w)))))

; Posn Posn -> Posn
; if the food's been eaten calls (food-create current_head_posn) to create new food
; returns the current food posn otherwise
; NB: generative recursion here
(define (food-check-create f p)
  (if (equal? f p)
      (food-create p)
      f))

; Posn -> Posn
; generates new food
; NB: generative recursion here
(define (food-create p)
  (food-check-create (make-posn (* DIAMETER (+ (random (- WIDTH 1)) 1)) (* DIAMETER (+ (random (- HEIGHT 1)) 1))) p))

; WS -> WS
; If food eaten extend the snake by one segment and the snake moves, new food created,
; snake moves 
(define (world-lives w) 
  (if (food-eaten? w)
      (make-ws (cons (segment-moves (first (ws-snake w))) (ws-snake w)) 
               (food-check-create (ws-food w) (segment-position (first (ws-snake w)))))
      (make-ws (snake-moves (ws-snake w)) (ws-food w))))

; Snake Key -> Snake
; changes the direction of the front-most segment to the one received 
; NB: snake cannot turn 180 i.e. can't move to the opposite direction to its current
(define (change-snake-direction s a-key) 
      (cond 
        [(and (key=? a-key "left") (not (string=? (segment-direction (first s)) "right"))) 
         (cons (make-segment (segment-position (first s)) "left") (rest s))]
        [(and (key=? a-key "right") (not (string=? (segment-direction (first s)) "left"))) 
         (cons (make-segment (segment-position (first s)) "right") (rest s))]
        [(and (key=? a-key "up") (not (string=? (segment-direction (first s)) "down"))) 
         (cons (make-segment (segment-position (first s)) "up") (rest s))]
        [(and (key=? a-key "down") (not (string=? (segment-direction (first s)) "up"))) 
         (cons (make-segment (segment-position (first s)) "down") (rest s))]
        [else s]))

; WS -> WS
; change the world state when a key received
(define (world-turns w a-key)
  (make-ws (change-snake-direction (ws-snake w) a-key) (ws-food w)))

; Snake -> Boolean
; - returns True if distance to the wall < 1
; - returns False otherwise
(define (hit-the-wall? s) 
  (cond
    [(string=? (segment-direction (first s)) "left") (<= (- (segment-x (first s)) RADIUS) 0)]
    [(string=? (segment-direction (first s)) "right") (>= (+ (segment-x (first s)) RADIUS) (* WIDTH DIAMETER))]
    [(string=? (segment-direction (first s)) "up") (<= (- (segment-y (first s)) RADIUS) 0)]
    [(string=? (segment-direction (first s)) "down") (>= (+ (segment-y (first s)) RADIUS) (* HEIGHT DIAMETER))]))
               
; Snake Posn -> Boolean 
; Checks if there's a segment in the list with the same coordinates as given
(define (hit-itself? s p) 
  (cond
    [(empty? s) false]
    [else (or (equal? (segment-position (first s)) p) (hit-itself? (rest s) p))]))

(check-expect (hit-itself? TEST-SNAKE (segment-position HEAD)) true)
(check-expect (hit-itself? TEST-SNAKE (segment-position (third TEST-SNAKE))) true)
(check-expect (hit-itself? TEST-SNAKE (make-posn 20 3)) false)

; Snake -> Boolean
; - return True if (hit-the-wall? s) is True, or
; - run into itself
(define (run-into-obstacle? w)
  (or (hit-the-wall? (ws-snake w)) (hit-itself? (rest (ws-snake w)) (segment-position (first (ws-snake w))))))

; Segment -> Image
; adds (text "ouch!" 12 "blue")to the last rendered scene
(define (last-scene w)
  (place-image (text "ouch!" 16 "blue") 
               (* (/ WIDTH 2) DIAMETER)
               (* (/ HEIGHT 2) DIAMETER)
               (render-world w)))

; (snake-main WS0 (/ 1 4)) in the interactions area to test the game
(define (snake-main w0 rate) 
  (big-bang w0 
            (on-tick world-lives rate) 
            (to-draw render-world) 
            (on-key world-turns)
            (stop-when run-into-obstacle? last-scene)))
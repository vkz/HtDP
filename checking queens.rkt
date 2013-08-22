;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |checking queens|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; Tile is one of:
; - true (if a queen can be placed on that square
; - false 

; N by N Chessboard is a (List-of (List-of Tiles))
; where each list is N elements long

(check-expect (build-board 1 =) (list (list true)))
(check-expect (build-board 2 =) (list (list true false)
                                      (list false true)))
(check-expect (build-board 3 =) (list (list true false false)
                                      (list false true false)
                                      (list false false true)))
(check-expect (board-ref (build-board 3 =) 0 1) false)
(check-expect (board-ref (build-board 3 =) 2 2) true)
(check-expect (board-ref (build-board 3 =) 2 1) false)
(check-expect (threatened? (make-posn 3 0) (make-posn 3 2)) true) ;vertical
(check-expect (threatened? (make-posn 3 0) (make-posn 1 0)) true) ;horizontal
(check-expect (threatened? (make-posn 3 0) (make-posn 0 3)) true) ;diagonal
(check-expect (threatened? (make-posn 3 0) (make-posn 3 0)) true) ;same position
(check-expect (threatened? (make-posn 3 2) (make-posn 4 2)) true) ;vertical
(check-expect (threatened? (make-posn 3 2) (make-posn 3 4)) true) ;horizontal
(check-expect (threatened? (make-posn 3 2) (make-posn 4 3)) true) ;diagonal
(check-expect (threatened? (make-posn 3 2) (make-posn 1 0)) true) ;diagonal
(check-expect (threatened? (make-posn 3 2) (make-posn 1 3)) false) ;no threat

;; build-board : N (N N  ->  boolean)  ->  board
;; to create a board of size n x n, 
;; fill each position with indices i and j with (f i j)
(define (build-board n f)
  (build-list n (lambda (i) 
                  (build-list n (lambda (j) (f i j))))))

;; board-ref : board N N  ->  boolean
;; to access a position with indices i, j on a-board
(define (board-ref a-board i j) 
  (list-ref (list-ref a-board i) j))

;; threatened? : posn posn -> Boolean
;; checks if a certain tile located at t is threatened by a queen at q
(define (threatened? queen tile)
  (local (;cross-v? : posn posn -> boolean 
          (define (cross-v? q p) (= (posn-y q) (posn-y p)))
          
          ;cross-h? : posn posn -> boolean
          (define (cross-h? q p) (= (posn-x q) (posn-x p)))
          
          ;cross-d? : posn posn -> boolean
          (define (cross-d? q p) 
            (= (abs (/ (- (posn-y p) (posn-y q))
                       (- (posn-x p) (posn-x q))))
               1)))
    (or (equal? queen tile)
        (cross-v? queen tile)
        (cross-h? queen tile)
        (cross-d? queen tile))))
  
(define TRUE (lambda (i j) true))
(define FALSE (lambda (i j) false))
(define Empty-board2 (build-board 2 TRUE)) 
(define Empty-board3 (build-board 3 TRUE))
(define Empty-board4 (build-board 4 TRUE))
(define Empty-board8 (build-board 8 TRUE))

;; placement : Number board -> false or board
;; place N (N > 0) queens on the board or return false
(define (placement N board) 
  (local (;; place-one-more : Number board -> false or board
          ;; n-1 queens already on the board, place another one 
          (define (place-one-more n bd)
            (local (;unthreatened-tiles : board -> List-of unthretened tiles
                    (define (unthreatened-tiles bd) 
                      (local ((define I (lambda (i) (cond 
                                                       [(= i (length bd)) empty]
                                                       [else (append (local ((define J (lambda (j) 
                                                                                         (cond 
                                                                                           [(= j (length bd)) empty]
                                                                                           [(board-ref bd i j) (cons (make-posn i j) (J (add1 j)))]
                                                                                           [else (J (add1 j))]))))
                                                                       (J 0))
                                                                     (I (add1 i)))]))))
                        (I 0)))
                    
                    ;place-at-posn : posn board -> board
                    (define (place-at-posn p bd)
                      (local ((define (turn-if-threatened i j)
                                (if (threatened? p (make-posn i j)) false (board-ref bd i j))))
                        (build-board (length bd) turn-if-threatened)))
                    
                    ;place-one/list : List-of tiles Board -> Board or false
                    ;place a queen on the posn in the list or return false if list is empty
                    (define (place-one/list safe-list bd)
                      (cond
                        [(empty? safe-list) false]
                        [else (local ((define possible-placement 
                                        (place-one-more (add1 n) (place-at-posn (first safe-list) bd))))
                                (cond
                                  [(boolean? possible-placement) (place-one/list (rest safe-list) bd)]
                                  [else possible-placement]))])))
              (cond 
                [(> n N) bd]
                [else (local ((define possible-placement 
                                (place-one/list (unthreatened-tiles bd) bd)))
                        (cond
                          [(boolean? possible-placement) false]
                          [else possible-placement]))]))))
    (place-one-more 1 board)))
 

(check-expect (placement 1 Empty-board2) (build-board 2 FALSE))
(check-expect (placement 2 Empty-board2) false)
(check-expect (placement 1 Empty-board3) (list (list false false false)
                                               (list false false true)
                                               (list false true false)))
(check-expect (placement 2 Empty-board3) (build-board 3 FALSE))
(check-expect (placement 4 Empty-board4) (build-board 4 FALSE))
(check-expect (placement 2 Empty-board4) (list (list false false false false)
                                               (list false false false false)
                                               (list false false false false)
                                               (list false true false false)))
(check-expect (placement 5 Empty-board4) false)
                                                     
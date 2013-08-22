;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |checking queens vectors and mutators|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(check-expect (threatened? (make-posn 3 0) (make-posn 3 2)) true) ;vertical
(check-expect (threatened? (make-posn 3 0) (make-posn 1 0)) true) ;horizontal
(check-expect (threatened? (make-posn 3 0) (make-posn 0 3)) true) ;diagonal
(check-expect (threatened? (make-posn 3 0) (make-posn 3 0)) true) ;same position
(check-expect (threatened? (make-posn 3 2) (make-posn 4 2)) true) ;vertical
(check-expect (threatened? (make-posn 3 2) (make-posn 3 4)) true) ;horizontal
(check-expect (threatened? (make-posn 3 2) (make-posn 4 3)) true) ;diagonal
(check-expect (threatened? (make-posn 3 2) (make-posn 1 0)) true) ;diagonal
(check-expect (threatened? (make-posn 3 2) (make-posn 1 3)) false) ;no threat

; a board n-by-n is 
; - (vectorof (vectorof Anything)) each vector is of length n>0

;; build-board : N (N N  ->  boolean)  ->  board
;; to create a board of size n x n, 
;; fill each position with indices i and j with (f i j)
(define (build-board n f) 
  (build-vector n
                (lambda (i) (build-vector n 
                                          (lambda (j) (f i j))))))

(check-expect (build-board 3 (lambda (i j) (= i j)))
              (vector (vector true false false)
                      (vector false true false)
                      (vector false false true)))

;; board-ref : board N N  ->  boolean
;; to access a position with indices i, j on a-board
(define (board-ref a-board i j)
  (vector-ref (vector-ref a-board i) j))

(check-expect (board-ref (vector (vector 1 2 3)
                                 (vector 4 5 6)
                                 (vector 7 8 9)) 1 2) 6)


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
                                                       [(= i (vector-length bd)) empty]
                                                       [else (append (local ((define J (lambda (j) 
                                                                                         (cond 
                                                                                           [(= j (vector-length bd)) empty]
                                                                                           [(board-ref bd i j) (cons (make-posn i j) (J (add1 j)))]
                                                                                           [else (J (add1 j))]))))
                                                                       (J 0))
                                                                     (I (add1 i)))]))))
                        (I 0)))
                    
                    ;place-at-posn : posn board -> board
                    (define (place-at-posn p bd)
                      (local ((define (turn-if-threatened i j)
                                (if (threatened? p (make-posn i j)) false (board-ref bd i j))))
                        (build-board (vector-length bd) turn-if-threatened)))
                    
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
(check-expect (placement 1 Empty-board3) (vector (vector false false false)
                                               (vector false false true)
                                               (vector false true false)))
(check-expect (placement 2 Empty-board3) (build-board 3 FALSE))
(check-expect (placement 4 Empty-board4) (build-board 4 FALSE))
(check-expect (placement 2 Empty-board4) (vector (vector false false false false)
                                               (vector false false false false)
                                               (vector false false false false)
                                               (vector false true false false)))
(check-expect (placement 5 Empty-board4) false)
                                                     
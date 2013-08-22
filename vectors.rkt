;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname vectors) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; vector-sum : (vectorof number)  ->  number
;; to compute the sum of the numbers in v
(define (vector-sum v)
  (local (;; vector-sum-aux : (vectorof number) N  ->  number
          ;; to sum the numbers in v with index in [0, i)
          (define (vector-sum-aux i) 
            (cond
              [(zero? i) 0]
              [else (+ (vector-ref v (sub1 i)) 
                       (vector-sum-aux (sub1 i)))])))
    (vector-sum-aux (vector-length v))))

;; norm : (vectorof number)  ->  number
;; to compute the square-root of the sum of the squares of the numbers in v
(define (norm v)
  (local (;; vector-sum-aux : (vectorof number) N  ->  number
          ;; to sum the numbers in v with index in [0, i)
          (define (vector-sum-squares-aux i) 
            (cond
              [(zero? i) 0]
              [else (+ (sqr (vector-ref v (sub1 i)))
                       (vector-sum-squares-aux (sub1 i)))])))
    (sqrt (vector-sum-squares-aux (vector-length v)))))


(define N1 (norm (vector 1 1 1)))
(define V1 (vector (/ 1 N1)
                   (/ 1 N1)
                   (/ 1 N1)))

;; vector-contains-doll? : (vectorof symbols) -> true or false
;; check if there's a doll among the elements
(define (vector-contains-doll? v) 
  (local (;contains-doll? : number -> true or false
          (define (contains-doll? i)
            (cond
              [(zero? i) false]
              [else (if (equal? 'doll (vector-ref v (sub1 i)))
                        (sub1 i)
                        (contains-doll? (sub1 i)))])))
    (contains-doll? (vector-length v))))
         
(check-expect (vector-contains-doll? (vector 'a 'x 'doll 'half)) 2)
(check-expect (vector-contains-doll? (vector 'a 'x 'half)) false)
(check-expect (vector-contains-doll? (vector 'a 'x 'doll)) 2)

;; vector-count-symbols : symbol (vectorof symbols) -> number
;; how many symbols s is in the vector
(define (vector-count-symbols v s)
  (local (;contains-doll? : number -> true or false
          (define (count-symbols i counter)
            (cond
              [(zero? i) counter]
              [else (if (equal? s (vector-ref v (sub1 i)))
                        (count-symbols (sub1 i) (add1 counter))
                        (count-symbols (sub1 i) counter))])))
    (count-symbols (vector-length v) 0)))
         
(check-expect (vector-count-symbols (vector 'a 'x 'doll 'half 'x) 'x) 2)
(check-expect (vector-count-symbols (vector 'a 'x 'half) 'none) 0)
(check-expect (vector-count-symbols (vector 'a 'x 'doll) 'doll) 1)

; vector+ : vector voctor -> vector
; pointwise sum of the two vectors
(define (vector-compose op v w)
  (build-vector (vector-length v) 
                (lambda (i) (op (vector-ref v i)
                               (vector-ref w i)))))

(define (vector+ v w)
  (vector-compose + v w))

(define (vector- v w)
  (vector-compose - v w))

(check-expect (vector- (vector 1 1 1)
                       (vector 0 -1 1)) (vector 1 2 0))
(check-expect (vector+ (vector 1 1 1)
                       (vector 0 -1 1)) (vector 1 0 2))
                
; a board n-by-n is 
; - (vectorof (vectorof Anything)) each vector is of length n>0

;; build-board : N (N N  ->  Any)  ->  board
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

;; transpose : board (matrix) -> board 
;; transpose the matrix (i j) -> (j i)
(define (transpose a-board)
  (build-board (vector-length a-board) 
           (lambda (i j) (board-ref a-board j i))))
           
           
(check-expect (transpose (vector (vector 1 0 -1)
                                 (vector 2 0 9)
                                 (vector 1 1 1)))
              (vector (vector 1 2 1)
                      (vector 0 0 1)
                      (vector -1 9 1)))
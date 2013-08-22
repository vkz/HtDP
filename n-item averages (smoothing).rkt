;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |n-item averages (smoothing)|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; list-3-average : listof N -> listof N
;; produce a list of sliding 3-item averages
(define (list-3-average a-lon)
  (cond
    [(< (length a-lon) 3) empty]
    [else (local ((define 3-average (/ (+ (first a-lon) (second a-lon) (third a-lon)) 3)))
            (cons 3-average (list-3-average (rest a-lon))))]))

(define T (list 1.10 1.12 1.08 1.09 1.11))
(define TS (list 1.10 329/300 82/75))
(check-expect (list-3-average T) TS)

;; vec-3-average : vectorof N -> vectorof N
;; a vector of 3-item averages
(define (vec-3-average a-vec)
  (local ((define (3-average i)
            (/ (+ (vector-ref a-vec i)
                  (vector-ref a-vec (add1 i))
                  (vector-ref a-vec (add1 (add1 i))))
               3)))
    (build-vector (- (vector-length a-vec) 2) 3-average)))

(define V (vector 1.10 1.12 1.08 1.09 1.11))
(define VS (vector 1.10 329/300 82/75))
(check-expect (vec-3-average V) VS)

             
;; vec-3-average-mut : vectorof N -> (void)
;; effect : fill first Length - 3 places with 3-item sliding averages and
;; pad the rest of the vector with false
;; respective intervals are [0,N-3](N-3,N)
(define (vec-3-average-mut a-vec)
  (local ((define (pad-3-average i)
            (cond 
              [(> i (- (vector-length a-vec) 3)) (pad-false i)]
              [else (local ((define 3-average (/ (+ (vector-ref a-vec i)
                                                    (vector-ref a-vec (add1 i))
                                                    (vector-ref a-vec (add1 (add1 i))))
                                                 3)))
                      (begin
                        (vector-set! a-vec i 3-average)
                        (pad-3-average (add1 i))))]))
          
          (define (pad-false i)
            (cond 
              [(= i (vector-length a-vec)) (void)]
              [else (begin
                      (vector-set! a-vec i false)
                      (pad-false (add1 i)))])))
    (pad-3-average 0)))

(define Vmut (vector 1.10 1.12 1.08 1.09 1.11))
(define VSmut (vector 1.10 329/300 82/75 false false))
(begin (vec-3-average-mut Vmut)
       (equal? Vmut VSmut))
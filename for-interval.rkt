;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname for-interval) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; for-interval :  N (N  ->  N) (N  ->  N) (N  ->  X)  ->  X
;; to evaluate (action i (vector-ref V i)) for i, (step i), ...
;; until (end? i) holds (inclusive)
;; generative recursion: step generates new value, end? detects end
;; termination is not guaranteed 
(define (for-interval i end? step action final)
  (cond
    [(end? i) (final i)]
    [else (begin
	    (action i)
	    (for-interval (step i) end? step action final))]))

(define V (vector 2 3 5 22 3 432 17))
(define V1 (vector 1 2 3 4))

(define (rotate-left v)
  (local ((define 1st (vector-ref v 0)))
    (for-interval 0 
                  (lambda (i) (= i (sub1 (vector-length v))))
                  add1 
                  (lambda (i) (vector-set! v i (vector-ref v (add1 i))))
                  (lambda (i) (vector-set! v i 1st)))))
                                                    

(define (vector-reverse! v)
  (local ((define (swap i j)
            (local ((define tmp (vector-ref v i)))
              (begin (vector-set! v i (vector-ref v j))
                     (vector-set! v j tmp)))))
    (for-interval 0
                  (lambda (i) (>= i (- (sub1 (vector-length v)) i)))
                  add1 
                  (lambda (i) (swap i (- (sub1 (vector-length v)) i)))
                  (lambda (i) (void)))))

(define (vector-sum! v)
  (cond
    [(> (vector-length v) 1) (for-interval (- (vector-length v) 2)
                                           zero?
                                           sub1
                                           (lambda (i) (vector-set! v i (+ (vector-ref v i)
                                                                           (vector-ref v (add1 i)))))
                                           (lambda (i) (vector-set! v i (+ (vector-ref v i)
                                                                           (vector-ref v (add1 i))))))]
    [else (void)]))


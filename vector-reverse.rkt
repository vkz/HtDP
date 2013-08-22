;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname vector-reverse) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; vector-reverse! : vector of any -> vector of any reversed
(define (vector-reverse! v)
  (local ((define (reverse-aux left right)
            (cond
              [(>= left right) v]
              [else (begin
                      (swap left right)
                      (reverse-aux (add1 left) (sub1 right)))]))
          
          (define (swap i j)
            (local ((define temp (vector-ref v i)))
              (begin
                (vector-set! v i (vector-ref v j))
                (vector-set! v j temp)))))
    (reverse-aux 0 (sub1 (vector-length v)))))

(define (main v-length)
  (local ((define V (build-vector v-length (lambda (n) (random v-length))))
          (define reversedV (build-vector v-length (lambda (n) (vector-ref V n)))))
    (begin 
      (vector-reverse! reversedV)
      (list V reversedV))))
                    
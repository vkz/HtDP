;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname vector-mutators) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; call-status : (vectorof boolean)
;; to keep track of the floors from which calls have been issued 
(define call-status (vector true true true false true true true false))

;; reset :  ->  void
;; effect: to set all fields in call-status to false
;(define (reset)
 ; (reset-aux call-status 0 (sub1 (vector-length call-status))))

;; reset-aux : (vectorof boolean) M N  ->  void
;; effect: to set the fields of v with index in [M N) to false
(define (reset-aux v m n)
  (cond 
    [(< n m) (void)]
    [else (begin
            (vector-set! v n false)
            (vector-set! v m false)
            (reset-aux v (add1 m) (sub1 n)))]))
     
;; vec-for-all : (N X  ->  void) (vectorof X)  ->  void
;; effect: to apply f to all indices and values in vec
;; equation: 
;; (vec-for-all f (vector v-0 ... v-N)) 
;; = 
;; (begin (f N v-N) ... (f 0 v-0) (void))
(define (vec-for-all f vec)
  (local ((define (apply-f i)
            (cond 
              [(zero? i) (void)]
              [else (begin
                      (vector-set! vec (sub1 i) (f (sub1 i) (vector-ref vec (sub1 i))))
                      (apply-f (sub1 i)))]))
          )
    (apply-f (vector-length vec))))

(define VC (vector 1 2 3 4))

;; count-vowels : (listof letter) 
;;           ->  (vector number number number number number)
;; where a letter is a symbol in 'a ... 'z
;; to determine how many times the five vowels occur in chars
;; the resulting vector lists the counts in the lexicographic order
(define (count-vowels chars)
  (cond
    [(empty? chars) (vector 0 0 0 0 0)]
    [else
     (local ((define count-rest (count-vowels (rest chars))))
       (begin
         (count-a-vowel (first chars) count-rest)
         count-rest))]))

;; count-a-vowel : letter (vector number number number number number)  ->  void
;; effect: to modify counts at the appropriate place if l is a vowel, 
;; none otherwise
(define (count-a-vowel l counts) 
  (cond 
    [(symbol=? l 'a) (vector-set! counts 0 (add1 (vector-ref counts 0)))]
    [(symbol=? l 'e) (vector-set! counts 1 (add1 (vector-ref counts 1)))]
    [(symbol=? l 'i) (vector-set! counts 2 (add1 (vector-ref counts 2)))]
    [(symbol=? l 'o) (vector-set! counts 3 (add1 (vector-ref counts 3)))]
    [(symbol=? l 'u) (vector-set! counts 4 (add1 (vector-ref counts 4)))]
    [else (void)]))

(define (count-vowels-bv chars)
  (local ((define (count-vowel x chars)
            (cond
              [(empty? chars) 0]
              [else (cond
                      [(symbol=? x (first chars))
                       (+ (count-vowel x (rest chars)) 1)]
                      [else (count-vowel x (rest chars))])])))
    (build-vector 5 (lambda (i) 
                      (cond
                        [(= i 0) (count-vowel 'a chars)]
                        [(= i 1) (count-vowel 'e chars)]
                        [(= i 2) (count-vowel 'i chars)]
                        [(= i 3) (count-vowel 'o chars)]
                        [(= i 4) (count-vowel 'u chars)])))))

;; #\a = 97
;; #\z = 122
;building a list of random lowercase character symbols 
(define RS (build-list 5000 (lambda (n) (string->symbol (string (integer->char (+ 97 (random 25))))))))

;measuring 
(time (count-vowels RS))
(time (count-vowels-bv RS))

;building a list of random grades 
(define RN (build-list 5000 (lambda (n) (+ 2 (random 4)))))

;; histogram : list-of-grades -> vector-of-numbers 
;; for grades 2, 3, 4, 5 calculate how many occure in the list of grades 
;; return a vector of length 4 with amounts in the respective slots
(define (histogram alog0)
  (local ((define hvec (vector 0 0 0 0))
          (define (h alog)
            (cond
              [(empty? alog) hvec]
              [else (local ((define ref (- (first alog) 2)))
                      (begin (vector-set! hvec ref (add1 (vector-ref hvec ref)))
                             (h (rest alog))))])))
    (h alog0)))
 
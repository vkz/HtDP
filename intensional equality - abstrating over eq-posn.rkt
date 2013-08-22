;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |intensional equality - abstrating over eq-posn|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; eq-posn : posn posn  ->  boolean
;; test for intensional equality 
;; i.e. determine whether two posn structures 
;; are affected by the same mutation 
(define (eq-posn p1 p2)
  (local (;; save old x values of p1 and p2
	  (define old-x1 (posn-x p1))
	  (define old-x2 (posn-x p2))
	  ;; modify both x fields of p1 and p2
	  (define effect1 (set-posn-x! p1 5))
	  (define effect2 (set-posn-x! p2 6))
	  ;; now compare the two fields
	  (define same (= (posn-x p1) (posn-x p2)))
	  ;; restore old values
	  (define effect3 (set-posn-x! p1 old-x1))
	  (define effect4 (set-posn-x! p2 old-x2)))
    same))

;; eq-struct : struct struct (struct->Any) (struct->(void))  ->  boolean
;; abstract over eq-posn so it works on any structure 
;; test for intensional equality 
;; i.e. determine whether two posn structures 
;; are affected by the same mutation 
(define (eq-struct p1 p2 struct-x set-struct-x!)
  (local (;; save old x values of p1 and p2
	  (define old-x1 (struct-x p1))
	  (define old-x2 (struct-x p2))
	  ;; modify both x fields of p1 and p2
	  (define effect1 (set-struct-x! p1 5))
	  (define effect2 (set-struct-x! p2 6))
	  ;; now compare the two fields
	  (define same (= (struct-x p1) (struct-x p2)))
	  ;; restore old values
	  (define effect3 (set-struct-x! p1 old-x1))
	  (define effect4 (set-struct-x! p2 old-x2)))
    same))

;; eq-struct :(struct->Any) (struct->(void))  ->  (struct scruct -> boolean)
;; better abstraction over eq-posn so it works on any structure 
;; test for intensional equality 
;; i.e. determine whether two posn structures 
;; are affected by the same mutation 
(define (eqv-struct struct-x set-struct-x!)
  (lambda (p1 p2) 
    (local (;; save old x values of p1 and p2
            (define old-x1 (struct-x p1))
            (define old-x2 (struct-x p2))
            ;; modify both x fields of p1 and p2
            (define effect1 (set-struct-x! p1 5))
            (define effect2 (set-struct-x! p2 6))
            ;; now compare the two fields
            (define same (= (struct-x p1) (struct-x p2)))
            ;; restore old values
            (define effect3 (set-struct-x! p1 old-x1))
            (define effect4 (set-struct-x! p2 old-x2)))
      same)))


;; TESTS - applying all 3 functions should result in equal lists
;; testing eq-posn
(list (eq-posn (make-posn 1 2) (make-posn 1 2))
      (local ((define p (make-posn 1 2)))
        (eq-posn p p))
      (local ((define p (make-posn 1 2))
              (define a (list p)))
        (eq-posn (first a) p)))
;; testing eq-struct
(list (eq-struct (make-posn 1 2) (make-posn 1 2) posn-x set-posn-x!)
      (local ((define p (make-posn 1 2)))
        (eq-struct p p posn-x set-posn-x! ))
      (local ((define p (make-posn 1 2))
              (define a (list p)))
        (eq-struct (first a) p posn-x set-posn-x!)))
;; testing eqv-struct
(define eqv-posn (eqv-struct posn-x set-posn-x!))
(list (eqv-posn (make-posn 1 2) (make-posn 1 2))
      (local ((define p (make-posn 1 2)))
        (eqv-posn p p))
      (local ((define p (make-posn 1 2))
              (define a (list p)))
        (eqv-posn (first a) p)))
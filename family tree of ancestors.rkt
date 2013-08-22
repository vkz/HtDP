;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |family tree of ancestors|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define-struct child (father mother name born eyes))

;; Oldest Generation:
(define Carl (make-child empty empty 'Carl 1926 'green))
(define Bettina (make-child empty empty 'Bettina 1926 'green))

;; Middle Generation:
(define Adam (make-child Carl Bettina 'Adam 1950 'yellow))
(define Dave (make-child Carl Bettina 'Dave 1955 'black))
(define Eva (make-child Carl Bettina 'Eva 1965 'blue))
(define Fred (make-child empty empty 'Fred 1966 'pink))

;; Youngest Generation: 
(define Gustav (make-child Fred Eva 'Gustav 1988 'brown))
(define Amber (make-child empty empty 'Amber 1989 'hazel))

(define Hal (make-child Gustav Amber 'Hal 2009 'hazel))
;;=======================================================

;; A family-tree-node (child) is either
;; - empty
;; - (make-struct father mother name eyes)
;; where father is ftn, mother is ftn, name and eyes are symbols

;; count-ancestors : ftn -> Number
;; returns the number of direct ancestors
; given Carl -> 0
; given Eva -> 2
; given Gustav -> 4
(define (count-ancestors a-ftree) 
  (cond
    [(empty? a-ftree) 0]
    [else (+ 1 
             (count-ancestors (child-father a-ftree))
             (count-ancestors (child-mother a-ftree)))]))

(check-expect (count-ancestors Carl) 1)
(check-expect (count-ancestors Eva) 3)
(check-expect (count-ancestors Gustav) 5)


(define (total-age a-ftree) 
  (cond
    [(empty? a-ftree) 0]
    [else (+ (- 2012 (child-born a-ftree))
             (total-age (child-father a-ftree))
             (total-age (child-mother a-ftree)))]))

(define (avg-age a-ftree)
  (/ (total-age a-ftree) (count-ancestors a-ftree)))

(check-expect (avg-age Carl) (- 2012 (child-born Carl)))
(check-expect (avg-age Eva) (/ (+ (- 2012 (child-born Eva))
                                  (- 2012 (child-born Carl))
                                  (- 2012 (child-born Bettina)))
                               3))

(define (proper-blue-eyed-ancestor? a-ftree)
  (local ((define (father? ch)
            (not (empty? (child-father ch))))
          
          (define (mother? ch)
            (not (empty? (child-mother ch)))))
    (or 
      (and (father? a-ftree) (or (symbol=? 'blue (child-eyes (child-father a-ftree)))
                             (proper-blue-eyed-ancestor? (child-father a-ftree))))
      (and (mother? a-ftree) (or (symbol=? 'blue (child-eyes (child-mother a-ftree)))
                             (proper-blue-eyed-ancestor? (child-mother a-ftree)))))))

(check-expect (proper-blue-eyed-ancestor? Carl) false)
(check-expect (proper-blue-eyed-ancestor? Eva) false)
(check-expect (proper-blue-eyed-ancestor? Gustav) true)

;; to-blue-eyed-ancestor : ftn  ->  path or false 
;; to compute the path from a-ftn tree to a blue-eyed ancestor
(define (to-blue-eyed-ancestor a-ftn) 
  (local ((define (father? ch)
            (not (empty? (child-father ch))))
          
          (define (mother? ch)
            (not (empty? (child-mother ch)))))
    (cond
      [(and (father? a-ftn) (symbol=? 'blue (child-eyes (child-father a-ftn))))
       (cons 'father empty)]
      [(and (father? a-ftn) (proper-blue-eyed-ancestor? (child-father a-ftn)))
       (append '(father) (to-blue-eyed-ancestor (child-father a-ftn)))]
      [(and (mother? a-ftn) (symbol=? 'blue (child-eyes (child-mother a-ftn))))
       (cons 'mother empty)]
      [(and (mother? a-ftn) (proper-blue-eyed-ancestor? (child-mother a-ftn)))
       (append '(mother) (to-blue-eyed-ancestor (child-mother a-ftn)))]
      [else false])))
                           

(check-expect (to-blue-eyed-ancestor Dave) false)
(check-expect (to-blue-eyed-ancestor Gustav) '(mother))
(check-expect (to-blue-eyed-ancestor Hal) '(father mother))

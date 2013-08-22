;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |more accumulators|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define-struct ex (left right))
(define-struct lam (arg body))
;; Expression is one of:
;; - Symbol or
;; - struct (make-lam Symbol Expression)
;; - struct (make-ex Expression Expression)

;;(lambda (x) y)
(define T1 (make-lam 'x 'y))

;;((lambda (x) x) 
;; (lambda (x) x))
(define T2 (make-ex (make-lam 'x 'x)
                    (make-lam 'x 'x)))

;;(((lambda (y)
;;    (lambda (x) y))
;;  (lambda (z) z))
;; (lambda (w) w))
(define T3 (make-ex (make-ex (make-lam 'y 
                                       (make-lam 'x 'y)) 
                             (make-lam 'z 'z)) 
                    (make-lam 'w 'w)))

;; free-or-bound : Lam  ->  Lam 
;; to replace each non-binding occurrence of a variable in a-lam 
;; with 'free or 'bound, depending on whether the 
;; occurrence is bound or not.
(define (free-or-bound a-lam0) 
  (local (; parse : Lam (listof Symbols) -> Lam
          (define (parse a-lam accumulator)
            (cond
              [(symbol? a-lam) (if (member? a-lam accumulator) 'bound 'free)]               
              [(lam? a-lam) (make-lam (parse (lam-arg a-lam) accumulator)
                                      (parse (lam-body a-lam) (cons (lam-arg a-lam) accumulator)))]
              [else (make-ex (parse (ex-left a-lam) accumulator)
                             (parse (ex-right a-lam) accumulator))]))
          )
    (parse a-lam0 empty)))

(check-expect (free-or-bound T1) (make-lam 'free 'free))
(check-expect (free-or-bound T2) (make-ex (make-lam 'free 'bound)
                                          (make-lam 'free 'bound)))
(check-expect (free-or-bound T3) (make-ex (make-ex (make-lam 'free 
                                                             (make-lam 'free 'bound))
                                                   (make-lam 'free 'bound))
                                          (make-lam 'free 'bound)))

;; unique-binding : Lam  ->  Lam 
;; to replace variables names of binding occurrences and their bound
;; counterparts so that no name is used twice in a binding occurrence
(define (unique-binding a-lam0) 
  (local (; parse : Lam (listof Symbol) (listof Symbol) -> Lam
          (define (parse a-lam originals uniques)
            (cond
              [(symbol? a-lam)
               (local (;i-or-false : (listof Symbols) -> Number or false
                       (define (find-symbol a-los)
                         (cond
                           [(empty? a-los) false]
                           [else (if (symbol=? a-lam (first a-los)) 
                                     (- (length originals) (length a-los))
                                     (find-symbol (rest a-los)))]))
                       (define IND (find-symbol originals)))
                 (if (boolean? IND) (gensym a-lam) (list-ref uniques IND)))]    
              
              [(lam? a-lam) 
               (local ((define ORIGS (cons (lam-arg a-lam) originals))
                       (define UNIQUES (cons (gensym (lam-arg a-lam)) uniques)))
                 (make-lam (parse (lam-arg a-lam) ORIGS UNIQUES)
                           (parse (lam-body a-lam) ORIGS UNIQUES)))]
              
              [else (make-ex (parse (ex-left a-lam) originals uniques)
                             (parse (ex-right a-lam) originals uniques))]))
          )
    (parse a-lam0 empty empty)))
 
                                
(unique-binding T1) 
(unique-binding T2)
(unique-binding T3)
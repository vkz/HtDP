;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |simple text parsing|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define TEST1 
  '(The TeachScheme! Project aims to improve the 
  problem-solving and organization skills of high 
  school students. It provides software and lecture 
  notes as well as exercises and solutions for teachers.))

(define TEST2 
  '(The TeachScheme Web Page
  Here you can find: 
  (LectureNotes for Teachers)
  (Guidance for (DrScheme: a Scheme programming environment))
  (Exercise Sets)
  (Solutions for Exercises)
  For further information: write to scheme@cs
  for all other))

;; size : WP  ->  number
;; to count the number of symbols that occur in a-wp
(define (size a-wp) 
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp)) (+ 1 (size (rest a-wp)))]
    [else (+ (size (first a-wp)) (size (rest a-wp)))]))

(check-expect (size TEST2) 32)

;; occurs: Symbol WP -> number
;; count the number of times the symbol appears on the web-page (no recursion 
;; into the embedded pages)
(define (occurs s a-wp)
  (cond 
    [(empty? a-wp) 0]
    [(and (symbol? (first a-wp)) (symbol=? s (first a-wp))) (+ 1 (occurs s (rest a-wp)))]
    [else (occurs s (rest a-wp))]))

(check-expect (occurs 'and TEST1) 3)
(check-expect (occurs 'for TEST2) 1)

(define (occurs-everywhere s a-wp)
  (cond 
    [(empty? a-wp) 0]
    [(symbol? (first a-wp)) (if (symbol=? s (first a-wp))
                                (+ 1 (occurs-everywhere s (rest a-wp)))
                                (occurs-everywhere s (rest a-wp)))]                                          
    [else (+ (occurs-everywhere s (first a-wp)) (occurs-everywhere s (rest a-wp)))]))

(check-expect (occurs-everywhere 'for TEST1) 1)
(check-expect (occurs-everywhere 'for TEST2) 4)


;; replace: Symbol Symbol WP -> WP 
;; replace the old symbol with the new one and return the resulting page (no structural changes to the page)
(define (replace old new a-wp)
  (cond
    [(empty? a-wp) empty]
    [(symbol? (first a-wp)) (cond
                              [(symbol=? old (first a-wp)) (cons new (replace old new (rest a-wp)))]
                              [else (cons (first a-wp) (replace old new (rest a-wp)))])]
    [else (cons (replace old new (first a-wp)) (replace old new (rest a-wp)))]))
    

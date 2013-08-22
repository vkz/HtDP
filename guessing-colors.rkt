;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname guessing-colors) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "gui.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "gui.ss" "teachpack" "htdp")))))
;; Constants:

;; the legitimate colors 
(define COLORS
  (list 'black 'white 'red 'blue 'green 'gold 'pink 'orange 'purple 'navy))

;; the number of colors
(define COL# (length COLORS)) 

;; Data Definition:
;; A color is a symbol on COLORS. 

;; target1, target2 : color 
;; the two variables represent the two colors that the first player chose
(define counter 0)
(define target1 (first COLORS))
(define target2 (first COLORS))

;; the legitimate answers
(define ANSWERS
  (list 'perfect! ; if the first target is equal to the first guess and the second target is equal to the second guess;
        'OneColorAtCorrectPosition ; if the first guess is equal to the first target or the second guess is equal to the second target;
        'OneColorOccurs ; if either of the guesses is one of the two targets;
        'NothingCorrect)) ; otherwise.

;; check-color : color color color color -> answer
;; checks the two given colors against the targets and prints the appropriate answer
(define (check-colors guess1 guess2 target1 target2)
  (local ((define HIT1? (symbol=? guess1 target1))
          (define HIT2? (symbol=? guess2 target2))
          (define T-LIST (list target1 target2)))
    (cond 
      [(and HIT1? HIT2?) (list-ref ANSWERS 0)]
      [(or HIT1? HIT2?) (list-ref ANSWERS 1)]
      [(or (member? guess1 T-LIST)
           (member? guess2 T-LIST)) (list-ref ANSWERS 2)]
      [else (list-ref ANSWERS 3)])))

(check-expect (check-colors 'black 'black target1 target2) (list-ref ANSWERS 0))
(check-expect (check-colors 'black 'gold target1 target2) (list-ref ANSWERS 1))
(check-expect (check-colors 'gold 'black target1 target2) (list-ref ANSWERS 1))
(check-expect (check-colors 'black 'red 'red 'gold) (list-ref ANSWERS 2))
(check-expect (check-colors 'black 'green 'red 'gold) (list-ref ANSWERS 3))
;; master :  ->  void
;; effect: set target1 and target2 to two randomly chosen items from COLORS
;;         set counter to zero
(define (master)
  (local ((define (random-pick)
            (list-ref COLORS (random COL#))))
    (begin
      (set! counter 0)
      (set! target1 (random-pick))
      (set! target2 (random-pick)))))


;; master-check : color color  ->  symbol
;; to determine how many colors at how many positions are guessed correctly
;; The function defers to check-color, the solution of exercise 5.1.5.
;; effect : increment the counter
(define (master-check guess1 guess2)
  (local ((define CHECK (check-colors guess1 guess2 target1 target2))
          (define COUNT (add1 counter))
          (define PRINT (string-append (symbol->string CHECK)
                     "\n\n"
                     "tried " (int->string COUNT) " times")))
    (begin 
      (set! counter (add1 counter))
      (cond 
        [(symbol=? CHECK (list-ref ANSWERS 0)) (begin 
                                                 (master)
                                                 PRINT)]
        [else PRINT]))))


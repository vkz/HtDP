;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname cards-functional-style-no-mutators) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; A HIGHLY INSTRUCTIVE EXERCISE (HTDPv1 exercise 41.3.4)
;; (idea for implementation found in HTDPv1 exercise 40.2.2)

;; How would you emulate a card game? 
;; Normally a hand is a (make-hand rank suit next) structure where next is empty or a hand
;; To add another card you'd need to use mutators
;;
;;
;; Below is a card game emulation "functional style" i.e. 
;; - no structures
;; - no mutators
;; - no lists
;; - only local variables used as state-vars and functions
;; Behavior is replicated by passing around functions.
;; Hand is therefore a function

;; create-hand: number symbol -> hand 
;; where hand is just a lambda applied to the local variables
;; NOTE: local vars rank0, suit0, next-hand can only be changed in the scope local to 
;; the (create-hand ...) function, hence a setter (set-next-hand! ...) provided
(define (create-hand rank0 suit0)
  (local ((define next-hand empty)
          (define (set-next-hand! new-hand) (set! next-hand new-hand)))
    (lambda (select) (select rank0 suit0 next-hand set-next-hand!))))

;; set-next-hand! : hand hand -> (void)
(define (set-next-hand! hand new-next-hand)
  (hand (lambda (r s next-hand set-nh!) (set-nh! new-next-hand))))

;; show-rank : hand -> number
(define (hand-rank hand)
  (hand (lambda (r s next-hand set-next-hand!) r)))

;; show-suit: hand -> symbol
(define (hand-suit hand)
  (hand (lambda (r s next-hand set-next-hand!) s)))

;; next-hand: hand -> empty or hand
(define (next-hand hand)
  (hand (lambda (r s nh set-nh) nh)))

;; insert-sorted: number symbol hand -> hand
;; assuming hand is sorted by rank insert a new card
(define (insert-sorted! r s hand)
  (local ((define new-card (create-hand r s))
          (define (insert-aux h)
            (cond 
              [(empty? (next-hand h)) (set-next-hand! h new-card)]
              [else (cond
                      [(>= (hand-rank h) r (hand-rank (next-hand h)))
                       (begin 
                         (set-next-hand! new-card (next-hand h))
                         (set-next-hand! h new-card))]
                      [else (insert-aux (next-hand h))])])))
    (cond
      [(> r (hand-rank hand)) (begin
                                (set-next-hand! new-card hand)
                                new-card)]
      [else (begin
              (insert-aux hand)
              hand)])))

;; add-at-end!: number symbol hand -> (void)
;; effect: add another card at the end of the hand
(define (add-at-end! rank0 suit0 hand)
  (hand (lambda (rank suit next-hand set-next-hand!) 
          (cond
            [(empty? next-hand) 
             (set-next-hand! (create-hand rank0 suit0))]
            [else (add-at-end! rank0 suit0 next-hand)]))))
;; show-hand: hand -> list-of-cards 
;; where a card is a (list rank suit)
(define (show-hand hand)
  (hand (lambda (rank suit next-hand set-next-hand!)
          (cond 
            [(empty? next-hand) (cons (list rank suit) empty)]
            [else (cons (list rank suit) (show-hand next-hand))]))))
          
;; create a hand of 3 cards and then show them    
(define hand0 (create-hand 6 'spades))
(begin (set! hand0 (insert-sorted! 3 'clubs hand0))
       (set! hand0 (insert-sorted! 10 'hearts hand0))
       (set! hand0 (insert-sorted! 5 'diamonds hand0))
       (show-hand hand0))
 
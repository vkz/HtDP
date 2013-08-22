;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hangman) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "gui.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "gui.ss" "teachpack" "htdp")))))
;; A letter is a symbol in: 'a ... 'z plus '_
;; A word is a (listof letter).
;; A body-part is one of the following symbols: 
;; '(head body right-arm left-arm right-leg left-leg)
;;;;;;;;;;;;;;;;;;;;;;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define WORDS 
  '((h e l l o)
    (w o r l d)
    (i s)
    (a)
    (s t u p i d)
    (p r o g r a m)
    (a n d)
    (s h o u l d)
    (n e v e r)
    (b e)
    (u s e d)
    (o k a y)
    ))

(define HM empty)

;; hangman is an interface
;; "init" :: (void)
;; "guess" :: symbol -> response
;; "reveal" :: -> (listof word word)

;; make-hangman : listof words -> (symbol -> response)
;; given a non-empty list of words choses one at random and returns a hangman-guess function
(define (make-hangman a-low)
  (local ((define PARTS '(head body right-arm left-arm right-leg left-leg))
          
          ;; chosen-word : word
          ;; the word that the player is to guess
          (define chosen-word (first a-low))
          ;; status-word : word
          ;; represents which letters the player has and hasn't guessed
          (define status-word (first a-low))
          ;; body-parts-left : (listof body-part)
          ;; represents the list of body parts that are still "available"
          (define body-parts-left PARTS)

          ;; hangman :  ->  void
          ;; effect: initialize chosen-word, status-word, and body-parts-left
          (define (hangman-init)
            (local (;make-status-word : word -> word
                    (define (make-status-word w)
                      (map (lambda (s) '_) w))
                    )
              (begin
                (set! chosen-word (list-ref a-low (random (length a-low))))
                (set! status-word (make-status-word chosen-word))
                (set! body-parts-left PARTS))))

          ;; reveal-list : word word letter  ->  word
          ;; to compute the new status word
          (define (reveal-list chosen-word status-word guess)
            (local ((define (reveal-letter chosen-letter status-letter)
                      (cond 
                        [(symbol=? guess chosen-letter) guess]
                        [else status-letter])))
              (map reveal-letter chosen-word status-word)))
          
          ;; hangman-guess : letter  ->  response
          ;; to determine whether the player has won, lost, or may continue to play
          ;; and, if so, which body part was lost, if no progress was made
          ;; effects: (1) if the guess represents progress, update status-word
          ;; (2) if not, shorten the body-parts-left by one 
          (define (hangman-guess guess)
            (local ((define new-status (reveal-list chosen-word status-word guess)))
              (cond
                [(equal? new-status status-word)
                 (local ((define next-part (first body-parts-left)))
                   (begin 
                     (set! body-parts-left (rest body-parts-left))
                     (cond
                       [(empty? body-parts-left) (list "The End" chosen-word)]
                       [else (list "Sorry" next-part status-word)])))]
                [else
                 (cond
                   [(equal? new-status chosen-word) "You won"]
                   [else 
                    (begin 
                      (set! status-word new-status)
                      (list "Good guess!" status-word))])])))
          
          ;; hangman-manager : string -> (->) or void
          ;; strings are 'init 'guess 'reveal 
          (define (hangman-manager s) 
            (cond 
              [(string=? s "init") (hangman-init)]
              [(string=? s "guess") hangman-guess]
              [(string=? s "reveal") (list chosen-word status-word)]
              [else (error "hangman-manager: possible messages are init, guess, reveal")]))
          )
    (begin 
      (hangman-init)
      hangman-manager)))
          
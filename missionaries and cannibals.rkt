;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |missionaries and cannibals|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; just call MAIN
;; can change the MC and BOAT-CAP constants

(define MC 3)       ;number of missionaries = number of cannibals
(define BOAT-CAP 2) ;boat-capacity

;; State is a struct make-mcstate
;; - (mcstate-start) is a struct make-mc
;; - (mcstate-finish) is a struct make-mc 
;; - (mcstate-boat) is a symbol 'start or 'finish 
;; - (mcstate-path) is a (cons (make-trip start finish) path)
(define-struct mcstate (start finish boat path))
(define-struct trip (start finish boat))
(define-struct mc (m c))
;; Where
;; - (mc-m) is a number [0, MC]
;; - (mc-c) is a number [0, MC]

;; how-many : Symbol Symbol State -> number [0, MC]
;; how-many 'm or 'c on the
;; 'start or 'finish bank of the river
(define TEST-MCSTATE (make-mcstate (make-mc 2 1) (make-mc 1 2) 'finish empty))
(define START (make-mcstate (make-mc MC MC) (make-mc 0 0) 'start (list (make-trip (make-mc MC MC) (make-mc 0 0) 'start))))
(check-expect (how-many 'c 'finish TEST-MCSTATE) 2)
(check-expect (how-many 'c 'start  TEST-MCSTATE) 1)
(check-expect (how-many 'm 'finish TEST-MCSTATE) 1)
(check-expect (m@finish TEST-MCSTATE) 1)
(check-expect (c@start TEST-MCSTATE) 1)


(define (how-many who where st)  
  (cond  
    [(symbol=? 'm who) (cond 
                                    [(symbol=? 'start where) (mc-m (mcstate-start st))]
                                    [(symbol=? 'finish where) (mc-m (mcstate-finish st))]
                                    [else (error "how-many: wrong symbol")])]
    [(symbol=? 'c who) (cond 
                                    [(symbol=? 'start where) (mc-c (mcstate-start st))]
                                    [(symbol=? 'finish where) (mc-c (mcstate-finish st))]
                                    [else (error "how-many: wrong symbol")])]
    [else (error "how-many: wrong symbol")])) 
(define (m@start st) (how-many 'm 'start st))
(define (m@finish st) (how-many 'm 'finish st))
(define (c@start st) (how-many 'c 'start st))
(define (c@finish st) (how-many 'c 'finish st))

;; Load is a struct (make-load) where
;; (<= (load-m) MC) and 
;; (<= (load-c) MC) and
;; (<= (+ load-m load-c) BOAD-CAP)
(define-struct load (m c))

;; boat-load? : make-load -> boolean
;; checks if all conditions for being a load are met
(define (boat-load? l)
  (and (load? l) (<= (load-m l) MC) (<= (load-c l) MC) (<= 1 (+ (load-m l) (load-c l)) BOAT-CAP)))

;; make-BOAD-LOADS : Number -> Listof load
;; consumes a maximum capacity and produces a list of all possible boat loads
(define (make-BOAT-LOADS cap) 
  (local (;add-m : Number Listof Loads -> Listof Loads
          (define (add-m mcap accumulator)
            (cond
              [(< mcap 0) accumulator]
              [else (local (;add-c : Number Listof loads -> Listof loads
                            (define (add-c ccap accumulator) 
                              (cond 
                                [(< ccap 0) accumulator]
                                [else (if (boat-load? (make-load mcap ccap))
                                          (add-c (sub1 ccap) (cons (make-load mcap ccap) accumulator))
                                          (add-c (sub1 ccap) accumulator))])))
                      (add-m (sub1 mcap) (add-c MC accumulator)))])))
    (add-m MC empty)))

;; List of all possible boat loads
(define BOAT-LOADS (make-BOAT-LOADS BOAT-CAP))

;; valid-state? : state -> boolean
(define (valid-mcstate st)
  (and (>= (c@start st) 0) (>= (m@start st) 0) (>= (c@finish st) 0) (>= (m@finish st) 0)
       (if (> (m@start st) 0) (>= (m@start st) (c@start st)) true) 
       (if (> (m@finish st) 0) (>= (m@finish st) (c@finish st)) true)
       (not (member? (first (mcstate-path st)) (rest (mcstate-path st))))))
       
;; valid-states : listof states -> listof states
;; filters invalid states out
(define (valid-states a-los)
  (filter valid-mcstate a-los))

;; state-successors : state -> listof states
;; produce a listof all valid succession states to the given
(define (state-successors st)
  (local (;boat-trip : boat-load -> state
          (define (boat-trip a-load)
            (local ((define START- (make-mc (- (m@start st) (load-m a-load))
                                              (- (c@start st) (load-c a-load))))
                      (define FINISH+ (make-mc (+ (m@finish st) (load-m a-load))
                                               (+ (c@finish st) (load-c a-load))))
                      (define START+ (make-mc (+ (m@start st) (load-m a-load))
                                              (+ (c@start st) (load-c a-load))))
                      (define FINISH- (make-mc (- (m@finish st) (load-m a-load))
                                               (- (c@finish st) (load-c a-load)))))
              (cond 
                [(symbol=? 'start (mcstate-boat st)) (make-mcstate START- 
                                                                   FINISH+ 
                                                                   'finish  
                                                                   (cons (make-trip START- FINISH+ 'finish)
                                                                         (mcstate-path st)))]
                [(symbol=? 'finish (mcstate-boat st)) (make-mcstate START+ 
                                                                    FINISH-
                                                                    'start
                                                                    (cons (make-trip START+ FINISH- 'start)
                                                                          (mcstate-path st)))]))))
    (filter valid-mcstate (map boat-trip BOAT-LOADS))))
           
;; next-states : Listof states -> Listof states
;; list of states that are reachable from the ones given
(define (next-states a-los0 with-dupes?)
  (local (;no-dupes : listof states -> listof states
          (define (no-dupes a-los)
            (cond 
              [(empty? a-los) empty]
              [else (cons (first a-los) 
                          (filter (lambda (st) (not (equal? st (first a-los)))) (no-dupes (rest a-los))))])))
    (if with-dupes? 
        (foldr (lambda (st los) (append (state-successors st) los))
           empty 
           a-los0)
        (no-dupes (foldr (lambda (st los) (append (state-successors st) los))
           empty 
           a-los0)))))
    
;; final-state? : state -> boolean 
(define (final-state? st)
  (and (equal? (mcstate-start st) (make-mc 0 0))
       (equal? (mcstate-finish st) (make-mc MC MC))
       (equal? (mcstate-boat st) 'finish)
       (not (empty? (mcstate-path st))))) 

;; final-states : listof states -> listof states
;; only return the final states
(define (final-states a-los)
  (filter final-state? a-los))

;; mc-solvable? : listof states -> boolean
;; produce new states until the (final-state?) is found
(define (mc-solvable? a-los)
  (cond
    [(empty? a-los) "not solvable"]
    [(empty? (final-states a-los)) (mc-solvable? (next-states a-los true))]
    [else true]))

;; mc-solution : listof states -> listof trips
;; print list of trips that lead to solutions
(define (mc-solutions a-los)
  (cond
    [(empty? a-los) "not solvable"]
    [(empty? (final-states a-los)) (mc-solutions (next-states a-los true))]
    [else (map mcstate-path (filter final-state? a-los))])) 


(define MAIN (mc-solutions (list START))) 
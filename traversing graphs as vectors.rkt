;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |traversing graphs as vectors|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define Graph-as-list 
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))


(define Graph
  (vector '(1 4)
          '(4 5)
          '(3)
          empty
          '(2 5)
          '(3 6)
          empty))
          

(define Cyclic-graph-as-list
  '((A (B E))
    (B (E F))
    (C (B D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

(define Cyclic-graph
  (vector '(1 4)
          '(4 5)
          '(1 3)
          empty
          '(2 5)
          '(3 6)
          empty))
   

; ;; neighbours : node graph -> (listof node)
; ;; to return the neighbours (listof node) for the given node
; ;; empty if the node has no neighbours
; (define (neighbours node graph)
;   (local ((define FOUNDNODE (filter (lambda (n) (equal? (first n) node)) graph)))
;     (if (empty? FOUNDNODE) empty (second (first FOUNDNODE)))))
;           
; (check-expect (neighbours 'L Graph) empty) ; no such symbol
; (check-expect (neighbours 'D Graph) empty) ; no neighbours
; (check-expect (neighbours 'B Graph) (list 'E 'F)) ; have noighbours

;; find-route : node node graph  ->  (listof node) or false
;; to create a path from origination to destination in G
;; if there is no path, the function produces false
;; FALLS INTO INFINITE LOOP ON CYCLES - DOESN'T WORK FOR CYCLICAL GRAPHS
(define (find-route origination destination G)
  (local (;; neighbours : node graph -> (listof node)
          ;; to return the neighbours (listof node) for the given node
          ;; empty if the node has no neighbours
          (define (neighbours node)
            (vector-ref G node))

          ;; find-route/list : (listof node) node graph  ->  (listof node) or false
          ;; to create a path from some node on lo-originations to destination
          ;; if there is no path, the function produces false
          (define (find-route/list lo-or)
            (cond 
              [(empty? lo-or) false]
              [else (local ((define possible-route
                              (find-route (first lo-or) destination G)))
                      (cond
                        [(boolean? possible-route) (find-route/list (rest lo-or))]
                        [else possible-route]))])))
    (cond
      [(= origination destination) (list destination)]
      [else (local ((define possible-route
                      (find-route/list (neighbours origination))))
              (cond 
                [(boolean? possible-route) false]
                [else (cons origination possible-route)]))])))

; ;; find-route/list : (listof node) node graph  ->  (listof node) or false
; ;; to create a path from some node on lo-originations to destination
; ;; if there is no path, the function produces false
; (define (find-route/list lo-or destination G)
;   (cond 
;     [(empty? lo-or) false]
;     [else (local ((define possible-route
;                     (find-route (first lo-or) destination G)))
;             (cond
;               [(boolean? possible-route) (find-route/list (rest lo-or) destination G)]
;               [else possible-route]))]))

;(check-expect (find-route '7 '3 Graph) false) ; 'L symbol (origination) is not on the graph
;(check-expect (find-route '2 '7 Graph) false) ; 'L symbol (destination) is not on the graph
(check-expect (find-route '2 '2 Graph) (list '2)) ; origination = destination
(check-expect (find-route '2 '3 Graph) (list '2 '3)) ; destination among immediate neighbours
(check-expect (find-route '1 '6 Graph) (list '1 '4 '5 '6)) ; should choose the shortest route (list 'B 'F 'G), yet now it just chooses the first it finds (list 'B 'E 'F 'G)



;; test-on-all-nodes : Graph -> (list-of (list-of Nodes or false))
;; produce the result of applying the find-route to every pair of nodes in the graph
(define (test-on-all-nodes G)
  (local ((define LG (vector-length G))
          
          ;routes-to : number number -> (listof ((listof nodes) or false))
          (define (routes-to origination destination)
            (cond
              [(< destination LG) (cons (find-route origination destination G)
                                        (routes-to origination (add1 destination)))]
              [else empty]))
          
          ;routes-from : number -> listof ((listof nodes) or false))
          (define (routes-from n)
            (cond
              [(< n LG) (append (routes-to n 0) (routes-from (add1 n)))]
              [else empty]))
          )
   (routes-from 0)))
        


;; every-other-node-but : Node Graph -> List-of Nodes
;; produces every node on the graph but the one given
(define (every-other-node-but node G)
  (local ((define (strip-add next tail)
            (if (equal? (first next) node) 
                tail
                (cons (first next) tail)))
          )
    (foldr strip-add empty G)))

;(check-expect (every-other-node-but 'D Graph) '(A B C E F G))
;(check-expect (every-other-node-but 'A Graph) '(B C D E F G))

;; route-to/list : Node (List-of Nodes) Graph -> List-of (false or List-of-Nodes)
;; produce a list of routes from the given node to every node in the list (false otherwise)
(define (route-to/list node lon G) 
  (map (lambda (n) (find-route node n G)) lon))


;1000times : Graph -> run test 1000 times
;run test-on-all-nodes 1000 times on the graph G
(define (1000times G counter)
  (cond
    [(< counter 500) (1000times G (add1 (+ counter (- (length (test-on-all-nodes G))
                                                      (length (test-on-all-nodes G))))))]
    [else false])) 
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |simple-graphs and mutators|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define-struct node (name visited to))

;; create-node : symbol  ->  node
;; to create a simple graph node that contains itself
(define (create-node name)
  (local ((define the-node (make-node name  false false)))
    (begin
      (set-node-to! the-node the-node)
      the-node)))

;; connect-nodes : symbol symbol graph  ->  void
;; effect: to mutate the to field in the structure named 
;; from-name so that it contains the structure named to-name
(define (connect-nodes from-name to-name a-graph)
  (set-node-to! (lookup-node from-name a-graph)
                (lookup-node to-name a-graph)))

;; lookup-node : symbol graph  ->  node or false
;; to lookup up the node named x in a-graph
(define (lookup-node x a-graph) 
  (local ((define filtered-list (filter (lambda (a-node) (equal? (node-name a-node) x)) a-graph)))
    (cond
      [(empty? filtered-list) (error "lookup-node failed: no such node")]
      [else (first filtered-list)])))

;; symbolic-graph-to-structures: listof pairsof symbols -> simple graph
;; turn the list of pairs into a list of nodes like the-graph below
(define (symbolic-graph-to-structures a-lop)
  (local ((define uninit-graph (map (lambda (pair) (create-node (first pair))) a-lop)))
    (begin (map (lambda (pair) (connect-nodes (first pair) (second pair) uninit-graph))
                a-lop)
           uninit-graph)))

(define (reset-all-visited! a-sg)
            (cond 
              [(empty? a-sg) (void)]
              [else (begin (set-node-visited! (first a-sg) false)
                           (reset-all-visited! (rest a-sg)))]))

(define (route-exists? orig dest sg)
  (local ((define (route-exists-aux? orig dest)
            (cond
              [(eq? orig dest) true]
              [(node-visited orig) false]
              [else
               (begin
                 (set-node-visited! orig true)
                 (route-exists-aux? (node-to orig) dest))])))
    (begin
      (reset-all-visited! sg)
      (route-exists-aux? orig dest)))) 

(check-expect (route-exists? (lookup-node 'A the-graph)
                             (lookup-node 'F the-graph)
                             the-graph)
              false)
(check-expect (route-exists? (lookup-node 'A the-graph)
                             (lookup-node 'D the-graph)
                             the-graph)
              false)
(check-expect (route-exists? (lookup-node 'B the-graph)
                             (lookup-node 'E the-graph)
                             the-graph)
              true)
(check-expect (route-exists? (lookup-node 'D the-graph)
                             (lookup-node 'C the-graph)
                             the-graph)
              true)

;; reachable: node -> (void)
;; set node-visited to true for all those nodes that can be reached from the given
(define (reachable a-node sg)
  (local ((define (reachable-aux nd)
            (cond
              [(equal? (node-visited nd) true) (void)]
              [else (begin
                      (set-node-visited! nd true)
                      (reachable-aux (node-to nd)))])))
  (begin
    (reset-all-visited! sg)
    (reachable-aux a-node))))


(define another-graph
  (symbolic-graph-to-structures '((A B) (B C) (C E) (D E) (E B) (F F))))
 
;; the-graph : graph 
;; the list of all available nodes 
(define the-graph
  (list (create-node 'A)
        (create-node 'B)
        (create-node 'C)
        (create-node 'D)
        (create-node 'E)
        (create-node 'F)))

;; setting up the graph: 
(begin
  (connect-nodes 'A 'B the-graph)
  (connect-nodes 'B 'C the-graph)
  (connect-nodes 'C 'E the-graph)
  (connect-nodes 'D 'E the-graph)
  (connect-nodes 'E 'B the-graph))
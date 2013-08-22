;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |binary trees|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; Binary Tree (bt=node) is:
;; - false
;; - (make-node Number Symbol BT BT)
(define-struct node (soc name left right))

; ordinary Binary Tree
(define Anna (make-node 71 'anna false false))
(define Seth (make-node 17 'seth false false))
(define Alex (make-node 63 'alex Anna Seth))
(define Rachel (make-node 45 'rachel false false))
(define Kate (make-node 39 'kate Alex Rachel))
(define Fiona (make-node 100 'fiona false false))
(define Tony (make-node 19 'tony Fiona Kate))

;; Binary Search Tree (BST) is a BT
;; - false is a BST
;; - (make-node soc name left right) is a BST only if
;;  -- left and right are also BSTs
;;  -- all soc numbers in left are smaller than soc
;;  -- all soc numbels in right are larger than soc

; Binary Search Tree (BST)
(define C (make-node 22 'c false false))
(define E (make-node 34 'e false false))
(define D (make-node 30 'd C E))
(define G (make-node 54 'g false false))
(define F (make-node 39 'f D G))
(define A (make-node 10 'a false false))
(define B (make-node 19 'b A F))

; contains-num: Number BT -> Boolean
; assertains if a particular number appears on a binary tree
(define (contains-num num bt) 
  (cond
    [(false? bt) false]
    [else (or (equal? num (node-soc bt))
              (contains-num num (node-left bt))
              (contains-num num (node-right bt)))]))

; search-bt: Number BT -> Symbol or false
; assertains if a particular number appears on a binary tree
(define (search-bt num bt) 
  (cond
    [(false? bt) false]
    [else 
     (cond 
       [(equal? num (node-soc bt)) (node-name bt)]
       [(contains-num num (node-left bt)) (search-bt num (node-left bt))]
       [else (search-bt num (node-right bt))])]))

(check-expect (search-bt 16 Kate) false)
(check-expect (search-bt 39 Kate) 'kate)
(check-expect (search-bt 39 false) false)
(check-expect (search-bt 17 Kate) 'seth)

; inorder: BT -> listof Numbers
; prodeces a list of node numbers in the left-to-right order
(define (inorder bt) 
  (cond
    [(false? bt) empty]
    [else (append (inorder (node-left bt))
                  (cons (node-soc bt) empty)
                  (inorder (node-right bt)))]))
          

(check-expect (inorder false) empty)
(check-expect (inorder Alex) (list 71 63 17))
(check-expect (inorder Kate) (list 71 63 17 39 45))
(check-expect (inorder Tony) (list 100 19 71 63 17 39 45))
(check-expect (inorder B) (list 10 19 22 30 34 39 54))

; search-bst: Number BST -> Symbol or false
(define (search-bst num bst)
  (cond
    [(false? bst) false]
    [(= (node-soc bst) num) (node-name bst)]
    [(< num (node-soc bst)) (search-bst num (node-left bst))]
    [else (search-bst num (node-right bst))]))

(check-expect (search-bst 30 false) false)
(check-expect (search-bst 19 B) 'b)
(check-expect (search-bst 54 B) 'g)
(check-expect (search-bst 35 B) false)
(check-expect (search-bst 22 B) 'c)

; create-bst: Number Symbol BST -> BST
; create a node out of (number symbol) and add it to the BST instead false 
(define (create-bst num s bst) 
  (cond
    [(false? bst) (make-node num s false false)]
    [(and (< num (node-soc bst)) (false? (node-left bst)))
     (make-node (node-soc bst) 
                (node-name bst)
                (make-node num s false false)
                (node-right bst))]
    [(< num (node-soc bst)) (create-bst num s (node-left bst))]
    [(and (> num (node-soc bst)) (false? (node-right bst)))
     (make-node (node-soc bst) 
                (node-name bst)
                (node-left bst)
                (make-node num s false false))]
    [(> num (node-soc bst)) (create-bst num s (node-right bst))]
    [else bst]))
  

(check-expect (create-bst 56 'h G) 
              (make-node 54 'g false (make-node 56 'h false false)))
(check-expect (create-bst 66 'a false) 
              (make-node 66 'a false false))
(check-expect (create-bst 53 'b (create-bst 66 'a false))
              (make-node 66
                         'a
                         (make-node 53 'b false false)
                         false))
; Test sample
(define sample
  (reverse 
   (list (list 99 'o)
        (list 77 'l)
        (list 24 'i)
        (list 10 'h)
        (list 95 'g)
        (list 15 'd)
        (list 89 'c)
        (list 29 'b)
        (list 63 'a))))






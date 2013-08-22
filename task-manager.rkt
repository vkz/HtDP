;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname task-manager) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "gui.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "gui.ss" "teachpack" "htdp")))))


;

;remove, which removes the first task from the queue, if any;

;count, which computes the number of items in the queue.

;A user should start the task manager with start-task-manager.

;; A Task is a string 
;; A Task Queue is a list of Tasks





;;;;;;;;;;;;;;;;;;;;;;;;;;; MODEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Variables
;; TQ : Task Queue
;; initially empty
(define TQ empty)

;; start-task-manager : (void) -> (void)
;; effect : initialize the TQ to empty
(define (init-task-manager)
  (set! TQ empty))

;; enter-task : task -> task queue
;; which adds a task to end of the queue
;; effect : add the task to the end of TQ
(define (enter-task t)
  (cond 
    [(string=? t "") TQ]
    [else (begin 
            (set! TQ (append TQ (cons t empty)))
            TQ)]))

;; next : (void) -> task or (void) if no more tasks
;; which determines the next (first (rest TQ)) task in the queue, if any
;; effect : (first TQ) is removed
(define (next-task)
  (cond 
    [(empty? TQ) (void)]
    [else (local ((define HEAD (first TQ)))
            (begin 
              (set! TQ (rest TQ))
              HEAD))]))

;; current-task : -> task or ""
(define (current-task)
  (cond
    [(empty? TQ) ""]
    [else (first TQ)]))

;; count : (void) -> number
;; number of items in the TQ
(define (count-tasks)
   (length TQ))

;;;;;;;;;;;;;;;;;;;;;;;;;;; CONTROL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (enter x)
  (begin 
    (enter-task (text-contents enter-task-text))
    (draw-message current-task-msg (current-task))
    (draw-message number-msg (number->string (count-tasks)))))
    
(define (next x)
  (begin
    (next-task)
    (draw-message current-task-msg (current-task))
    (draw-message number-msg (number->string (count-tasks)))))
  
(define (quit-task-manager x) true)

;;;;;;;;;;;;;;;;;;;;;;;;;;; VIEW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-task-msg
  (make-message "Welcome to Task Manager"))

(define number-msg
  (make-message "2012"))

(define enter-task-text
  (make-text "Task:"))

(define enter-task-button
  (make-button "Enter" enter))

(define next-task-button
  (make-button "Next" next))

(define quit-button
  (make-button "Quit" quit-task-manager))

(define (main)
  (begin 
    (init-task-manager)
    (create-window 
     (list (list current-task-msg number-msg)
           (list enter-task-text enter-task-button)
           (list next-task-button quit-button)))))
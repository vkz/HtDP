;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define SCN (empty-scene 200 20))
(define CURS (rectangle 1 20 "solid" "red"))

; A State is a string

(define-struct editor (pre post))
; Editor is a struct (make-editor String String)
; interp. (make-editor s t) means the text to display is
; (string-append s t) with the cursor displayed between s and t

;KeyEvent is a single character string like
; 'left'
; 'right'
; '\b' = backspace - delete the char immediately before the cursor
; any single character 

; Editor KeyEvent -> Editor
; consumes an editor 'e' and a keystroke 'k' appending it to (editor-pre e) 
; unless it's a backspace or 'left' or 'right' which it should handle by respectively 
; removing one character immediate before the curs, moving one char to the left, to the right
(define (edit e k) 
  (cond
    [(key=? "\b" k) (make-editor (str-remove-last (editor-pre e)) (editor-post e))]
    [else (make-editor (string-append (editor-pre e) k) (editor-post e))])
  ) 

; String -> String
; if the string is not-empty remove the last character and return the resulting string
(define (str-remove-last s) 
  (if (= (string-length s) 0) 
      s
      (substring s 0 (- (string-length s) 1))))

; Editor -> Image
; place Editor (e) image on the canvas of 200x20
; using (text String 11 "black") for the editor text and
; using (rectangle 1 20 "solid" "red") for the cursor
(define (render e)
  (place-image/align 
   (underlay/xy 
    (text (string-append (editor-pre e) (editor-post e)) 11 "black")
    (image-width (text (editor-pre e) 11 "black")) -5
    CURS)
   1 10
   "left" "middle"
   SCN))

               
(define (main e)
  (big-bang e (on-key edit) (to-draw render)))

(main (make-editor " " ""))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |HW12 (4)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname editor-part-2)
                                                         (read-case-sensitive #t)
                                                         (teachpacks ())
                                                         (htdp-settings
                                                          #(#t
                                                            constructor
                                                            repeating-decimal #f #t none #f () #f)))

|#
(require 2htdp/image)
(require 2htdp/universe)

(define BACKGROUND (empty-scene 1000 500 "white"))
(define FSIZE-EDIT   20) ; the default text font size (can be changed at runtime)
(define FSIZE-MENU   25)

(define COLOR-EDIT   "black")
(define COLOR-MENU   "red")

(define COLOR-SEARCH "blue")
(define FSIZE-SEARCH 25)

(define LINE-SEP     "\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GENERAL UTILITY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; split : String 1String -> [List-of String]
; split the given string into a list of substrings defined by the given separator character.
; (The separator is not part of the substrings in the output list.)
(check-expect (split "First|Second|Third|Fourth" "|") (list "First" "Second" "Third" "Fourth"))
(check-expect (split "A|B|C|D"   "|") (list "A" "B" "C" "D"))
(check-expect (split "|A|B|C|D"  "|") (list "" "A" "B" "C" "D"))
(check-expect (split "|A|B|C|D|" "|") (list "" "A" "B" "C" "D" ""))
(check-expect (split ""          "|") (list ""))
(check-expect (split "A"         "|") (list "A"))
(check-expect (split "|"         "|") (list "" ""))
(check-expect (split "||"        "|") (list "" "" ""))
(define (split str sep)
  (local [(define (add-char-or-sep c los)
            (cond [(and (string=? c sep) (empty? los)) (list "" "")]
                  [(string=? c sep)                    (cons "" los)]
                  [(empty? los)                        (list c)]
                  [else                                (cons (string-append c (first los))
                                                             (rest los))]))]
    (foldr add-char-or-sep (list "") (explode str))))

; last : [X] [NEList-of X] -> X
; the last element of a non-empty list
(check-expect (last (list 1 2 3 4)) 4)
(check-expect (last (list 1))       1)
(define (last lox)
  (list-ref lox (sub1 (length lox))))

; subst : [X] X Nat [List-of X] -> [List-of X]
; substitute new for the element at pos in lox (error if does not exist)
(check-expect (subst "!" 0 (list 1 2 3)) (list "!" 2 3))
(check-expect (subst "?" 3 (list 0 1 2 3 4 5 6)) (list 0 1 2 "?" 4 5 6))
(define (subst new pos lox)
  (if (= pos 0)
      (cons new (rest lox))
      (cons (first lox) (subst new (sub1 pos) (rest lox)))))

; subst-two : [X] X X Nat [List-of X] -> [List-of X]
; substitute the *two* elements new1 and new2 for the element at pos (error if does not exist in lox)
(check-expect (subst-two 1 2 0 '(0)) (list 1 2))
(check-expect (subst-two 3 4 3 '(0 1 2 ? 5 6 7)) (list 0 1 2 3 4 5 6 7))
(define (subst-two new1 new2 pos lox)
  (if (= pos 0)
      (cons new1 (cons new2 (rest lox)))
      (cons (first lox) (subst-two new1 new2 (sub1 pos) (rest lox)))))

; subst-for-two : [X] X Nat [List-of X]-> [List-of X]
; substitute new for the *two* elements as positions pos and pos+1 (error if they do not exist in lox)
(check-expect (subst-for-two "X" 0 (list 0 1)) (list "X"))
(check-expect (subst-for-two "X" 2 (list 0 1 2 3 4 5 6)) (list 0 1 "X" 4 5 6))
(define (subst-for-two new pos lox)
  (if (= pos 0)
      (cons new (rest (rest lox)))
      (cons (first lox) (subst-for-two new (sub1 pos) (rest lox)))))

; char-insert : 1String Nat String -> String
; insert character c into s at position pos, or at end if pos = (string-length s)
(check-expect (char-insert "X" 0 "") "X")
(check-expect (char-insert "X" 0 "A") "XA")
(check-expect (char-insert "X" 2 "AB") "ABX")
(define (char-insert c pos s)
  (string-append (substring s 0 pos)
                 c
                 (substring s pos)))

; char-delete : Nat String -> String
; delete the character at the given position (error if does not exist)
(check-expect (char-delete 0 "Hello") "ello")
(check-expect (char-delete 2 "Hello") "Helo")
(define (char-delete pos s)
  (string-append (substring s 0 pos)
                 (substring s (add1 pos))))

; stack-images-left-aligned : [List-of Image] -> Image
; stack the images on top of each other, aligned on the left
(check-expect (stack-images-left-aligned '()) empty-image)
(check-expect (stack-images-left-aligned (list (text "A" 12 "black")
                                               (text "ABC" 13 "red")))
              (above/align "left"
                           (text "A" 12 "black")
                           (text "ABC" 13 "red")))
(define (stack-images-left-aligned loi)
  (local [(define (stack-image image stack)
            (above/align "left" image stack))]
    (foldr stack-image empty-image loi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA DEFINITIONS FOR A TEXT BUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Line is a String
; A line of text contained in some text buffer.
; No special characters (such as newlines) allowed.
(define line-0 "When I came back from Munich,")
(define (line-templ l)
  (... l ...))

; A Text is a String
; The text contained in some text buffer, in single-string format.
; Lines of text are separated by LINE-SEP
(define text-0
  (string-append
   "About a hiring visit by Edsger Dijkstra (1930-2002),"                        LINE-SEP
   "a famous computer scientist:"                                                LINE-SEP
   LINE-SEP
   "\"When I came back from Munich, it was September, and I was Professor of"    LINE-SEP
   "Mathematics at the Eindhoven University of Technology. Later I learned that" LINE-SEP
   "I had been the Department's third choice, after two numerical analysts had"  LINE-SEP
   "turned the invitation down; the decision to invite me had not been an easy"  LINE-SEP
   "one, on the one hand because I had not really studied mathematics, and on"   LINE-SEP
   "the other hand because of my sandals, my beard and my 'arrogance' (whatever" LINE-SEP
   "that may be).\""))

(define-struct buffer [lol lnum cnum fsize mode])

; A Buffer is a (make-buffer [NEList-of Line] Nat Nat PosInt String)
; a buffer of a text editor:
; - lol is the list of lines of the text.
;   This list is NEVER empty (see buffer-text-empty below)
; - lnum is the  line  number where the cursor is. Range: 0 .. len(lol)-1
; - cnum is the column number where the cursor is. Range: 0 .. len(s) . s = string at line lnum.
; - fsize is the font size
; - mode is the mode of the buffer:
;   * "EDIT"   : normal text input
;   * "MENU"   : menu mode, in which the command menu is displayed
(define buffer-text-empty (make-buffer (list "")               0 0 FSIZE-EDIT "EDIT"))
(define buffer-text   (make-buffer (list    "Line 0" "Line 1") 1 3 FSIZE-EDIT "EDIT"))
(define buffer-menu   (make-buffer (list    "Line 0" "Line 1") 1 3 FSIZE-EDIT "MENU"))
(define buffer-search (make-buffer (list    "" "Line 0" "Line 1") 1 3 FSIZE-EDIT "SEARCH"))
(define (buffer-templ buffer)
  (... (buffer-lol   buffer)
       (buffer-lnum  buffer)
       (buffer-cnum  buffer)
       (buffer-fsize buffer)
       (buffer-mode  buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UTILITY FUNCTIONS OPERATING ON A BUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; start-of-buffer? : Buffer -> Boolean
; are we at the top-left corner of the buffer?
(check-expect (start-of-buffer? buffer-text-empty) #t)
(check-expect (start-of-buffer? buffer-text)       #f)
(define (start-of-buffer? buffer)
  (= (buffer-lnum buffer) (buffer-cnum buffer) 0))

; end-of-buffer? : Buffer -> Boolean
; are we at the lower right corner of the buffer, i.e. last line, last column?
(check-expect (end-of-buffer? buffer-text-empty) #t)
(check-expect (end-of-buffer? buffer-text)       #f)
(define (end-of-buffer? buffer)
  (local [(define lol (buffer-lol buffer))]
    (and (= (buffer-lnum buffer) (sub1 (length lol)))
         (= (buffer-cnum buffer) (string-length (last lol))))))

; end-of-line : Buffer Nat -> Nat
; the end-of-line position of the given line in the buffer
(check-expect (end-of-line buffer-text-empty 0) 0)
(check-expect (end-of-line buffer-text       1) 6)
(define (end-of-line buffer lnum)
  (local [(define lol  (buffer-lol buffer))
          (define line (list-ref lol lnum))]
    (string-length line)))

; end-of-line? : Buffer -> Boolean
; are we at the end of the current line in the buffer?
(check-expect (end-of-line? buffer-text-empty) #t)
(check-expect (end-of-line? buffer-text)       #f)
(define (end-of-line? buffer)
  (local [(define lnum (buffer-lnum buffer))
          (define cnum (buffer-cnum buffer))]
    (= cnum (end-of-line buffer lnum))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN: THE BIG-BANG WORLD PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; editor : Text -> Text
; Launches the text editor, with text as the initial buffer contents.
; Returns the buffer contents as a Text.
; It can therefore be called in the form
; (define new-text (editor old-text))
; so that the buffer content after the editor finishes is saved in variable new-text
; which can then be stored in a file.
(define (editor initial-text)
  (lol->text
   (buffer-lol
    (big-bang (make-buffer (text->lol initial-text) 0 0 FSIZE-EDIT "EDIT")
      [name     "A Simple Text Editor"]
      [to-draw  draw-buffer]
      [on-key   process-key]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CONVERSIONS Text <-> [NEList-of Line]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; text->lol : Text -> [NEList-of Line]
; the text converted into a list of lines
(check-expect (text->lol "") (buffer-lol buffer-text-empty)) ; a special case
(check-expect (text->lol (string-append "Line 0" LINE-SEP "Line 1")) (buffer-lol buffer-text))
(define (text->lol text)
  (split text LINE-SEP))

; lol->text : [NEList-of Line] -> Text
; the buffer content converted into a Text, with LINE-SEP inserted as needed
(check-expect (lol->text (buffer-lol buffer-text-empty)) "")
(check-expect (lol->text (buffer-lol buffer-text))   (string-append "Line 0" LINE-SEP "Line 1"))
(define (lol->text lol)
  (local [(define (line-combine l s)
            (string-append l LINE-SEP s))
          (define text (foldr line-combine "" lol))]
    (substring text 0 (sub1 (string-length text))))) ; substring removes the trailing LINE-SEP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DRAWING THE BUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; draw-buffer : Buffer -> Image
; render the buffer in text- or cmd mode, as the case may be
(check-expect (draw-buffer buffer-text-empty) (draw-buffer-text-mode buffer-text-empty))
(check-expect (draw-buffer buffer-menu) (draw-buffer-menu-mode buffer-text-empty))
(check-expect (draw-buffer buffer-search) (draw-buffer-search-mode buffer-text-empty))
(define (draw-buffer buffer)
  (cond [(string=? (buffer-mode buffer) "EDIT")   (draw-buffer-text-mode   buffer)]
        [(string=? (buffer-mode buffer) "MENU")   (draw-buffer-menu-mode   buffer)]
        [(string=? (buffer-mode buffer) "SEARCH") (draw-buffer-search-mode buffer)])) ;; Exercise 3
      
; draw-buffer-text-mode : Buffer -> Image
; render a buffer in EDIT mode: the buffer text
(check-expect (draw-buffer-text-mode buffer-text-empty)
              (place-image/align (char->image-cursor " " FSIZE-EDIT COLOR-EDIT)
                                 0 0 "left" "top" BACKGROUND))
(define (draw-buffer-text-mode buffer)
  (local [(define lol   (buffer-lol   buffer))
          (define lnum  (buffer-lnum  buffer))
          (define cnum  (buffer-cnum  buffer))
          (define fsize (buffer-fsize buffer))]
    (place-image/align (stack-images-left-aligned (lines->images lol lnum cnum fsize COLOR-EDIT))
                       0 0 "left" "top"
                       BACKGROUND)))

; lines->images : [NEList-of Line] Nat Nat PosInt String -> [List-of Image]
; turn a list of lines into a list of images containing the text
(check-expect (lines->images (list "Line of text" "") 1 0 FSIZE-EDIT COLOR-EDIT)
              (list (line->image "Line of text" FSIZE-EDIT COLOR-EDIT #f)
                    (char->image-cursor " " FSIZE-EDIT COLOR-EDIT)))
(define (lines->images lol lnum cnum fsize fcolor)
  (if (= lnum 0)
      (cons (line->image-cursor (first lol) cnum fsize fcolor)
            (map (lambda (l) (line->image l fsize fcolor #f)) (rest lol)))
      (cons (line->image (first lol) fsize fcolor #f)
            (lines->images (rest lol) (sub1 lnum) cnum fsize fcolor))))
        
; line->image : Line PosInt String Boolean -> Image
; the given text rendered as an image
(check-expect (line->image "Line of text" FSIZE-EDIT COLOR-EDIT #f)
              (text/font "Line of text" FSIZE-EDIT COLOR-EDIT "Monospace" "default" "normal"
                         "normal" #f))
(define (line->image line fsize fcolor underline?)
  (text/font line fsize fcolor "Monospace" "default" "normal" "normal" underline?))

; line->image-cursor : Line Nat PosInt String -> Image
; the given text rendered as an image in which the char at the given position is underlined
(check-expect (line->image-cursor "Line of text" 0 FSIZE-EDIT COLOR-EDIT)
              (beside (text/font "L"           FSIZE-EDIT COLOR-EDIT "Monospace" "default"
                                 "normal" "normal" #t)
                      (text/font "ine of text" FSIZE-EDIT COLOR-EDIT "Monospace" "default"
                                 "normal" "normal" #f)))
(define (line->image-cursor line cnum fsize fcolor)
  (if (< cnum (string-length line))
      (beside (line->image (substring line 0 cnum) fsize fcolor #f)
              (char->image-cursor (string-ith line cnum) fsize fcolor)
              (line->image (substring line (add1 cnum)) fsize fcolor #f))
      ; special case: cursor is post-end-of-line
      (beside (line->image line fsize fcolor #f)
              (char->image-cursor " " fsize fcolor))))

; char->image-cursor : 1String PosInt String -> Image
; the 1-character string rendered as an image of an underlined character
(check-expect (char->image-cursor "X" FSIZE-EDIT COLOR-EDIT)
              (text/font "X" FSIZE-EDIT COLOR-EDIT "Monospace" "default" "normal" "normal" #t))
(define (char->image-cursor c fsize fcolor)
  (text/font c fsize fcolor "Monospace" "default" "normal" "normal" #t))

; a string announcing the list of available commands
(define AVAIL-CMDS "Available commands:")

; a list of strings to be displayed in MENU mode
(define MENU-LIST
  (list AVAIL-CMDS
        ""
        "<ESC>: toggle Menu display"
        "<F1> : decrease text-font size"
        "<F2> : increase text-font size"))

; a list of text images, one per line of text describing the menu in MENU mode
(define MENU-IMAGES
  (map (lambda (l) (line->image l FSIZE-MENU COLOR-MENU (string=? l AVAIL-CMDS))) MENU-LIST))

; the entire menu text as a single image
(define MENU-IMAGE (stack-images-left-aligned MENU-IMAGES))

; draw-buffer-menu-mode : Buffer -> Image
; render a buffer in MENU mode: the menu
(check-expect (draw-buffer-menu-mode buffer-text-empty) (overlay MENU-IMAGE BACKGROUND))
(define (draw-buffer-menu-mode _)
  (overlay MENU-IMAGE BACKGROUND))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESPONDING TO KEY EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the following keys are ignored in EDIT mode
; IGNORE-KEYS-EDIT : -> [List-of Key]
(define IGNORE-KEYS-EDIT
  (list
   "\t"                  ; tab
   "\u007F"              ; delete key
   "shift" "rshift"      ; typing "shift-a" still results in an "A" being processed
   "control" "rcontrol"
   "menu"
   "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12"
   "numlock"
   "scroll"
   "wheel-up" "wheel-down" "wheel-left" "wheel-right")) ; "mouse" key events

; ignore-key? : Key [List-of Key] -> Boolean
; is the key to be ignored, i.e. a member of the given key list?
(check-expect (ignore-key? "menu" IGNORE-KEYS-EDIT) #t)
(check-expect (ignore-key? "A"    IGNORE-KEYS-EDIT) #f)
(define (ignore-key? key lok)
  (ormap (lambda (ik) (key=? ik key)) lok))

; process-key : Buffer Key -> Buffer
; respond to each key event
(check-expect (process-key buffer-text   "escape") buffer-menu)
(check-expect (process-key buffer-menu   "escape") buffer-text)
(check-expect (process-key buffer-search "f3") buffer-text)
(define (process-key buffer key)
  (cond [(string=? (buffer-mode buffer) "EDIT")   (process-key-text-mode   buffer key)]
        [(string=? (buffer-mode buffer) "MENU")   (process-key-menu-mode   buffer key)]
        [(string=? (buffer-mode buffer) "SEARCH") (process-key-search-mode buffer key)]))

; process-key-text-mode : Buffer Key -> Buffer
; respond to each key event in text mode
(check-expect (process-key-text-mode buffer-text "escape") buffer-menu)
(check-expect (process-key-text-mode buffer-text "f3") buffer-search)
(define (process-key-text-mode buffer key)
  (cond [(ignore-key? key IGNORE-KEYS-EDIT) buffer]
        [(key=? key "left")   (process-text-left      buffer)]
        [(key=? key "right")  (process-text-right     buffer)]
        [(key=? key "up")     (process-text-up        buffer)]
        [(key=? key "down")   (process-text-down      buffer)]
        [(key=? key "home")   (process-text-home      buffer)]
        [(key=? key "end")    (process-text-end       buffer)]
        [(key=? key "\r")     (process-text-return    buffer)]
        [(key=? key "\b")     (process-text-backspace buffer)]
        [(key=? key "f1")     (process-text-f1        buffer)] ; decrease font
        [(key=? key "f2")     (process-text-f2        buffer)] ; increase font
        [(key=? key "f3")     (process-text-f3        buffer)] ; ->  SEARCH mode (Exercise 3)
        [(key=? key "escape") (process-text-escape    buffer)] ; ->  MENU  mode
        [else                 (process-text-printable buffer key)]))



; process-text-left : Buffer -> Buffer
; process the "left" cursor key: move left, possibly up one line
(check-expect (process-text-left buffer-text) (make-buffer (list "Line 0" "Line 1") 1 2 FSIZE-EDIT
                                                           "EDIT"))
(check-expect (process-text-left buffer-text-empty) buffer-text-empty)
(define (process-text-left buffer)
  (local [(define lol   (buffer-lol   buffer))
          (define lnum  (buffer-lnum  buffer))
          (define cnum  (buffer-cnum  buffer))
          (define fsize (buffer-fsize buffer))
          (define mode  (buffer-mode  buffer))]
    (cond [(start-of-buffer? buffer) buffer]
          [(= cnum 0)                (make-buffer lol (sub1 lnum) (end-of-line buffer (sub1 lnum))
                                                  fsize mode)]
          [else                      (make-buffer lol lnum (sub1 cnum) fsize mode)])))

; process-text-right : Buffer -> Buffer
; process the "right" cursor key: move right, possibly down one line
(check-expect (process-text-right buffer-text) (make-buffer (list "Line 0" "Line 1") 1 4 FSIZE-EDIT
                                                            "EDIT"))
(check-expect (process-text-right buffer-text-empty) buffer-text-empty)
(define (process-text-right buffer)
  (local [(define lol   (buffer-lol   buffer))
          (define lnum  (buffer-lnum  buffer))
          (define cnum  (buffer-cnum  buffer))
          (define fsize (buffer-fsize buffer))
          (define mode  (buffer-mode  buffer))]
    (cond [(end-of-buffer? buffer) buffer]
          [(end-of-line?   buffer) (make-buffer lol (add1 lnum) 0 fsize mode)]
          [else                    (make-buffer lol lnum (add1 cnum) fsize mode)])))

; process-text-up : Buffer -> Buffer
; process the "up" cursor key
(check-expect (process-text-up buffer-text-empty) buffer-text-empty)
(check-expect (process-text-up buffer-text) (make-buffer (list "Line 0" "Line 1") 0 3 FSIZE-EDIT
                                                         "EDIT"))
(define (process-text-up buffer)
  (local [(define lol   (buffer-lol   buffer))
          (define lnum  (buffer-lnum  buffer))
          (define cnum  (buffer-cnum  buffer))
          (define fsize (buffer-fsize buffer))
          (define mode  (buffer-mode  buffer))
          (define prev-lnum (sub1 lnum))]
    (if (= lnum 0)
        buffer
        (make-buffer lol
                     prev-lnum
                     (min cnum (end-of-line buffer prev-lnum))
                     fsize
                     mode))))

; process-text-down : Buffer -> Buffer
; process the "down" cursor key
(check-expect (process-text-down buffer-text-empty) buffer-text-empty)
(check-expect (process-text-down (make-buffer (list "Line 0" "Line 1") 0 3 FSIZE-EDIT "EDIT"))
              buffer-text)
(define (process-text-down buffer)
  (local [(define lol   (buffer-lol   buffer))
          (define lnum  (buffer-lnum  buffer))
          (define cnum  (buffer-cnum  buffer))
          (define fsize (buffer-fsize buffer))
          (define mode  (buffer-mode  buffer))
          (define next-lnum (add1 lnum))]
    (if (= lnum (sub1 (length lol)))
        buffer
        (make-buffer lol
                     next-lnum
                     (min cnum (end-of-line buffer next-lnum))
                     fsize
                     mode))))

; process-text-home : Buffer -> Buffer
; process the "home" key
(check-expect (process-text-home buffer-text-empty) buffer-text-empty)
(check-expect (process-text-home buffer-text) (make-buffer (list "Line 0" "Line 1") 1 0 FSIZE-EDIT
                                                           "EDIT"))
(define (process-text-home buffer)
  (local [(define lol      (buffer-lol buffer))
          (define lnum     (buffer-lnum buffer))
          (define cnum     (buffer-cnum buffer))
          (define new-lnum (if (= cnum 0) 0 lnum))
          (define new-cnum 0)]
    (make-buffer lol
                 new-lnum
                 new-cnum
                 (buffer-fsize buffer)
                 (buffer-mode  buffer))))

; process-text-end : Buffer -> Buffer
; process the "end" key
(check-expect (process-text-end buffer-text-empty) buffer-text-empty)
(check-expect (process-text-end buffer-text) (make-buffer (list "Line 0" "Line 1") 1 6 FSIZE-EDIT
                                                          "EDIT"))
(define (process-text-end buffer)
  (local [(define lol  (buffer-lol  buffer))
          (define lnum (buffer-lnum buffer))
          (define cnum (buffer-cnum buffer))
          (define new-lnum (if (end-of-line? buffer) (sub1 (length lol)) lnum))
          (define new-cnum (end-of-line buffer new-lnum))]
    (make-buffer lol
                 new-lnum
                 new-cnum
                 (buffer-fsize buffer)
                 (buffer-mode  buffer))))

; process-return : Buffer -> Buffer
; process the "return" key, as follows:
; 1. replace current line by the current line up to and excluding the current position
; 2. insert a new line after the current one,
;    consisting of the current line from the current position
; 3. lnum++, cnum = 0
(check-expect (process-text-return buffer-text)
              (make-buffer (list "Line 0" "Lin" "e 1") 2 0 FSIZE-EDIT "EDIT"))
(define (process-text-return buffer)
  (local [(define lol   (buffer-lol   buffer))
          (define lnum  (buffer-lnum  buffer))
          (define cnum  (buffer-cnum  buffer))
          (define fsize (buffer-fsize buffer))
          (define mode  (buffer-mode  buffer))
          (define line (list-ref lol lnum))
          (define new-line-1 (substring line 0 cnum))
          (define new-line-2 (substring line cnum))
          (define new-lol (subst-two new-line-1 new-line-2 lnum lol))]
    (make-buffer new-lol (add1 lnum) 0 fsize mode)))

; process-text-backspace : Buffer -> Buffer
; process the "backspace" key, as follows:
; 1. if at the start of the buffer, do nothing.
; 2. if column > 0: just delete the current character and move left.
; 3. if column = 0: replace the lines at positions lnum-1 and lnum by their concatenation
;    Note that case 3 precisely reverses the effect of the <RETURN> key
(check-expect (process-text-backspace (make-buffer (list "Line 0" "Lin" "e 1") 2 0 FSIZE-EDIT "EDIT"))
              buffer-text)
(check-expect (process-text-backspace buffer-text) (make-buffer (list "Line 0" "Lie 1") 1 2
                                                                FSIZE-EDIT "EDIT"))
(define (process-text-backspace buffer)
  (local [(define lol   (buffer-lol   buffer))
          (define lnum  (buffer-lnum  buffer))
          (define cnum  (buffer-cnum  buffer))
          (define fsize (buffer-fsize buffer))
          (define mode  (buffer-mode  buffer))
          (define line  (list-ref lol lnum))]
    (cond [(start-of-buffer? buffer) buffer]
          [(> cnum 0) (make-buffer
                       (subst (char-delete (sub1 cnum) line) lnum lol)
                       lnum (sub1 cnum) fsize mode)]
          [(= cnum 0) (make-buffer
                       (subst-for-two (string-append (list-ref lol (sub1 lnum))
                                                     (list-ref lol lnum))
                                      (sub1 lnum)
                                      lol)
                       (sub1 lnum)
                       (end-of-line buffer (sub1 lnum))
                       fsize mode)])))


; process-text-printable : Buffer Key -> Buffer
; process the key event corresponding to a printable key: just insert it
(check-expect (process-text-printable buffer-text       "!") (make-buffer (list "Line 0" "Lin!e 1")
                                                                          1 4 FSIZE-EDIT "EDIT"))
(check-expect (process-text-printable buffer-text-empty "!") (make-buffer (list "!") 0 1 FSIZE-EDIT
                                                                          "EDIT"))
(define (process-text-printable buffer key)
  (local [(define lol   (buffer-lol   buffer))
          (define lnum  (buffer-lnum  buffer))
          (define cnum  (buffer-cnum  buffer))
          (define fsize (buffer-fsize buffer))
          (define mode  (buffer-mode  buffer))
          (define line  (list-ref lol lnum))]
    (make-buffer (subst (char-insert key cnum line) lnum lol)
                 lnum (add1 cnum) fsize mode)))

; process-text-f1 : Buffer -> Buffer
; reduce the text-font size
(check-expect (process-text-f1 buffer-text-empty) (make-buffer (list "")                0 0
                                                               (sub1 FSIZE-EDIT) "EDIT"))
(check-expect (process-text-f1 buffer-text)       (make-buffer (list "Line 0" "Line 1") 1 3
                                                               (sub1 FSIZE-EDIT) "EDIT"))
(define (process-text-f1 buffer)
  (local [(define fsize (buffer-fsize buffer))]
    (make-buffer (buffer-lol  buffer)
                 (buffer-lnum buffer)
                 (buffer-cnum buffer)
                 (max (sub1 fsize) 1)
                 (buffer-mode buffer))))

; process-text-f2 : Buffer -> Buffer
; increase the text-font size
(check-expect (process-text-f2 buffer-text-empty) (make-buffer (list "")                0 0
                                                               (add1 FSIZE-EDIT) "EDIT"))
(check-expect (process-text-f2 buffer-text)       (make-buffer (list "Line 0" "Line 1") 1 3
                                                               (add1 FSIZE-EDIT) "EDIT"))
(define (process-text-f2 buffer)
  (local [(define fsize (buffer-fsize buffer))]
    (make-buffer (buffer-lol  buffer)
                 (buffer-lnum buffer)
                 (buffer-cnum buffer)
                 (add1 fsize)
                 (buffer-mode buffer))))

; process-text-escape : Buffer -> Buffer
; switch from EDIT into MENU mode
(check-expect (process-text-escape buffer-text) buffer-menu)
(define (process-text-escape buffer)
  (make-buffer (buffer-lol   buffer)
               (buffer-lnum  buffer)
               (buffer-cnum  buffer)
               (buffer-fsize buffer)
               "MENU"))

; process-key-menu-mode : Buffer Key -> Buffer
; respond to each key event in MENU mode
(check-expect (process-key-menu-mode buffer-menu "escape") buffer-text)
(define (process-key-menu-mode buffer key)
  (if (key=? key "escape")
      (make-buffer (buffer-lol   buffer)
                   (buffer-lnum  buffer)
                   (buffer-cnum  buffer)
                   (buffer-fsize buffer)
                   "EDIT")
      buffer))

; process-text-f3 : Buffer -> Buffer
; switch from EDIT into SEARCH mode
(check-expect (process-text-f3 buffer-text) buffer-search)
(define (process-text-f3 buffer)
  (make-buffer (cons "" (buffer-lol   buffer))
               (buffer-lnum  buffer)
               (buffer-cnum  buffer)
               (buffer-fsize buffer)
               "SEARCH"))

; the following keys are ignored in SEARCH mode
; IGNORE-KEYS-SEARCH : -> [List-of Key]
(define IGNORE-KEYS-SEARCH
  (list
   "\t"                  ; tab
   "\u007F"              ; delete key
   "shift" "rshift"      ; typing "shift-a" still results in an "A" being processed
   "control" "rcontrol"
   "menu"
   "f1" "f2" "f4" "f5" "f6" "f7" "f8" "f9" "f10" "f11" "f12"
   "numlock"
   "scroll"
   "left" "right" "up" "down" "home" "end" "escape" ; ignore these keys when in search
   "wheel-up" "wheel-down" "wheel-left" "wheel-right")) ; "mouse" key events



; process-key-search-mode : Buffer Key -> Buffer
; respond to each key event in SEARCH mode
(check-expect (process-key-search-mode buffer-search "f1") buffer-search)
(check-expect (process-key-search-mode buffer-search "\r")
              (make-buffer (cons (string-append (first (buffer-lol buffer-search)) "\n")
                                 (rest (buffer-lol buffer-search)))
                           (buffer-lnum buffer-search)
                           (buffer-cnum buffer-search)
                           (buffer-fsize buffer-search)
                           (buffer-mode  buffer-search)))
(check-expect (process-key-search-mode buffer-search "\b") buffer-search)
(check-expect (process-key-search-mode buffer-search "f3") buffer-text)
(check-expect (process-key-search-mode buffer-search "a")
              (make-buffer (cons (string-append (first (buffer-lol buffer-search)) "a")
                                 (rest (buffer-lol buffer-search)))
                           (buffer-lnum buffer-search)
                           (buffer-cnum buffer-search)
                           (buffer-fsize buffer-search)
                           (buffer-mode  buffer-search)))
(define (process-key-search-mode buffer key)
  (cond [(ignore-key? key IGNORE-KEYS-SEARCH) buffer]
        [(key=? key "\r")     (process-text-return-search    buffer)]
        [(key=? key "\b")     (process-text-backspace-search buffer)]
        [(key=? key "f3")     (process-text-f3-search        buffer)]
        [else                 (process-text-printable-search buffer key)]))


; process-text-f3-search : Buffer -> Buffer
; switch from SEARCH into EDIT mode
(check-expect (process-text-f3-search buffer-search) buffer-text)
(check-expect (process-text-f3-search (make-buffer (cons "Line 1" (rest (buffer-lol buffer-search)))
                                                   0
                                                   0
                                                   (buffer-fsize buffer-search)
                                                   (buffer-mode  buffer-search)))
              (make-buffer (rest (buffer-lol buffer-search))
                           1
                           0
                           (buffer-fsize buffer-search)
                           (buffer-mode  buffer-text)))
(check-expect (process-text-f3-search (make-buffer (cons "Line 3" (rest (buffer-lol buffer-search)))
                                                   1
                                                   2
                                                   (buffer-fsize buffer-search)
                                                   (buffer-mode  buffer-search)))
              (make-buffer (rest (buffer-lol buffer-search))
                           1
                           2
                           (buffer-fsize buffer-search)
                           (buffer-mode  buffer-text)))  
                                                   
(define (process-text-f3-search buffer)
  (local [(define first-lol (first (buffer-lol buffer)))
          (define whole-text (lol->text (rest (buffer-lol buffer))))
          (define cursor-pos (lnum-cnum->pos (rest (buffer-lol buffer))
                                             (buffer-lnum buffer)
                                             (buffer-cnum buffer)))
          (define cursor-search-pos (string-search first-lol whole-text cursor-pos))]
    (if (> cursor-search-pos (string-length whole-text))
        (make-buffer (rest (buffer-lol buffer))
                     (buffer-lnum  buffer)
                     (buffer-cnum  buffer)
                     (buffer-fsize buffer)
                     "EDIT")
        (make-buffer (rest (buffer-lol buffer))
                     (posn-x (pos->lnum-cnum (rest (buffer-lol buffer)) cursor-search-pos)) 
                     (posn-y (pos->lnum-cnum (rest (buffer-lol buffer)) cursor-search-pos)) 
                     (buffer-fsize buffer)
                     "EDIT"))))

; lnum-cnum->pos : [NEList-of Line] Nat Nat -> Nat
; takes the row and column position of a [NEList-of Line] and returns
; the position of the cursor when the list is combined into Text
(check-expect (lnum-cnum->pos (list "Line 0" "Line 1") 1 3) 10)
(check-expect (lnum-cnum->pos (list "Line 0" "Line 1") 0 4) 4)
(check-expect (lnum-cnum->pos (list "Line 0" "Line 1") 1 6) 13)
(check-expect (lnum-cnum->pos (list "Line 0" "Line 1") 1 0) 7)
(define (lnum-cnum->pos lol r c)
  (cond [(= r 0) c]
        [(> r 0) (+ (add1 (string-length (first lol)))
                    (lnum-cnum->pos (rest lol) (sub1 r) c))]))

; pos->lnum-cnum : [NEList-of Line] Nat -> Posn
; takes the position of cursor in a text and the non-empty list of line and returns
; the row and column position in a Posn
(check-expect (pos->lnum-cnum (list "Line 0" "Line 1") 10) (make-posn 1 3))
(check-expect (pos->lnum-cnum (list "Line 0" "Line 1") 4) (make-posn 0 4))
(check-expect (pos->lnum-cnum (list "Line 0" "Line 1") 13) (make-posn 1 6))
(check-expect (pos->lnum-cnum (list "Line 0" "Line 1") 7) (make-posn 1 0))
(define (pos->lnum-cnum lol p)
  (cond
    [(or (empty? lol) (< (sub1 p) (string-length (first lol)))) (make-posn 0 p)]
    [else
     (local [(define recursion (pos->lnum-cnum (rest lol) (- p 1 (string-length (first lol)))))]
       (make-posn (add1 (posn-x recursion)) (posn-y recursion)))]))

; process-text-return-search : Buffer -> Buffer
; respond to the return key in SEARCH mode
(check-expect (process-text-return-search buffer-search)
              (make-buffer (cons (string-append (first (buffer-lol buffer-search)) "\n")
                                 (rest (buffer-lol buffer-search)))
                           (buffer-lnum buffer-search)
                           (buffer-cnum buffer-search)
                           (buffer-fsize buffer-search)
                           (buffer-mode  buffer-search)))
(define (process-text-return-search buffer)
  (local [(define first-lol (first (buffer-lol buffer)))
          (define rest-lol  (rest (buffer-lol buffer)))]
    (make-buffer (cons (string-append first-lol "\n") rest-lol)
                 (buffer-lnum  buffer)
                 (buffer-cnum  buffer)
                 (buffer-fsize buffer)
                 "SEARCH")))

; process-text-backspace-search : Buffer -> Buffer
; respond to the backspace key in SEARCH mode
(check-expect (process-text-backspace-search buffer-search) buffer-search)
(check-expect (process-text-backspace-search
               (make-buffer (cons "a" (rest (buffer-lol buffer-search)))
                            (buffer-lnum buffer-search)
                            (buffer-cnum buffer-search)
                            (buffer-fsize buffer-search)
                            (buffer-mode  buffer-search)))
              buffer-search)                            
(define (process-text-backspace-search buffer)
  (local [(define first-lol (first (buffer-lol buffer)))
          (define rest-lol  (rest (buffer-lol buffer)))]
    (cond [(string=? first-lol "") buffer]
          [else
           (make-buffer (cons (substring first-lol 0 (sub1 (string-length first-lol))) rest-lol)
                        (buffer-lnum  buffer)
                        (buffer-cnum  buffer)
                        (buffer-fsize buffer)
                        "SEARCH")])))

; process-text-printable-search : Buffer Key -> Buffer
; respond to the printable keys in SEARCH mode
(check-expect (process-text-printable-search buffer-search "b")
              (make-buffer (cons (string-append (first (buffer-lol buffer-search)) "b")
                                 (rest (buffer-lol buffer-search)))
                           (buffer-lnum buffer-search)
                           (buffer-cnum buffer-search)
                           (buffer-fsize buffer-search)
                           (buffer-mode  buffer-search)))
(define (process-text-printable-search buffer key)
  (local [(define first-lol (first (buffer-lol buffer)))
          (define rest-lol  (rest (buffer-lol buffer)))]
    (make-buffer (cons (string-append first-lol key) rest-lol)
                 (buffer-lnum  buffer)
                 (buffer-cnum  buffer)
                 (buffer-fsize buffer)
                 "SEARCH")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Exercise 5

;; string-search : String String Nat -> Nat
;; takes two string s and t and a position and finds the position of the first occurence of s in t
;; if not found, return length of t + 1
(check-expect (string-search "pattern" "pattern string test"  0)  0) ; an easy search
(check-expect (string-search "pattern" "pattern string test"  1) 20) ; not found at pos. >= 1
(check-expect (string-search "pattern" "pattern string test" 30) 20) ; bad start position: 30
(check-expect (string-search "string"  "pattern string test"  0)  8) ; found at pos. 8
(check-expect (string-search "string"  "pattern string test"  8)  8) ; same answer as previous!
(check-expect (string-search "string"  "pattern string test"  9) 20) ; not found at pos. >= 9
(check-expect (string-search ""        "pattern string test"  0)  0) ; "" always matches
(check-expect (string-search "paddern" "pattern string test"  0) 20) ; note "...dd...": not found
(define (string-search s t p)
  (cond [(> p (string-length t)) (add1 (string-length t))]
        [(not (string-contains? s (substring t p))) (add1 (string-length t))]
        [else (if (string=? s (substring t p (+ p (string-length s))))
                  p
                  (string-search s t (add1 p)))]))


;; Exercise 7

; a list of strings to be displayed in SEARCH mode
(define SEARCH-LIST
  (list "Search Instructions:"
        ""
        "<F3> : search for entered string"
        "Ignores all command keys except for \b and \r"
        "Enter the search string below:"))

; a list of text images, one per line of text describing the menu in SEARCH mode
(define SEARCH-IMAGES
  (map (lambda (l) (line->image l FSIZE-SEARCH COLOR-SEARCH (string=? l "Search Instructions:")))
       SEARCH-LIST))

; the entire SEARCH text as a single image
(define SEARCH-IMAGE (stack-images-left-aligned SEARCH-IMAGES))

; draw-buffer-search-mode : Buffer -> Image
; render a buffer in search mode
(check-expect (draw-buffer-search-mode (make-buffer (cons "Jin" (rest (buffer-lol buffer-search)))
                                                    (buffer-lnum buffer-search)
                                                    (buffer-cnum buffer-search)
                                                    (buffer-fsize buffer-search)
                                                    (buffer-mode  buffer-search)))
              (overlay
               (above/align "left" SEARCH-IMAGE (line->image "Jin" FSIZE-SEARCH COLOR-SEARCH #f))
               BACKGROUND))
(check-expect (draw-buffer-search-mode buffer-search)
              (overlay
               (above/align "left" SEARCH-IMAGE (line->image "" FSIZE-SEARCH COLOR-SEARCH #f))
               BACKGROUND))
(define (draw-buffer-search-mode buffer)
  (local [(define search-term
            (line->image (first (buffer-lol buffer)) FSIZE-SEARCH COLOR-SEARCH #f))]
    (overlay (above/align "left" SEARCH-IMAGE search-term) BACKGROUND)))

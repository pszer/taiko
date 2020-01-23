(declare (unit FONT))
(declare (uses TEXTURE))

; A glyph font is an 128 long vector where an ASCII
; character where the integer value of the character
; is its index in the vector.

(import (prefix sdl2 sdl2:))

; Takes in a list of pairs, where each pair
; is an ASCII character and a texture-key.
(define (make-glyph-font chars)
  (let ((font (make-vector 128 #f)))
	(define (iter chars)
	  (if (null? chars)
		  font
		  (let ((index (caar chars))
				(key   (cdar chars)))
			(vector-set! font (char->integer index) key)
			(iter (cdr chars)))))
	(iter chars)))

(define (glyph-font-texture font char)
  (if font
  	(lookup-texture (vector-ref font (char->integer char)))
	#f))

; returns the width of text rendered in a specific font
(define (width-glyph-font-text font text)
  (let ((len (string-length text))) 
	(define (iter i total)
	  (unless (= i len)
		(let ((char-tex (glyph-font-texture (string-ref text i))))
		  (if char-tex
			  (let-values (((_ _ w _) (sdl2:query-texture char-tex)))	
				(iter (+ i 1) (+ total w)))
			  (iter (+ i 1) total)))))
	(iter 0 0)))

(define (init-glyph-fonts)
  (set! score-glyph-font (make-glyph-font (list (cons #\0 (core-texs-index "score-0.png"))
												(cons #\1 (core-texs-index "score-1.png"))
												(cons #\2 (core-texs-index "score-2.png"))
												(cons #\3 (core-texs-index "score-3.png"))
												(cons #\4 (core-texs-index "score-4.png"))
												(cons #\5 (core-texs-index "score-5.png"))
												(cons #\6 (core-texs-index "score-6.png"))
												(cons #\7 (core-texs-index "score-7.png"))
												(cons #\8 (core-texs-index "score-8.png"))
												(cons #\9 (core-texs-index "score-9.png"))
												(cons #\, (core-texs-index "score-comma.png"))
												(cons #\. (core-texs-index "score-dot.png"))
												(cons #\% (core-texs-index "score-percent.png"))
												(cons #\x (core-texs-index "score-x.png"))))))
(define score-glyph-font #f)

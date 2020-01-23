(declare (unit RENDERER))
(declare (uses TEXTURE))
(declare (uses FONT))

(import (prefix sdl2 sdl2:))

(define renderer #f)

(define (clear-screen r g b)
  (sdl2:render-draw-colour-set! renderer
								(sdl2:make-colour r g b))
  (sdl2:render-clear! renderer))

(define (render-texture-simple! texture-key srcrect destrect)
  (let ((texture (lookup-texture texture-key)))
	(when texture
	  (sdl2:render-copy! renderer texture srcrect destrect))))
(define (render-texture! texture-key srcrect destrect angle center flip)
  (let ((texture (lookup-texture texture-key)))
	(when texture
	  (sdl2:render-copy-ex! renderer texture srcrect destrect angle center flip))))

(define (number->integer num)
  (inexact->exact (floor num)))

(define (render-texture-centered! texture-key srcrect destrect)
  (if (not destrect)
	  (render-texture-simple! texture-key srcrect destrect)
	  (let ((texture (lookup-texture texture-key))
			(new-x (- (sdl2:rect-x destrect) (/ (sdl2:rect-w destrect) 2)))
			(new-y (- (sdl2:rect-y destrect) (/ (sdl2:rect-h destrect) 2)))
			(w (sdl2:rect-w destrect))
			(h (sdl2:rect-h destrect)))
		(when texture
		  (sdl2:render-copy! renderer texture srcrect
							 (sdl2:make-rect (number->integer new-x)
											 (number->integer new-y)
											 w h))))))

(define (render-texture-preserve-aspect-horiz! texture-key srcrect destrect)
  (define (get-new-y scale y h tex-h)
	(number->integer (- (+ y
						   (/ h 2))
						(/ (* scale tex-h) 2))))
  (let ((texture (lookup-texture texture-key)))
	(when texture
	  (let-values (((_ _ w h) (sdl2:query-texture texture)))
		(let ((dest-scale (/ (sdl2:rect-w destrect) w)))
		  (sdl2:render-copy! renderer
							 texture
							 srcrect
							 (sdl2:make-rect (sdl2:rect-x destrect)
											 (get-new-y dest-scale
														(sdl2:rect-y destrect)
														(sdl2:rect-h destrect)
														h)
										     (sdl2:rect-w destrect)
										     (number->integer (* dest-scale h)))))))))

; hor-align = 'left 'center 'right
; ver-align = 'top 'center 'bottom
(define (render-texture-align! texture-key srcrect destrect hor-align ver-align flip)
  (let ((texture (lookup-texture texture-key)))
	(when texture
	  (let ((x (sdl2:rect-x destrect))
			(y (sdl2:rect-y destrect))
			(w (sdl2:rect-w destrect))
			(h (sdl2:rect-h destrect)))
		(case hor-align
		  ((center) (set! x (- x (quotient w 2))))
		  ((right)  (set! x (- x w))))
		(case ver-align
		  ((center) (set! y (- y (quotient h 2))))
		  ((bottom) (set! y (- y h))))
		(sdl2:render-copy-ex! renderer texture srcrect (sdl2:make-rect x y w h)
							  0.0 #f flip)))))

; Renders a repeating bar |||||||
(define (render-bar! texture-key destrect)
  (let ((texture (lookup-texture texture-key)))
	(define (iter! rect srcrect x-end scale)
	  (unless (>= (sdl2:rect-x rect) x-end)
		(let ((diff (- x-end (sdl2:rect-x rect))))
		  (when (< diff (sdl2:rect-w rect))
			  (sdl2:rect-w-set! srcrect (number->integer (* scale diff))))
		  (sdl2:render-copy! renderer texture srcrect rect)
		  (sdl2:rect-x-set! rect (number->integer (+ (sdl2:rect-x rect) (sdl2:rect-w rect))))
		  (iter! rect srcrect x-end scale))))
	(when texture
	  (let-values (((_ _ w h) (sdl2:query-texture texture)))
		(let ((scale (/ (sdl2:rect-h destrect) h)))
		  (iter! (sdl2:make-rect (sdl2:rect-x destrect)
								 (sdl2:rect-y destrect)
								 (number->integer (* scale w))
								 (number->integer (* scale h)))
				 (sdl2:make-rect 0 0 w h)
				 (+ (sdl2:rect-x destrect) (sdl2:rect-w destrect))
				 scale))))))

; align is either 'left 'center 'right
; vert-align is either 'top 'center 'bottom
(define (render-glyph-font! font text x y align vert-align)
  (define (create-char-rect x y w h align vert-align)
	(define (get-y)
	  (case vert-align
		((top) y)
		((center) (- y (quotient h 2)))
		((bottom) (- y h))
		(else y)))
	(case align
	  ((left center) (sdl2:make-rect x (get-y) w h))
	  ((right)       (sdl2:make-rect (- x w) (get-y) w h))))
  (let ((len (string-length text))) 
	(define (iter i x)
	  (unless (= i len)
		(let ((char-tex (glyph-font-texture font (string-ref text i))))
		  (if char-tex
			  (let-values (((_ _ w h) (sdl2:query-texture char-tex)))
				(sdl2:render-copy! renderer char-tex #f (create-char-rect x y w h align vert-align))
				(if (eq? align 'right)
					(iter (+ i 1) (- x w))
					(iter (+ i 1) (+ x w))))
			  (iter (+ i 1) x)))))
	(if (eq? align 'center)
		(iter 0 (- x (number->integer (/ (width-glyph-font-text font text) 2))))
		(iter 0 x))))

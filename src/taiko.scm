(declare (unit TAIKO))
(declare (uses RENDERER))
(declare (uses INPUT))

(import (prefix sdl2 sdl2:)
		srfi-13)

; Details for how to render the playfield
; (e.g. bar position, drum position and size etc.
(define-record playfield
  bar          ; sdl2:rect
  drum         ; sdl2:rect
  drum-center  ; sdl2:point offset from drum topleft corner
  drum-overlay-size ; sdl2:point for drum hit overlay size
  pad-center   ; sdl2:point offset from bar topleft corner

  bg-texture   ; background texture key (#f if no background)
  bar-texture  ; bar texture key
  drum-texture ; drum texture key
  inner-drum-texture ; don inner drum hit overlay
  outer-drum-texture ; katsu outer drum hut overlay

  score-pos    ; sdl2:point score offset
  score-font   ; glyph-font
  score-align  ; 'left 'center 'right
)

(define +TAIKO-DEFAULT-PLAYFIELD+ 'undefined)
(define +TAIKO-DRUMHIT-OVERLAY-TIME+ 133)

(define taiko-last-left-katsu-hit 0)
(define taiko-last-right-katsu-hit 0)
(define taiko-last-left-don-hit 0)
(define taiko-last-right-don-hit 0)

(define taiko-playfield #f)

(define (init-taiko-playfield)
  (set! taiko-playfield (make-playfield (sdl2:make-rect 181 140 0 200) ; bar
										(sdl2:make-rect 0 140 181 200)  ; drum
										(sdl2:make-point 85 98)
										(sdl2:make-point 73 125)
										(sdl2:make-point 32 32)
										#f #f #f #f #f
										(sdl2:make-point -4 4)
										score-glyph-font
										'right))
  (set! +TAIKO-DEFAULT-PLAYFIELD+ (core-texs-index "playfield.png"))
  (playfield-bg-texture-set!   taiko-playfield +TAIKO-DEFAULT-PLAYFIELD+)
  (playfield-bar-texture-set!  taiko-playfield (core-texs-index "bar.png"))
  (playfield-drum-texture-set! taiko-playfield (core-texs-index "drum.png"))
  (playfield-inner-drum-texture-set! taiko-playfield (core-texs-index "druminner.png"))
  (playfield-outer-drum-texture-set! taiko-playfield (core-texs-index "drumouter.png"))
  (update-taiko-playfield))
(define (update-taiko-playfield)
  (sdl2:rect-w-set! (playfield-bar taiko-playfield)
					(- window-width (sdl2:rect-x (playfield-bar taiko-playfield)))))

(define taiko-score 0)
(define (taiko-score->string)
  (string-pad (number->string taiko-score) 10 #\0))

(define (taiko-init)
  (init-taiko-playfield))

(define (taiko-render-playfield info)
  (taiko-render-background info)
  (taiko-render-bar info)
  (taiko-render-drum info)
  (taiko-render-score info))
(define (taiko-render-background info)
  (let ((texture (playfield-bg-texture info)))
	(when texture
	  (render-texture-preserve-aspect-horiz! texture
											 #f
											 (sdl2:make-rect 0
															 0
															 window-width
															 window-height)))))
(define (taiko-render-bar info)
  (let ((texture (playfield-bar-texture info)))
	(when texture
	  (render-bar! texture (playfield-bar info)))))
(define (taiko-render-drum info)
  (define (taiko-render-hit-overlay time texture-key rect align flip)
	(when (< (- (sdl2:get-ticks) time) +TAIKO-DRUMHIT-OVERLAY-TIME+)
	  (render-texture-align! texture-key #f rect align 'center flip)))
  (let ((texture (playfield-drum-texture info)))
	(when texture
	  (render-texture-simple! texture #f (playfield-drum info))))
  (let ((rect (sdl2:make-rect (+ (sdl2:point-x (playfield-drum-center info))
								 (sdl2:rect-x  (playfield-drum info)))
							  (+ (sdl2:point-y (playfield-drum-center info))
								 (sdl2:rect-y  (playfield-drum info)))
							  (sdl2:point-x (playfield-drum-overlay-size info))
							  (sdl2:point-y (playfield-drum-overlay-size info)))))
	(taiko-render-hit-overlay taiko-last-right-katsu-hit (playfield-outer-drum-texture info) rect 'left '())
	(taiko-render-hit-overlay taiko-last-left-katsu-hit (playfield-outer-drum-texture info) rect 'right '(horizontal))
	(taiko-render-hit-overlay taiko-last-right-don-hit (playfield-inner-drum-texture info) rect 'left '(horizontal))
	(taiko-render-hit-overlay taiko-last-left-don-hit (playfield-inner-drum-texture info) rect 'right '())))
(define (taiko-render-score info)
  (let ((pos (playfield-score-pos info))
		(align (playfield-score-align info)))
	(render-glyph-font! (playfield-score-font info)
						(taiko-score->string)
						(+ window-width (sdl2:point-x pos))
						(sdl2:point-y pos)
						align
						'top)))


(define (taiko-left-don-hit)
  (set! taiko-last-left-don-hit (sdl2:get-ticks))
  (taiko-don-hit))
(define (taiko-left-katsu-hit)
  (set! taiko-last-left-katsu-hit (sdl2:get-ticks))
  (taiko-katsu-hit))
(define (taiko-right-don-hit)
  (set! taiko-last-right-don-hit (sdl2:get-ticks))
  (taiko-don-hit))
(define (taiko-right-katsu-hit)
  (set! taiko-last-right-katsu-hit (sdl2:get-ticks))
  (taiko-katsu-hit))

(define (taiko-don-hit)
  #f)
(define (taiko-katsu-hit)
  #f)

(define (taiko-check-inputs)
  (when (eq? 'down (keypresses-state bind-left-outer-hit))
	(taiko-left-katsu-hit))
  (when (eq? 'down (keypresses-state bind-right-outer-hit))
	(taiko-right-katsu-hit))
  (when (eq? 'down (keypresses-state bind-left-inner-hit))
	(taiko-left-don-hit))
  (when (eq? 'down (keypresses-state bind-right-inner-hit))
	(taiko-right-don-hit)))
(define (taiko-update)
  (taiko-check-inputs))
(define (taiko-render)
  (update-taiko-playfield)
  (taiko-render-playfield taiko-playfield))

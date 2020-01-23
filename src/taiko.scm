(declare (unit TAIKO))
(declare (uses RENDERER))
(declare (uses INPUT))
(declare (uses BEATMAP))

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
  pad-size     ;

  bg-texture   ; background texture key (#f if no background)
  bar-texture  ; bar texture key
  drum-texture ; drum texture key
  pad-texture
  inner-drum-texture ; don inner drum hit overlay
  outer-drum-texture ; katsu outer drum hut overlay
  don-texture  ; don hit texture
  katsu-texture  ; katsu hit texture
  note-overlay-texture1 ; hit overlay texture
  note-overlay-texture2 ; hit overlay texture
  big-note-overlay-texture1 ; hit overlay texture
  big-note-overlay-texture2 ; hit overlay texture

  score-pos    ; sdl2:point score offset
  score-font   ; glyph-font
  score-align  ; 'left 'center 'right
)

(define +TAIKO-DEFAULT-PLAYFIELD+ 'undefined)
(define +TAIKO-DRUMHIT-OVERLAY-TIME+ 133)

(define +TAIKO-NOTE-WIDTH+ 128)
(define +TAIKO-NOTE-HEIGHT+ 128)
(define +TAIKO-BIG-NOTE-WIDTH+ 160)
(define +TAIKO-BIG-NOTE-HEIGHT+ 160)

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
										(sdl2:make-point 80 100)
										(sdl2:make-point 160 160)
										#f #f #f #f #f #f #f #f #f #f #f #f
										(sdl2:make-point -4 4)
										score-glyph-font
										'right))
  (set! +TAIKO-DEFAULT-PLAYFIELD+ (core-texs-index "playfield.png"))
  (playfield-bg-texture-set!   taiko-playfield +TAIKO-DEFAULT-PLAYFIELD+)
  (playfield-bar-texture-set!  taiko-playfield (core-texs-index "bar.png"))
  (playfield-drum-texture-set! taiko-playfield (core-texs-index "drum.png"))
  (playfield-pad-texture-set!  taiko-playfield (core-texs-index "pad.png"))
  (playfield-inner-drum-texture-set! taiko-playfield (core-texs-index "druminner.png"))
  (playfield-outer-drum-texture-set! taiko-playfield (core-texs-index "drumouter.png"))
  (playfield-don-texture-set! taiko-playfield (core-texs-index "don.png"))
  (playfield-katsu-texture-set! taiko-playfield (core-texs-index "katsu.png"))
  (playfield-note-overlay-texture1-set! taiko-playfield (core-texs-index "overlay0.png"))
  (playfield-note-overlay-texture2-set! taiko-playfield (core-texs-index "overlay1.png"))
  (playfield-big-note-overlay-texture1-set! taiko-playfield (core-texs-index "bigoverlay1.png"))
  (playfield-big-note-overlay-texture2-set! taiko-playfield (core-texs-index "bigoverlay2.png"))
  (update-taiko-playfield))

(define (update-taiko-playfield)
  (sdl2:rect-w-set! (playfield-bar taiko-playfield)
					(- window-width (sdl2:rect-x (playfield-bar taiko-playfield)))))

(define taiko-score 0)
(define (taiko-score->string)
  (string-pad (number->string taiko-score) 10 #\0))

(define taiko-beatmap #f)
; game variables
(define taiko-game-started #f)
(define taiko-game-start 0)
(define taiko-game-skip-offset 0)
(define taiko-game-offset 0)
; indices for current note/time/speed
(define taiko-game-curr-note 0)
(define taiko-game-curr-time 0)
(define taiko-game-curr-speed 0)

(define (taiko-init)
  (init-taiko-playfield)
  (taiko-start-game (read-beatmap "songs/oni.tko")))

(define (taiko-start-game beatmap)
  (set! taiko-game-started #t)
  (set! taiko-beatmap beatmap)
  (set! taiko-game-start (sdl2:get-ticks))
  (set! taiko-game-skip-offset 0)
  (set! taiko-game-offset 0))

(define (taiko-update-offset)
  (set! taiko-game-offset (- (sdl2:get-ticks) taiko-game-start)))

(define (taiko-update-curr-element dyn-vec count get-time curr update? func)
  (when (< curr count)
	;(display curr) (display "      ") (display count) (newline)
	(when (update? dyn-vec curr)
	  (func)
	  (set! curr (+ 1 curr))
	  (taiko-update-curr-element dyn-vec count get-time curr update? func))))
(define (taiko-update-curr-note)
  (define (update? dyn-vec i)
	(< (taiko-get-note-offset i) (- (taiko-get-max-hit-window))))
  (taiko-update-curr-element (beatmap-notes taiko-beatmap)
							 (beatmap-notes-count taiko-beatmap)
							 note-time
							 taiko-game-curr-note
							 update?
							 (lambda ()
							   (set! taiko-game-curr-note (+ 1 taiko-game-curr-note))
							   (taiko-miss))))

(define (taiko-miss)
  #f)

(define (taiko-get-perfect-hit-window)
  (beatmap-hit-window taiko-beatmap))
(define (taiko-get-good-hit-window)
  (beatmap-hit-window taiko-beatmap))
(define taiko-get-max-hit-window taiko-get-good-hit-window)

(define (taiko-get-note-offset index)
  (- (note-time (beatmap-get-note taiko-beatmap index)) taiko-game-offset))
(define (taiko-get-note-type index)
  (note-type (beatmap-get-note taiko-beatmap index)))

(define (taiko-render-playfield info)
  (taiko-render-background info)
  (taiko-render-bar info)
  (taiko-render-pad info)
  (taiko-render-drum info)
  (taiko-render-score info)
  (when taiko-game-started
	(taiko-render-notes info)))
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
(define (taiko-render-notes info)
  (define (get-note-type-width type)
	(case type
	  ((don katsu) +TAIKO-NOTE-WIDTH+)
	  ((big-don big-katsu +TAIKO-BIG-NOTE-WIDTH+))
	  (else 64)))
  (define (get-note-type-height type)
	(case type
	  ((don katsu) +TAIKO-NOTE-HEIGHT+)
	  ((big-don big-katsu +TAIKO-BIG-NOTE-HEIGHT+))
	  (else 64)))
  (define (get-note-type-texture type)
	(case type
	  ((don big-don) (playfield-don-texture info))
	  ((katsu big-katsu) (playfield-katsu-texture info))))
  (let ((pad-x (+ (sdl2:rect-x  (playfield-bar info))
				  (sdl2:point-x (playfield-pad-center info))))
		(pad-y (+ (sdl2:rect-y  (playfield-bar info))
				  (sdl2:point-y (playfield-pad-center info)))))
	(let ((note-count (beatmap-notes-count taiko-beatmap)))
	  (define (iter i)
		(when (< i note-count)
		  (let ((x-offset (/ (taiko-get-note-offset i) 1000))
				(type (taiko-get-note-type i)))
			(when (< x-offset 1)
			  (let ((rect (sdl2:make-rect (+ pad-x
											 (number->integer
											   (* x-offset window-width)))
										  pad-y
										  (get-note-type-width type)
										  (get-note-type-height type))))
				(render-texture-centered! (get-note-type-texture type) #f rect))))
		  (iter (+ i 1))))
	  (iter taiko-game-curr-note))))
(define (taiko-render-pad info)
  (let ((bar (playfield-bar info))
		(center (playfield-pad-center info))
		(size (playfield-pad-size info)))
	(render-texture-centered! (playfield-pad-texture info) #f
							  (sdl2:make-rect (+ (sdl2:rect-x bar) (sdl2:point-x center))
											  (+ (sdl2:rect-y bar) (sdl2:point-y center))
											  (sdl2:point-x size)
											  (sdl2:point-y size)))))

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

(define (taiko-update-game)
  (taiko-update-offset)
  (taiko-check-inputs)
  (taiko-update-curr-note))

(define (taiko-update)
  (taiko-update-game))

(define (taiko-render)
  (update-taiko-playfield)
  (taiko-render-playfield taiko-playfield))

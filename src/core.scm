(declare (uses RENDERER))
(declare (uses TAIKO))

(import miscmacros)
(import (prefix allegro al:))
(import (prefix sdl2 sdl2:))
(import (prefix sdl2-image img:))

(define window #f)
(define mixer #f)

; window-width/height are updated
; during any window event
(define window-width  800)
(define window-height 600)

(define (sdl-error-string)
  (string-append "SDL: " (sdl2:get-error)))
(define (img-error-string)
  (string-append "IMG: " (img:get-error)))

; calls "core-gamestate"-update and "core-gamestate"-render
(define core-gamestate "taiko")
(define (core-gamestate-update)
  (let ((sym (string->symbol (string-append core-gamestate "-update"))))
	(eval (list sym))))
(define (core-gamestate-render)
  (let ((sym (string->symbol (string-append core-gamestate "-render"))))
	(eval (list sym))))

;;; #t on success
(define (core-init!)
  (define (check thing) (unless thing (error (sdl-error-string))))
  (sdl2:set-main-ready!)
  (sdl2:init! '(video))
  (img:init! '(jpg png))
  (set! window (sdl2:create-window! "Taiko" 'centered 'centered window-width window-height '(resizable)))
  (check window)
  (set! renderer (sdl2:create-renderer! window -1 '(accelerated)))
  (check renderer)
  (load-core-texs +IMG_PATH+ renderer)
  (init-glyph-fonts)
  (taiko-init)
  #t)

(define (core-quit!)
  (img:quit!)
  (sdl2:quit!))
(on-exit core-quit!)

(define (core-handle-event ev exit-main-loop!)
  (define (mouse-button-symbol->key-symbol button)
	(string->symbol (string-append "mouse-" (symbol->string button))))
  (case (sdl2:event-type ev)
	((key-down)
	 (keypresses-add-record! (sdl2:keyboard-event-sym ev) 'down))
	((key-up)
	 (keypresses-add-record! (sdl2:keyboard-event-sym ev) 'up))
	((mouse-button-down)
	 (keypresses-add-record! (mouse-button-symbol->key-symbol (sdl2:mouse-button-event-button ev))
							 'down))
	((mouse-button-up)
	 (keypresses-add-record! (mouse-button-symbol->key-symbol (sdl2:mouse-button-event-button ev))
							 'up))
	((window)
	 (let-values (((x y) (sdl2:window-size window)))
	   (set! window-width  x)
	   (set! window-height y)))
	((quit)
	 (exit-main-loop! #t))))

(define (core-render)
  ;(clear-screen 0 0 0)
  (core-gamestate-render)
  (sdl2:render-present! renderer))

(define (core-log str)
  (display "log: " str) (newline))

(define (core-main-loop)
  (let/cc exit-main-loop!
	(while #t
	  (let ((ev (sdl2:poll-event!)))
		(while ev
		  (core-handle-event ev exit-main-loop!)
		  (set! ev (sdl2:poll-event!))))
	  (core-render)
	  (core-gamestate-update)
	  (keypresses-update!)
	  (sdl2:delay! 15))))

(when (core-init!)
  (core-main-loop))

(declare (unit INPUT))

(import (prefix sdl2 sdl2:))

; keypress state can be 'null 'down 'held 'up
; keypress record can also be used for mouse button events

(: make-keypress (symbol symbol -> pair))
(define (make-keypress key state)
  (cons key state))
(define (keypress-key kp)
  (car kp))
(define (keypress-state kp)
  (cdr kp))
(define (keypress-state-set! kp state)
  (set-cdr! kp state))

; keypresses is updated with keypress records
; whenever a key is pressed
(define keypresses (cons '*keypresses* '()))

(define (keypresses-lookup key)
  (assq key (cdr keypresses)))
(define (keypresses-state key)
  (let ((found (keypresses-lookup key)))
	(if found
		(cdr found)
		'null)))

(define (keypresses-add-record! key state)
  (let ((existing (keypresses-lookup key)))
	(if existing ; update state if keypress already exists
		(keypress-state-set! existing state)
		(let ((new-record (cons (make-keypress key state)
								(cdr keypresses))))
		  (set-cdr! keypresses new-record)))))

; changes 'down keypresses -> 'held
; removes 'up keypresses
(define (keypresses-update!)
  (define (update-down! kp-pair)
	(when (eq? (cdr kp-pair) 'down)
	  (keypress-state-set! kp-pair 'held)))
  (define (iter head)
	(unless (null? head)
	  (let ((record (cdr head)))
		(unless (null? record)
		  (when (eq? 'up (cdar record))
			(set-cdr! head (cdr record)))
		  (iter (cdr head))))))
  (iter keypresses)
  (for-each update-down! (cdr keypresses)))

(define bind-left-outer-hit 'h)
(define bind-right-outer-hit 'l)
(define bind-left-inner-hit 'j)
(define bind-right-inner-hit 'k)

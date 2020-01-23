(declare (unit BEATMAP))
(declare (uses DYNAMIC-VECTOR))

(import (chicken file posix)
		(chicken io)
		(chicken string)
		srfi-13
		srfi-1)

; notes are time symbols pairs
; eg. (200 'don)
; symbols are
; 'don 'katsu 'big-don 'big-katsu

(define (make-note time type)
  (cons time type))
(define note-time car)
(define note-type cdr)

; time-section
(define-record time-section
			   time
			   bpm
			   beat-count
			   beat-value)

; speed section is a time fractional number pair
(define (make-speed-section time speed)
  (cons time speed))
(define speed-section-time car)
(define speed-section-speed cdr)

(define-record beatmap
  notes          ; don/katsu notes
  time-sections  ; bpm and time signature sections
  speed-sections ; note speed sections

  song-name       ; string
  artist-name     ; string
  music-file      ; string
  difficulty-name ; string
  hit-window      ; number
  base-speed      ; number
)

(define (beatmap-get-note bm index)
  (dyn-vec-ref (beatmap-notes bm) index))
(define (beatmap-get-time-section bm index)
  (dyn-vec-ref (beatmap-time-sections bm) index))
(define (beatmap-get-speed-section bm index)
  (dyn-vec-ref (beatmap-speed-sections bm) index))

(define (beatmap-notes-count bm)
  (dyn-vec-count (beatmap-notes bm)))
(define (beatmap-time-sections-count bm)
  (dyn-vec-count (beatmap-time-sections bm)))
(define (beatmap-speed-sections-count bm)
  (dyn-vec-count (beatmap-speed-sections bm)))

; returns beatmap on success
; returns #f on failure
(define (read-beatmap path)
  (let ((file (file-open path open/rdonly)))
	(if file
		(let ((port (open-input-file* file))
			  (line "")
			  (notes (make-dyn-vec 32))
			  (time-sections (make-dyn-vec 4))
			  (speed-sections (make-dyn-vec 16))
			  (song-name   "<unknown song name>")
			  (artist-name "<unknown artist name>")
			  (music-file  "")
			  (difficulty-name "Taiko!")
			  (hit-window "50")
			  (base-speed "1.0"))
		  (define (set-beatmap-value! symbol value)
			(case symbol
			  ((song-name)   (set! song-name value))
			  ((artist-name) (set! artist-name value))
			  ((music-file)   (set! music-file value))
			  ((difficulty-name) (set! difficulty-name value))
			  ((hit-window) (set! hit-window value))
			  ((base-speed) (set! base-speed value))))
		  (define (parse-header str)
			(cond ((eof-object? str) (error "unexpected eof"))
				  ((string=? str ".end") #t)
				  (else (let ((colon-pos (string-index str #\:)))
						  (when colon-pos
							(let ((name (substring str 0 colon-pos))
								  (value (substring str (+ colon-pos 1) (string-length str))))
								  (set-beatmap-value! (string->symbol name) value))))
						(parse-header (read-line port)))))
		  (define (get-time-section time data)
			(let ((split (string-split data " ")))
			  (make-time-section time
								 (string->number (car split))
								 (string->number (cadr split))
								 (string->number (caddr split)))))
		  (define (parse-notes str)
			(cond ((eof-object? str) (error "unexpected eof"))
				  ((string=? str ".end") #t)
				  ((not (string=? str ""))
				   (let ((type (string-ref str 0))
						 (space-pos (string-index str #\space)))
					 (when space-pos
					   (let ((time (string->number (substring str 1 space-pos)))
							 (data (substring str (+ space-pos 1) (string-length str))))
						 (case type
						   ((#\@) (dyn-vec-push! notes (make-note time (string->symbol data))))
						   ((#\^) (dyn-vec-push! speed-sections (make-speed-section time
																					(string->number data))))
						   ((#\*) (dyn-vec-push! time-sections (get-time-section time data)))))))
				   (parse-notes (read-line port)))))
		  (define (loop str)
			(cond ((eof-object? str) #t)
				  ((string=? str ".header")
				   (parse-header (read-line port))
				   (loop (read-line port)))
				  ((string=? str ".begin")
				   (parse-notes (read-line port))
				   (loop (read-line port)))
				  (else (loop (read-line port)))))
		  (loop (read-line port))
		  (file-close file)
		  ;(display notes) (newline)
		  ;(display speed-sections) (newline)
		  ;(display time-sections) (newline)
		  (make-beatmap notes
						time-sections
						speed-sections
						song-name
						artist-name
						music-file
						difficulty-name
						(string->number hit-window)
						(string->number base-speed))))))

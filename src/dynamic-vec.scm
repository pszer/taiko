(declare (unit DYNAMIC-VECTOR))

(import vector-lib)

; A dynamic vector is made up of a vector, its element count
; and its current size. Whenever an element is to be added
; and more space is needed the vector automatically expands
(define (make-dyn-vec init-size)
  (cons (cons 0 init-size) (make-vector init-size)))

(define (dyn-vec-size   dv) (cdar dv))
(define (dyn-vec-count  dv) (caar dv))
(define (dyn-vec-vector dv) (cdr dv))

(define (set-dyn-vec-size! dv size)
  (set-cdr! (car dv) size))
(define (set-dyn-vec-count! dv count)
  (set-car! (car dv) count))
(define (set-dyn-vec-vector! dv vec)
  (set-cdr! dv vec))

(define (dyn-vec-ref dv index)
  (if (or (< index 0) (>= index (dyn-vec-count dv)))
	  (error "index out of bounds : DYNAMIC-VECTOR-REF" index count)
	  (vector-ref (dyn-vec-vector dv) index)))
(define (dyn-vec-set! dv index object)
  (if (or (< index 0) (>= index (dyn-vec-count dv)))
	  (error "index out of bounds : DYNAMIC-VECTOR-SET!" index (dyn-vec-count dv))
	  (vector-set! (dyn-vec-vector dv) index object)))

; Unsafe operations
(define (unsafe-dyn-vec-ref dv index)
  (vector-ref (dyn-vec-vector dv) index))
(define (unsafe-dyn-vec-set! dv index object)
  (vector-set! (dyn-vec-vector dv) index object))

(define (dyn-vec-full? dv)
  (= (dyn-vec-size dv) (dyn-vec-count dv)))
(define (dyn-vec-grow! dv new-size)
  (set-dyn-vec-vector! dv (vector-copy (dyn-vec-vector dv) 0 new-size #f))
  (set-dyn-vec-size!   dv new-size)
  'done)

(define (dyn-vec-push! dv object)
  (define (double-size)
	(dyn-vec-grow! dv (+ 1 (* 2 (dyn-vec-size dv)))))
  (when (dyn-vec-full? dv) (double-size))
  (let ((count (dyn-vec-count dv)))
	(set-dyn-vec-count! dv (+ 1 count))
	(unsafe-dyn-vec-set! dv count object))
  'done)
(define (dyn-vec-erase! dv index)
  (let ((count (dyn-vec-count dv))
		(end (- (dyn-vec-count dv) 1)))
	(when (or (< index 0) (>= index count))
	  (error "index out of bounds : DYNAMIC-VECTOR-ERASE" index count))
	(define (iter i)
	  (unless (= i end)
		(unsafe-dyn-vec-set! dv
							 (unsafe-dyn-vec-ref dv (+ i 1)))
		(iter (+ i 1))))
	(iter index)
	(set-dyn-vec-count! dv end)))

(define (dyn-vec-assoc dv key)
  (let ((count (dyn-vec-count dv)))
	(define (iter i)
	  (if (= i count)
		  #f
		  (let ((record (unsafe-dyn-vec-ref dv i)))
			(if (equal? (car record) key)
				(cdr record)
				(iter (+ i 1))))))
	(iter 0)))
; Returns index of pair, or -1 if doesnt exist
(define (dyn-vec-assoc-index dv key)
  (let ((count (dyn-vec-count dv)))
	(define (iter i)
	  (if (= i count)
		  -1
		  (let ((record (unsafe-dyn-vec-ref dv i)))
			(if (equal? (car record) key)
				i
				(iter (+ i 1))))))
	(iter 0)))

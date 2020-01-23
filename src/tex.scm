(declare (unit TEXTURE))
(declare (uses DYNAMIC-VECTOR))
(declare (uses FILESYSTEM))

(import (prefix sdl2 sdl2:))
(import (prefix sdl2-image img:))

(define +IMG_PATH+ "img/")
(define +IMG_EXTS+ (list ".png" ".jpg"))

; All 'core' textures found in the img/ folder
; all loaded statically into the core-texs vector.
; All dynamically loaded textures are put into cache-texs.
;
; core-texs and cache-texs are both association vectors, each entry
; is a textures name and texture data cons'd together.
;
; (lookup-texture key) takes in either a number or string key.
; A number key looks up the core-texs textures.
; A string key looks up the cache-texs textures.
; Returns #f if no texture found, otherwise returns sdl2:texture
; 'cache-texs' are not looked up by index because cache-texs is not
; guaranteed to be static.

; Creates a read-only texture record
(define (make-texture name img)
 (cons name img))
(define (texture-name tex) (car tex))
(define (texture-img  tex) (cdr tex))

(define (lookup-texture key)
  (cond ((number? key) (lookup-core-tex key))
		((string? key) (lookup-cache-tex key))
		(else #f)))
(define (lookup-core-tex index)
  (if (or (< index 0) (>= index (dyn-vec-count core-texs)))
	  #f ; no texture found (index out of bounds)
	  (texture-img (unsafe-dyn-vec-ref core-texs index)))) ; bounds already checked so unsafe is safe
(define (lookup-cache-tex name)
  (dyn-vec-assoc cache-texs name)) ; Uses an assocation vector search

; Gets the index of a loaded texture in core-texs
(define (core-texs-index name)
  (dyn-vec-assoc-index core-texs name))

; Loads texture at path.
; If texture loaded succesfully, returns a texture
; record with a given name, otherwise returns #f on error.
(define (load-texture path name renderer)
  (define (get-surface)
	(condition-case (img:load path)
					[(exn sdl2) #f]
					[var () void]))
  (condition-case (make-texture name 
								(sdl2:create-texture-from-surface renderer (get-surface)))
				  [(exn sdl2) #f]
				  [var () void]))

; Pushes texture at img/$fname into vec.
; Returns #t/#f for success/fail.
(define (mount-texture! vec fname renderer)
  (let ((path (string-append +IMG_PATH+ fname)))
	(let ((texture (load-texture path fname renderer)))
	  (if (not texture)
		  #f
		  (dyn-vec-push! vec texture)))))
(define (mount-texture-with-name! vec path name renderer)
  (let ((texture (load-texture path name renderer)))
	(if (not texture)
		#f
		(dyn-vec-push! vec texture))))

(define (load-core-texs folder renderer)
  (define (load path)
	(let ((name (trim-path path 1)))
	  (mount-texture-with-name! core-texs path (trim-path path 1) renderer)))
  (for-each load (find-file-types folder +IMG_EXTS+)))

(define core-texs (make-dyn-vec 32))
(define cache-texs (make-dyn-vec 32))

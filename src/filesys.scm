(declare (unit FILESYSTEM))

(import (chicken file)
		srfi-13
		srfi-1)

(define (is-extension? file ext)
  (let ((file-len (string-length file))
		(ext-len  (string-length ext)))
	(if (> ext-len file-len)
		#f
		(string=? ext (substring file (- file-len ext-len) file-len)))))

; Returns all files that end with a string
; in the list of strings exts
; e.g. (find-file-types "img/" '(".png" ".jpg")) finds all png/jpg files
(define (find-file-types folder exts)
  (define (is-exts-file? file)
	(any (lambda (e) (is-extension? file e)) exts))
  (let ((files (find-files folder)))
	(filter is-exts-file? files)))

; Trims 'depth' amount of path parent folders.
; i.e (trim-path "img/don.png" 1) -> "don.png"
;     (trim-path "folder/misc/misc2/test.png" 2) -> "misc2/test.png"
;     (trim-path "img/don.png" 999) -> "don.png"
(define (trim-path path depth)
  (let ((len (string-length path)))
	(define (iter pos depth)
	  (if (= depth 0)
		  (substring path pos len)
		  (let ((next-folder (string-index path #\/ pos len)))
			(if (number? next-folder)
				(iter (+ next-folder 1) (- depth 1))
				(substring path pos len)))))
	(iter 0 depth)))

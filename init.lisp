;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package pdf)

(defvar *zlib-loaded* nil)

(defun find-zlib-path ()
  #-(or macosx darwin)
  (uffi:find-foreign-library
   "libz"
   *zlib-search-paths*
   :drive-letters '("C" "D" "E")
   :types '("so" "a" "dll"))
  #+(or macosx darwin)
  (uffi:find-foreign-library
   "z"
   `(,(pathname-directory *load-pathname*))))

(defun load-zlib (&optional force)
  (when force (setf *zlib-loaded* nil))
  (unless *zlib-loaded*
    (let ((zlib-path (find-zlib-path)))
      (if zlib-path
	  (progn
	    (format t "~&;;; Loading ~s" zlib-path)
	    (uffi:load-foreign-library zlib-path
				       :module "zlib" 
				       :supporting-libraries '("c"))
	    (uffi:def-function ("compress" c-compress)
		((dest (* :unsigned-char))
		 (destlen (* :long))
		 (source :cstring)
		 (source-len :long))
	      :returning :int
	      :module "zlib")
	    (setf *zlib-loaded* t *compress-streams* t))
	  (progn
	    (warn "Unable to load zlib. Disabling compression.")
	    (setf *compress-streams* nil))))))

(load-zlib)

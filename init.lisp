;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package pdf)

(defvar *zlib-loaded* nil)

#+uffi
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun find-zlib-path ()
  (uffi:find-foreign-library
   "libz"
   *zlib-search-paths*
   :drive-letters '("C" "D" "E")
   :types '("so" "a" "dll" "dylib"))))

#+(and cmu uffi)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((zlib-path (find-zlib-path)))
    (when zlib-path
      (format t "~&;;; Loading ~s" zlib-path)
      (uffi:load-foreign-library zlib-path
                                 :module "zlib" 
                                 :supporting-libraries '("c")))))


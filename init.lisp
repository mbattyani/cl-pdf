;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package pdf)

(defun maybe-load-zlib ()
  (cond
    (*zlib-path*
      (format t "~&;;; Loading ~s" *zlib-path*)
      (uffi:load-foreign-library *zlib-path*
                                 :module "zlib" 
                                 :supporting-libraries '("c"))
      (push :zlib cl:*features*))
    (t
      (warn "Unable to load zlib. Disabling compression.")
      (setf *compress-streams* nil))))

(maybe-load-zlib)

;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package asdf)

;;;Choose the zlib implementation you want to use (only one!)
(pushnew :use-salza-zlib cl:*features*)
;(pushnew :use-uffi-zlib cl:*features*)
;(pushnew :use-abcl-zlib cl:*features*)
;(pushnew :use-no-zlib cl:*features*)

#-(or use-uffi-zlib use-salza-zlib use-abcl-zlib use-no-zlib)
(Error "You must choose which zlib version you want to use")

#-(or uffi (not use-uffi-zlib))
(ignore-errors
  (print "Trying to load UFFI:")
  (asdf:operate 'asdf:load-op :uffi)
  (pushnew :uffi cl:*features*)
  (print "UFFI loaded."))

(load (merge-pathnames "iterate/iterate.asd" *load-truename*))

#+use-salza-zlib
(load (merge-pathnames "salza/salza.asd" *load-truename*))

#+clisp (setf *warn-on-floating-point-contagion* nil)

(defsystem :cl-pdf
  :name "cl-pdf"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :version "2.0"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :licence "BSD like licence"
  :description "Common Lisp PDF Generation Library"
  :long-description "The cl-pdf package provides a stand-alone Common Lisp library to generate PDF files."
  :perform (load-op :after (op cl-pdf)
		    (pushnew :cl-pdf cl:*features*))
  :components ((:file "defpackage")
	       (:file "config" :depends-on ("defpackage"))
	       #+use-uffi-zlib (:file "init" :depends-on ("config"))
	       (:file "zlib" :depends-on (#+use-uffi-zlib "init"))
	       (:file "font-metrics"  :depends-on ("config"))
	       (:file "encodings"  :depends-on ("defpackage"))
	       (:file "t1-font" :depends-on ("font-metrics" "encodings"))
	       (:file "font" :depends-on ("t1-font"))
	       (:file "pdf" :depends-on ("font"))
	       (:file "x11-colors" :depends-on ("defpackage"))
	       (:file "pdf-base" :depends-on ("pdf" "x11-colors"))
	       (:file "png" :depends-on ("pdf-base"))
	       (:file "pdf-geom" :depends-on ("pdf-base"))
	       (:file "text" :depends-on ("pdf-base"))
	       (:file "bar-codes" :depends-on ("pdf-geom"))
	       (:file "chart" :depends-on ("text" "pdf-geom")))
  :depends-on (:iterate #+use-salza-zlib :salza))

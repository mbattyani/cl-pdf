;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package asdf)

#-uffi
(ignore-errors
  (print "Trying to load UFFI:")
  (asdf:operate 'asdf:load-op :uffi)
  (pushnew :uffi cl:*features*)
  (print "UFFI loaded."))

(load (merge-pathnames "iterate/iterate.asd" *load-truename*))

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
	       (:file "init" :depends-on ("config"))
	       (:file "zlib" :depends-on ("init"))
	       (:file "font-metrics"  :depends-on ("config"))
	       (:file "encodings"  :depends-on ("defpackage"))
	       (:file "t1-font" :depends-on ("font-metrics" "encodings"))
	       (:file "font" :depends-on ("t1-font"))
	       (:file "pdf" :depends-on ("font"))
	       (:file "x11-colors" :depends-on ("defpackage"))
	       (:file "pdf-base" :depends-on ("pdf" "x11-colors"))
	       (:file "pdf-geom" :depends-on ("pdf-base"))
	       (:file "text" :depends-on ("pdf-base"))
	       (:file "bar-codes" :depends-on ("pdf-geom"))
	       (:file "chart" :depends-on ("text" "pdf-geom")))
  :depends-on (:iterate))

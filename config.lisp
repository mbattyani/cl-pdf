;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package pdf)

;;; This file contains some special variables which need to be set
;;; depending on your Lisp implementation/OS/installation.

(defconstant +external-format+ #-(or lispworks clisp allegro) :default
   #+(and allegro mswindows) (excl:crlf-base-ef :1252) ;;:1252-base
   #+(and allegro unix) :default
   #+lispworks '(:latin-1 :eol-style :lf)
   #+clisp :unix)

(defvar *min-size-for-compression* 300)

(defvar *compress-streams* nil
  "Enables the internal streams compression by zlib")

;; The *afm-files-directories* is only for the 14 predefined fonts.
;; other fonts must have their afm files read only when they are loaded
(defparameter *afm-files-directories* (when *load-pathname*
					(list (merge-pathnames #P"afm/*.afm" *load-pathname*)))
  "The directory containing the Adobe Font Metrics files for the 14 predefined fonts")

;; define the :pdf-binary feature if your Lisp implementation accepts
;; to write binary sequences to character streams
;; For LW you need version 4.2.7 minimum
#+(or lispworks allegro)
(pushnew :pdf-binary *features*)

;(eval-when (:compile-toplevel :load-toplevel :execute)
#+use-uffi-zlib
(defvar *zlib-search-paths* `(,(directory-namestring *load-truename*)
                              #+lispworks
                              ,(directory-namestring (lw:lisp-image-name))
                              "/usr/local/lib/"
                              "/usr/lib/"
                              "/windows/system32/"
                              "/winnt/system32/")
  "The paths where to search the zlib shared library")

;a catchall for vaious kind of errors that can appen in the generation of a document.
; just catch 'max-number-of-pages-reached if you want to do something with this.
(defvar *max-number-of-pages* 1000
  "The maximum number of pages for a document")

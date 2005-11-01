;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package pdf)

;;; This file contains some special variables which need to be set
;;; depending on your Lisp implementation/OS/installation.

(defconstant +external-format+
  #-(or sbcl lispworks clisp allegro) :default
  #+sbcl :latin-1
  #+(and allegro mswindows) :octets
  #+(and allegro unix) :default
  #+lispworks '(:latin-1 :eol-style :lf)
  #+clisp :unix)

#+clisp
(setf *default-file-encoding*  (ext:make-encoding :charset charset:iso-8859-1))

(defvar *min-size-for-compression* 300)

(defvar *compress-streams* nil
  "Enables the internal streams compression by zlib")

;the cl-pdf base directory
(defvar *cl-pdf-base-directory*
   (make-pathname :name nil :type nil :version nil
     :defaults #.(or #-gcl *compile-file-truename* *load-truename*))
   "The base directory for cl-pdf source and auxiliary data")

;; The *afm-files-directories* is only for the 14 predefined fonts.
;; other fonts must have their afm files read only when they are loaded
(defparameter *afm-files-directories*
  (list (merge-pathnames #P"afm/*.afm" *cl-pdf-base-directory*))
  "The directory containing the Adobe Font Metrics files for the 14 predefined fonts")

;; define the :pdf-binary feature if your Lisp implementation accepts
;; to write binary sequences to character streams
;; For LW you need version 4.2.7 minimum
#+(or lispworks allegro sbcl)
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

;a catchall for various kind of errors that can happen in the generation of a document.
; just catch 'max-number-of-pages-reached if you want to do something with this.
(defvar *max-number-of-pages* 1000
  "The maximum number of pages for a document")


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

(defvar *compress-streams* t
  "Enables the internal streams compression by zlib")

;; The *afm-files-directories* is only for the 14 predefined fonts.
;; other fonts must have their afm files read only when they are loaded
(defparameter *afm-files-directories* (when *load-pathname*
					(list (merge-pathnames #P"afm/*.afm" *load-pathname*)))
  "The directory containing the Adobe Font Metrics files for the 14 predefined fonts")

;; define the :pdf-binary feature if your Lisp implementation accepts
;; to write binary sequences to character streams
;; For LW you need version 4.2.7 minimum
#+(or lispworks acl)
(pushnew :pdf-binary *features*)

;(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *zlib-search-paths* '("/usr/local/lib/" "/usr/lib/")
  "The paths where to search the zlib shared library")

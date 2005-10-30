;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Touched: Sat Oct 22 09:07:10 2005 +0530 <enometh@net.meer>
;;;   Time-stamp: <05/10/22 12:23:36 madhu>
;;;
;;;; vim:ft=lisp
;;;; Touched: <01-Oct-03 04:24:27 IST, madhu>
;;;; Type 3 Font Dictionaries.
;;;; Bugs-To: <enometh@net.meer>
;;;;
(defpackage "TYPE3"
  (:nicknames "T3")
  (:use "CL"))
(in-package "TYPE3")

;;(require 'cl-pdf)
;;(load (merge-pathnames #p"pktype.lisp" *cl-pdf-srcdir*))

; skulduggery
(defclass fake-font-mixin () ; faked (not computed)
  ((first-char :initarg :first-char :accessor first-char :initform 0)
   (last-char :initarg :last-char :accessor last-char :initform 255)
   (marked-chars :initarg :marked-chars :accessor marked-chars
		 :initform (loop for x from 0 to 255 collect x))))

(defclass t3-char-metrics (PDF:char-metrics)
  ((h :accessor h :initarg :h) ; pixel height
   (w :accessor w :initarg :w) ; pixel width
   (raster :accessor raster :initarg :raster)
   (rastersize :accessor rastersize :initarg :rastersize)
   (rw :accessor rw :initarg :rw)))

(defclass t3-font-metrics (PDF:font-metrics fake-font-mixin)
  ((image-used :accessor image-used)
   (font-scale :accessor font-scale)))

(defun load-pk-font (path &rest opts) ;; XXX first-char last-char marked-chars
  (loop with pkfile = (pk:get-pkfile path)
        and t3fm =(apply 'make-instance 't3-font-metrics opts)
        for car in (marked-chars t3fm) ;;  below 256
        for char-name = (format nil "/a~d" car) ; note: leading /!
        for (x-off y-off width height raster rastersize rw) = 
        (pk:pk-char-details pkfile car)
        for llx = (- x-off) 
        for lly = (1+ (- y-off height)) 
        for urx = (1+ (+ llx width))
        for ury = (+ lly height) 
        minimizing llx into b0
        minimizing lly into b1
        maximizing urx into b2
        maximizing ury into b3
        do
        (when (or (< width 1) (< height 1)) (error "todo: handle null-glyphs"))
        (let ((char-metrics 
                (make-instance 't3-char-metrics 
                               :code car :name char-name
                               :bbox (make-array 4 :initial-contents 
                                                 (list llx lly urx ury))
                               :h height :w width
                               :rastersize rastersize :raster raster :rw rw
                               :width  (pk:pk-char-width pkfile car))))
          (setf (aref (PDF::encoding-vector t3fm) car) char-metrics)
          (setf (gethash char-name (PDF:characters t3fm)) char-metrics))
        finally
        (setf (image-used t3fm) t)
        (setf (font-scale t3fm) (pk:pk-font-scale pkfile))
        (setf (PDF:font-bbox t3fm) 
              (make-array 4 :initial-contents (list b0 b1 b2 b3)))
	;;; XXX
        (setf (PDF:font-name t3fm) (pk:pk-font-name pkfile))
        (setf (gethash (PDF:font-name t3fm) PDF::*font-metrics*) t3fm)
        (setf (PDF:encoding-scheme t3fm) nil) ; XXX
	(setf (pdf::full-name t3fm)
	      (concatenate 'string "T3 Font [" (pdf:font-name t3fm) "]"))
        (return t3fm)))

;;; CL-PDF protocols
;;; 
;;; 
(defmethod PDF::font-descriptor ((t3fm t3-font-metrics)
                            &key (embed pdf::*embed-fonts*) (errorp t))
  (declare (ignore t3fm embed errorp))
  (error "Type3 fonts dont have descriptors"))

(defmethod PDF::font-type ((t3fm t3-font-metrics))
  (declare (ignore t3fm))
  "Type3")


;;; helpers for writing out pdf
;;; 
;;;
(defmethod %char-procs-content-stream ((t3cm t3-char-metrics))
  (let ((content-stream (make-instance 'PDF::pdf-stream)) p)
    (with-slots (h w raster rastersize rw PDF:bbox PDF:width) t3cm
      (destructuring-bind (llx lly urx ury) 
          (loop for x across PDF:bbox collect x)
        (setf (PDF::content content-stream)
              (with-output-to-string (stream)
                (format stream "~D 0 ~D ~D ~D ~D d1~%"  
                        PDF:width ;; index into widths array
                        llx lly urx ury)
                ;; if null-glyph prolly end-stream here
                (format stream "q~%~D 0 0 ~D ~D ~D cm~%BI~%" w h llx lly)
                (format stream "/W ~D~%/H ~D~%" w h)
                (format stream "/IM true~%/BPC 1~%/D [1 0]~%ID ")
                (setf p 0)
                (loop repeat h do 
                (loop repeat rw do ;; #+pdf-binary etc todo
                      (write-char (code-char (aref raster p)) stream)
                      (incf p)))
                (assert (= p rastersize))
                (format stream "~%EI~%Q")))))
    content-stream))


(defmethod %make-char-procs-dict ((t3fm t3-font-metrics))
  (loop for t3cm across (PDF::encoding-vector t3fm)
        when t3cm collect (cons (PDF:name t3cm)
                                (make-instance 
                                  'PDF::indirect-object
                                  :content (%char-procs-content-stream t3cm)))
        into dict-values
        finally (return (make-instance 
                          'PDF::dictionary :dict-values dict-values))))

(defmethod %make-resources-dict ((t3fm t3-font-metrics))
  (make-instance 
    'PDF::dictionary
    :dict-values
    `(("/ProcSet" .
       ,(format nil "[ /PDF ~A ]" (if (image-used t3fm) "/ImageB" ""))))))

(defmethod %compute-encoding-differences ((t3fm t3-font-metrics));XXX
  (with-output-to-string (s)
    (loop with undef-p for i from (first-char t3fm) to (last-char t3fm)
	  do (if (= i (first-char t3fm))
	       (cond ((aref (PDF::encoding-vector t3fm) i)
		      (setf undef-p nil)
		      (format s "[~a/a~d" i i))
		     (t (setf undef-p t)
			(format s "[~a/.notdef" i)))
	       (cond ((aref (PDF::encoding-vector t3fm) i)
		      (when undef-p
			(setf undef-p nil)
			(format s " ~d" i))
		      (format s "/a~d" i))
		     (t (unless undef-p
			  (setf undef-p t)
			  (format s " ~d/.notdef" i)))))
	  finally (format s "]"))))


(defmethod %make-t3-font-dict  ((t3fm t3-font-metrics))
  (make-instance
    'PDF::dictionary
    :dict-values
    `(("/Type" . "/Font")
      ("/Subtype" . "/Type3")
      ("/FontMatrix" . ,(let ((font-scale (font-scale t3fm)))
                          (make-array 6 :initial-contents
                                      (list font-scale 0 0 font-scale 0 0))))
      ("/FontBBox" . ,(PDF:font-bbox t3fm))
      ("/Resources" . ,(%make-resources-dict t3fm))
      ("/FirstChar" . ,(first-char t3fm))
      ("/LastChar" . ,(last-char t3fm))
      ("/Widths" . ,(loop for car from (first-char t3fm) to (last-char t3fm)
                          for t3cm = (aref (PDF::encoding-vector t3fm) car)
                          collect (if t3cm (PDF:width t3cm) 0) into widths
                          finally (return (make-array (length widths)
                                            :initial-contents widths))))
      ("/Encoding" . ,(make-instance
                        'pdf::indirect-object :content
                        (make-instance
                          'pdf::dictionary :dict-values
                          `(("/Type" . "/Encoding")
                            ("/Differences" .
                             ,(%compute-encoding-differences t3fm))))))
      #+nil ;; / BUG
      ("/Encoding" . ,(pdf::find-encoding-object
                        (pdf::extract-font-metrics-encoding t3fm)))
      ("/CharProcs" . ,(make-instance
                         'PDF::indirect-object
                         :content (%make-char-procs-dict t3fm))))))


;;; CL-PDF Protocols: contd
;;;
(defmethod PDF::make-dictionary ((fm t3-font-metrics)
                            &key font (encoding NIL)
				 ;; NOTE NO (pdf:encoding font) for t3-font
				 (embed pdf::*embed-fonts*)
                            &allow-other-keys)
  (warn "T3-font-metrics: ignoring bogus (font encoding embed): ~S"
	(list font encoding embed))
  ;;; This comment is preserved from an earlier patched version of
  ;;; pdf.lisp in the %INITIALIZE-FONT-OBJECT method specialized on a
  ;;; second parameter T3-FONT. With that architecture we were able to
  ;;; patch the FONT-OBJECT's CONTENT after creating the dictionary.
  ;;;
  ;;; (see `pdf::font-object's initialize-instance)
  ;;;
  ;;; ;; 1.4 spec doesnt require the next entry, but acrobat3 failed without it
  ;;; (pdf::add-dict-value (pdf::content font-object)
  ;;;		       "/Name" (pdf::name font-object))
  (%make-t3-font-dict fm))



;;; ----------------------------------------------------------------------
;;; [Optionally] PATCH font.lisp  madhu 051022
(in-package "PDF")
(defmethod initialize-instance :after ((font font) &rest init-options &key encoding &allow-other-keys)
  (let ((font-metrics (gethash (name font) *font-metrics*)))
    (unless font-metrics (error "Font ~s not found" (name font)))
    (setf (font-metrics font) font-metrics)
    (unless encoding 
      (setf (gethash (list (name font) nil) *font-cache*) font))
    (setf (encoding font)
	  (if encoding
	      (get-encoding encoding)
	      (extract-font-metrics-encoding font-metrics)))
    (setf (gethash (list (name font) (encoding font)) *font-cache*) font)
    (loop with font-characters = (characters font-metrics)
	  with pdf-widths = (pdf-widths font)
	  with void-char = (gethash "VoidCharacter" font-characters)
	  and characters = (characters font)
	  and hyphen-code = nil
	  for i from 0 to 255
	  for char-name across (char-names (encoding font))
	  for char = (or (gethash char-name font-characters)
                         (aref (encoding-vector font-metrics) i)
                         void-char)
	  when char ;; <- HERE ------------------------------------------
	  do  (setf (aref characters i) char
		   (aref pdf-widths i) (round (* 1000 (width char))))
	  (when (and (not hyphen-code) (string= char-name "hyphen"))
	    (setf hyphen-code i
		  (hyphen-code font) i
		  (hyphen-char font) (code-char i))))
    (compute-kern-pairs font)))

(in-package "TYPE3")

#||
(progn
;;;---------------------------------------------------------------------------
;;; Sample Use: interface to PDF

;; font-metrics with faked up typesetter state 
(defvar *t3fm* (load-pk-font
		#p"home:cl/pk/sktbs10.864pk"
		:first-char 4
		:last-char 136
		:marked-chars
		'(4 10 23 33 58 59 77 97 100 109 112 116 118 120 121 126 136)))

;; register our T3 font
(make-instance 'pdf::font :name "sktbs10" :font-metrics *t3fm*
	       :encoding pdf::*default-encoding*)

;; is it available?
(pdf:get-font "sktbs10")

(defun t3-example (&optional (file #p"t3.pdf") &aux
                             (pdf:*compress-streams* nil))
  (pdf:with-document    ()
    (pdf:with-page      ()
      (pdf:with-outline-level
        ("PFB Example" (pdf:register-page-reference))
        (let ((skt #+nil(make-instance 't3-font :t3-font-metrics *t3fm*)
		   (pdf:get-font "sktbs10")))
         (pdf:in-text-mode
            (pdf:set-font skt 14.346)
            (pdf:move-text 200 800)
            (pdf:show-text ";\\012a:\\210!a;pa;a;d\\027;~ya;!a;mxa;tM\\027a;\\012a;d:!;v\\004a;a")))))
    (pdf:write-document file)))

(t3-example))
||#

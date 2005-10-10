;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;;; Touched: <18-Nov-03 09:23:16 IST, madhu>
;;; T3 fonts by Suresh Madhu

(require 'cl-pdf)

(load (merge-pathnames #p"pktype.lisp" *cl-pdf-srcdir*))

(defclass t3-char-metrics (PDF:char-metrics)
  ((h :accessor h :initarg :h) ; pixel height
   (w :accessor w :initarg :w) ; pixel width
   (raster :accessor raster :initarg :raster)
   (rastersize :accessor rastersize :initarg :rastersize)
   (rw :accessor rw :initarg :rw)))

(defclass t3-font-metrics (PDF:font-metrics)
  ((image-used :accessor image-used)
   (font-scale :accessor font-scale)))

(defun load-pk-font (path)
  (loop with pkfile = (pk:get-pkfile path)
        and t3fm =(make-instance 't3-font-metrics)
        for car below 256 ; todo: when used-char-p (car)
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
        (setf (PDF:font-name t3fm) (pk:pk-font-name pkfile))
        (setf (gethash (PDF:font-name t3fm) PDF::*font-metrics*) t3fm)
        (setf (PDF:encoding-scheme t3fm) nil) ; XXX
        (return t3fm)))

;;; CL-PDF protocols
;;; 
;;; 

(defmethod PDF::font-descriptor ((t3fm t3-font-metrics)) 
  (declare (ignore t3fm))
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

(defmethod %compute-encoding-differences ;XXX CL-PDF has Bug Regarding "/"
  ((t3fm t3-font-metrics) &aux (t3-first-char 0) (t3-last-char 255))
  ; for `encoding' differences. Encodes ALL characters
  (with-output-to-string (s)
    (loop with undef-p for i from t3-first-char to t3-last-char
	  do (if (= i t3-first-char) 
	       (cond (t ;XXX
		      (setf undef-p nil)
		      (format s "[~a/a~d" i i))
		     (t (setf undef-p t) 
			(format s "[~a/.notdef" i)))
	       (cond (t ; XXX
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
      ("/FirstChar" . 1)
      ("/LastChar" . 255)
      ("/Widths" .  ,(loop with widths = (make-array 256 :initial-element 0)
                           for cm being each hash-value  ; XXX - from e-v
                           in (pdf:characters t3fm) do
                           (setf (aref widths (PDF:code cm)) (PDF:width cm))
                           finally (return widths)))
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

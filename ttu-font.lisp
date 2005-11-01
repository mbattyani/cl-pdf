;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

;;; Support for TrueTypeUnicode fonts

(in-package pdf)

(defclass ttu-font-metrics (font-metrics)
  ((c2g :accessor c2g
	:initform (make-array 131072 :element-type 'character :initial-element #\Nul))
   (cid-widths :accessor cid-widths :initform (make-array 0 :adjustable t :fill-pointer 0))
   (pdf-widths :accessor pdf-widths :initform nil)
   (binary-data :accessor binary-data :initform nil)
   (min-code :accessor min-code :initform 0)
   (max-code :accessor max-code :initform 0)
   (length1 :accessor length1)))

(define-afm-section
    (ufm-font-metrics "FontMetrics") (stream font-metrics)
  (let ((ucode-to-char (make-hash-table :test #'equal)))
      (process-keywords
       (key "EndFontMetrics" ()
            (let ((min-code #xfffe)
                  (max-code 0)
                  (void-char (gethash "VoidCharacter" (characters font-metrics)))
                  encoding-vector pdf-widths)
              (iter (for (code char-metrics) in-hashtable ucode-to-char)
                    (when (> code max-code) (setf max-code code))
                    (when (< -1 code min-code) (setf min-code code)))
              (setf encoding-vector (make-array (1+ max-code) :initial-element void-char)
                    pdf-widths (make-array (1+ max-code) :initial-element 0))
              (iter (for (code char-metrics) in-hashtable ucode-to-char)
                    (when (<= min-code code max-code)
                      (setf (aref encoding-vector code) char-metrics
                            (aref pdf-widths code) (round (* 1000 (width char-metrics))))))
              (setf (min-code font-metrics) min-code
                    (max-code font-metrics) max-code
                    (encoding-vector font-metrics) encoding-vector
                    (pdf-widths font-metrics) pdf-widths
                    (encoding-scheme font-metrics) :unicode-encoding
                    (gethash (string-downcase (font-name font-metrics)) *font-metrics*) font-metrics
                    (leading font-metrics) (- 1 (descender font-metrics))
                    (italic-sin font-metrics) (sin (/ (* pi (italic-angle font-metrics)) -180))))
            (return-from ufm-font-metrics font-metrics))
       (key "StartCharMetrics" ()
            (setf (characters font-metrics)
                  (ufm-char-metrics
		   stream (char-width font-metrics)
		   (italic-sin font-metrics) font-metrics ucode-to-char))))))

(define-afm-section
    (ufm-char-metrics "CharMetrics")
    (stream default-width italic-sin font-metrics ucode-to-char)
  (let ((metrics (characters font-metrics))
	(encoding (encoding-vector font-metrics))
	char-metrics)
    (process-keywords
     (key "EndCharMetrics" () (return-from ufm-char-metrics metrics))
     (t (let ((width default-width)
	      (stroke-width 0)
	      (cid-width 0)
              (code -1)
	      (name nil)
	      (gid nil))
          (process-keywords-in-line
           (key "U" ((p-code integer)) (setq code p-code))
           (key "CH" ((p-code hex)) (setq code p-code))
           (key "WX" ((p-width number)) (setq cid-width p-width))
           (key "N" ((p-name name)) (setq name p-name))
	   (key "G" ((p-gid integer)) (setq gid p-gid)))
	  (when (and (<= 0 code #xfffe) gid)
	    (setf (aref (c2g font-metrics) (* 2 code))
		  (code-char (ldb (byte 8 8) gid))
		  (aref (c2g font-metrics) (+ (* 2 code) 1))
		  (code-char (ldb (byte 8 0) gid)))
	    (vector-push-extend code (cid-widths font-metrics))
	    (vector-push-extend (vector cid-width) (cid-widths font-metrics)))
          (unless cid-width
            (error "Width is not given for a character C ~D." code))
	  (setf char-metrics (gethash name metrics))
          (setf (code char-metrics) code)
	  (when code
	    (setf (gethash code ucode-to-char) char-metrics)))))))

(defun read-ufm-file (filename &optional (font-metrics-class 'ttu-font-metrics))
  (let ((afm-filemane (merge-pathnames (make-pathname :type "afm") filename))
        ufm)
    (with-open-file (s afm-filemane :direction :input :external-format +external-format+)
      (setf ufm (afm-font-metrics s font-metrics-class)))
    (with-open-file (s filename :direction :input :external-format +external-format+)
      (ufm-font-metrics s ufm))))

(defmethod font-type ((fm ttu-font-metrics))
  (declare (ignore fm))
  "Type0")

(defun load-ttu-font (ufm-file &optional ttf-file)
  (let ((ttufm (read-ufm-file ufm-file 'ttu-font-metrics)))
    (when ttf-file
      (with-open-file (in ttf-file :direction :input :element-type :default)
	(setf (length1 ttufm)
	      (file-length in)
	      (binary-data ttufm)
	      (make-array (length1 ttufm) :element-type '(unsigned-byte 8)))
	(read-sequence (binary-data ttufm) in)))
    ttufm))

;;; example: (pdf:load-ttu-font #P"/tmp/arial.ufm" #P"/tmp/arial.ttf")

(defmethod font-descriptor ((fm ttu-font-metrics)
			    &key (embed *embed-fonts*) (errorp t))
  (flet ((conv-dim (d) (round (* 1000 d))))
    (make-instance
     'indirect-object
     :content
     (make-instance
      'dictionary ; :obj-number 0 :no-link t
      :dict-values
      `(("/Type" . "/FontDescriptor")
	("/FontName"  . ,(add-/ (font-name fm)))
	("/Flags"
	 . ,(logior
	     (if (fixed-pitch-p fm) 1 0)
	     ;; 4 ? non-ascii present
	     32
	     (if (< 0 (italic-angle fm)) 64 0)))
	("/FontBBox" . ,(map 'vector #'conv-dim (font-bbox fm)))
	("/ItalicAngle" . ,(conv-dim (italic-angle fm)))
	("/Ascent" . ,(conv-dim (ascender fm)))
	("/Descent" . ,(conv-dim (descender fm)))
	("/CapHeight" . ,(conv-dim (cap-height fm)))
	("/XHeight" . ,(conv-dim (x-height fm)))
	("/StemV" . ,10)
	,@(when (and embed (binary-data fm))
	    `(("/FontFile2"
	       . ,(make-instance
		   'indirect-object
		   :content
		   (make-instance
		    'pdf-stream
		    :content (binary-data fm)
		    :no-compression t
		    :dict-values `(("/Length1" . ,(length1 fm)))))))))))))

(defclass cid-font ()
  ((base-font :accessor base-font :initarg :base-font)
   (descriptor :accessor descriptor :initarg :descriptor)
   (widths :accessor widths :initarg :widths)
   (c2g :accessor c2g :initarg :c2g)))

(defmethod make-dictionary ((font cid-font) &key &allow-other-keys)
  (make-instance
   'dictionary
   :dict-values
   `(("/Type" . "/Font")
     ("/Subtype" . "/CIDFontType2")
     ("/BaseFont" . ,(add-/ (base-font font)))
     ("/CIDSystemInfo"
      . ,(make-instance
	  'dictionary
	  :dict-values
	  `(("/Registry" . ,(pdf-string "Adobe"))
	    ("/Ordering" . ,(pdf-string "UCS"))
	    ("/Supplement" . 0))))
     ("/FontDescriptor" . ,(descriptor font))
     ("/W" . ,(widths font))
     ("/CIDToGIDMap"
      . ,(make-instance
	  'indirect-object
	  :content
	  (make-instance
	   'pdf-stream
	   :content (c2g font)
	   :no-compression t))))))

(defmethod make-dictionary
    ((fm ttu-font-metrics)
     &key font (encoding (encoding font)) (embed *embed-fonts*))
  (let* ((font-descriptor (font-descriptor fm :embed embed :errorp nil))
	 (cid-font (make-instance
		    'cid-font
		    :base-font (font-name fm)
		    :descriptor font-descriptor
		    :widths (cid-widths fm)
		    :c2g (c2g fm))))
    (make-instance
     'dictionary
     :dict-values
     `(("/Type" . "/Font")
       ("/Subtype" . ,(add-/ (font-type fm)))
       ("/BaseFont" . ,(add-/ (concatenate 'string (font-name fm) "-UCS")))
       ("/Encoding" . "/Identity-H")
       ("/DescendantFonts"
	. ,(vector
	    (make-instance
	     'indirect-object
	     :content (make-dictionary cid-font))))))))

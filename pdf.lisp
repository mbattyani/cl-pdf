;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

(defparameter *version* 2.03)

(defparameter +pdf-header+ "%PDF-1.4")

(defvar *document* nil)
(defvar *outlines-stack* nil)
(defvar *root-page* nil)
(defvar *page* nil)
(defvar *page-stream* nil)
(defvar *pdf-stream* nil)
(defvar *xrefs* nil)
(defvar *name-counter* 100)

(defun gen-name (prefix)
  (format nil "~a~d" prefix (incf *name-counter*)))

(defclass dictionary ()
  ((dict-values :accessor dict-values :initform nil :initarg :dict-values)))

(defun add-dict-value (dict name value)
  (push (cons name value)(dict-values dict)))

(defun get-dict-value (dict name)
  (cdr (assoc name (dict-values dict) :test #'string=)))

(defun change-dict-value (dict name value)
  (setf (cdr (assoc name (dict-values dict) :test #'string=)) value))

(defclass pdf-stream (dictionary)
  ((content :accessor content :initform "" :initarg :content)
   (no-compression :accessor no-compression :initarg :no-compression :initform nil)))

(defmethod initialize-instance :after ((obj pdf-stream) &rest init-options &key empty &allow-other-keys)
  (unless empty
    (add-dict-value obj "/Length" #'(lambda () (length (content obj))))))

(defclass document ()
  ((objects :accessor objects :initform nil)
   (root-page :accessor root-page :initform nil)
   (catalog :accessor catalog :initform nil)
   (outline-root :accessor outline-root :initform nil)
   (named-refs :accessor named-refs :initform (make-hash-table :test #'equal))
   (fonts :accessor fonts :initform '())
   (gstates :accessor gstates :initform '())
   (encodings :accessor encodings :initform '())
   (last-object-number :accessor last-object-number :initform 0)
   (docinfo :accessor docinfo :initform nil)
   (author :accessor author :initarg :author :initform nil)
   (title  :accessor title :initarg :title :initform nil)
   (keywords :accessor keywords :initarg :keywords :initform nil)
   (subject :accessor subject :initarg :subject :initform nil)))

(defmethod initialize-instance :after ((doc document) &rest init-options
				       &key empty author title subject keywords &allow-other-keys)
  (declare (ignore init-options))
  (unless empty
    (let ((*document* doc))
      (setf (objects doc) (make-array 10 :fill-pointer 0 :adjustable t))
      (setf (catalog doc) (make-instance 'indirect-object))
      (setf (docinfo doc) (make-instance 'indirect-object))
      (setf (root-page doc) (make-instance 'page-node))
      (setf (outline-root doc)(make-instance 'outline))
      (setf (content (catalog doc))
	    (make-instance 'dictionary
			   :dict-values `(("/Type" . "/Catalog")
					  ("/Pages" . ,(root-page doc)))))
      (setf (content (docinfo doc))
            (make-instance 'dictionary
                           :dict-values `(("/Creator" . ,(format nil "(cl-pdf version ~A)" *version*))
                                          ,@(when author `(("/Author" . ,(format nil "(~A)" author))))
                                          ,@(when title `(("/Title" . ,(format nil "(~A)" title))))
                                          ,@(when subject `(("/Subject" . ,(format nil "(~A)" subject))))
                                          ,@(when keywords `(("/Keywords" . ,(format nil "(~A)" keywords))))
                                          ("/CreationDate" .
                                            ,(multiple-value-bind (second minute hour date month year)
                                                 (get-decoded-time)
                                              (format nil "(D:~D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D)"
                                                      year month date hour minute second)))))))))

(defclass indirect-object ()
  ((obj-number :accessor obj-number :initform (incf (last-object-number *document*)) :initarg :obj-number)
   (gen-number :accessor gen-number :initform 0 :initarg :gen-number)
   (content :accessor content :initform nil :initarg :content)))

(defmethod initialize-instance :after ((obj indirect-object) &rest init-options
				       &key no-link &allow-other-keys)
  (unless no-link
    (vector-push-extend obj (objects *document*))))

(defclass object-ref ()
  ((obj-number :accessor obj-number :initform 0 :initarg :obj-number)
   (gen-number :accessor gen-number :initform 0 :initarg :gen-number)))

(defclass page-node (indirect-object)
  ((pages :accessor pages :initform (make-array 1 :fill-pointer 0 :adjustable t))))

(defmethod initialize-instance :after ((obj page-node) &rest init-options &key no-link &allow-other-keys)
  (when (and *root-page* (not no-link))
    (vector-push-extend obj (pages *root-page*)))
  (setf (content obj) (make-instance 'dictionary
		       :dict-values `(("/Type" . "/Pages")
				      ("/Count" . ,#'(lambda ()(length (pages obj))))
				      ,@(when *root-page* `(("/Parent" . ,*root-page*)))
				      ("/Kids" . ,(pages obj))))))

(defclass page (indirect-object)
  ((bounds :accessor bounds :initform #(0 0 595 841) :initarg :bounds)
   (resources :accessor resources :initform (make-instance 'dictionary))
   (fonts :accessor fonts :initform '())
   (font-objects :accessor font-objects :initform (make-instance 'dictionary))
   (gstates :accessor gstates :initform '())
   (gstate-objects :accessor gstate-objects :initform (make-instance 'dictionary))
   (xobjects :accessor xobjects :initform (make-instance 'dictionary))
   (annotations :accessor annotations :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (content-stream :accessor content-stream)
   ))

(defmethod initialize-instance :after ((page page) &rest init-options
				       &key no-link (rotate 0) &allow-other-keys)
  (when (and *root-page* (not no-link))
    (vector-push-extend page (pages *root-page*)))
  (setf (content-stream page) (make-instance 'pdf-stream))
  (add-dict-value (resources page) "/Font" (font-objects page))
  (add-dict-value (resources page) "/ExtGState" (gstate-objects page))
  (add-dict-value (resources page) "/ProcSet" "[ /PDF /Text ]")
  (add-dict-value (resources page) "/XObject" (xobjects page))
  (let ((content (make-instance 'indirect-object :content (content-stream page))))
    (setf (content page)
	  (make-instance 'dictionary
		:dict-values `(("/Type" . "/Page")
			       ("/Parent" ,*root-page*)
			       ("/MediaBox" . ,#'(lambda ()(bounds page)))
			       ("/Resources" . ,(resources page))
			       ("/Annots" ,(annotations page))
			       ("/Rotate" . ,rotate)
			       ("/Contents" . ,content))))))

(defclass named-reference ()
  ((name :accessor name :initarg :name)
   (reference :accessor reference :initform nil)))

(defun get-named-reference (name)
  (let ((ref (gethash name (named-refs *document*))))
    (unless ref
      (setf ref (make-instance 'named-reference :name name))
      (setf (gethash name (named-refs *document*)) ref))
    ref))

(defun register-named-reference (reference &optional (name (gen-name "R")))
  (setf (reference (get-named-reference name)) reference)
  name)

(defun register-page-reference (&optional (name (gen-name "R")))
  (register-named-reference (vector *page* "/Fit") name)
  name)

(defclass outline (indirect-object)
  ((title :accessor title :initarg :title :initform nil)
   (reference :accessor reference :initform nil :initarg :reference)
   (sub-levels :accessor sub-levels :initform nil)
   (prev   :accessor prev   :initform nil)
   (next   :accessor next   :initform nil)))

(defun enter-outline-level (title ref-name)
  (let ((outline (make-instance 'outline :title title :reference (get-named-reference ref-name)))
	(parent (first *outlines-stack*)))
    (setf (sub-levels parent)(nconc (sub-levels parent)(list outline)))
    (push outline *outlines-stack*)))

(defun close-outline-level ()
  (pop *outlines-stack*))

(defmacro with-outline-level ((title ref-name) &body body)
 `(unwind-protect
   (progn (enter-outline-level ,title ,ref-name)
	  ,@body)
   (close-outline-level)))

(defun compute-outline-tree (outlines &optional (parent nil))
  (loop for prev = nil then outline
	for outline in outlines
	do
	(when prev (setf (next prev) outline))
	(setf (prev outline) prev)
	(compute-outline-tree (sub-levels outline) outline))
  (loop for outline in outlines
	for sub-levels = (sub-levels outline)
	for first = (first sub-levels)
	for last = (first (last sub-levels))
	do
	(with-slots ((reference reference)(prev prev)(next next)) outline
	  (setf (content outline)
		(make-instance 'dictionary
  		   :dict-values `(,@(if parent `(("/Title" . ,(concatenate 'string "("
									   (title outline) ")"))
						 ("/Parent" . ,parent))
					'(("/Type" "/Outlines")))
				  ,@(when first `(("/First" . ,first)))
				  ,@(when last `(("/Last" . ,last)))
				  ,@(when prev `(("/Prev" . ,prev)))
				  ,@(when next `(("/Next" . ,next)))
				  ,@(when reference `(("/Dest" . ,reference)))
				  ("/Count" . "0")))))))

(defun process-outlines (document)
  (when (and (outline-root document) (sub-levels (outline-root document)))
    (compute-outline-tree (list (outline-root document)))
    (add-dict-value (content (catalog document)) "/Outlines" (outline-root document))))

(defmacro enforce-/ (&rest names)
  `(progn
    ,@(loop for name in names
	    collect `(unless (char= (schar ,name 0) #\/)
		      (setf ,name (concatenate 'string "/" ,name))))))
(defun add-/ (name)
  (concatenate 'string "/" name))
 
(defclass encoding-object (indirect-object)
  ((encoding :accessor encoding :initarg :encoding)))

(defmethod initialize-instance :after ((encoding-object encoding-object) &rest init-options &key
				       encoding &allow-other-keys)
  (setf (content encoding-object)
	(make-instance 'dictionary
		       :dict-values `(("/Type" . "/Encoding")
				      ("/Differences" . ,(compute-encoding-differences encoding))))))

(defun find-encoding-object (encoding)
  (let ((encoding-object (cdr (assoc encoding (encodings *document*)))))
    (unless encoding-object
      (setf encoding-object (make-instance 'encoding-object :encoding encoding))
      (push (cons encoding encoding-object) (encodings *document*)))
    encoding-object))

(defclass font-object (indirect-object)
  ((name :accessor name :initform (gen-name "/CLF") :initarg :name)
   (font :accessor font :initarg :font)))

(defmethod initialize-instance :after ((font-object font-object) &rest init-options &key
				       font &allow-other-keys)
  (let* ((encoding-object (if (standard-encoding (encoding font))
			      (concatenate 'string "/" (name (encoding font)))
			      (find-encoding-object (encoding font))))
	 (font-metrics (font-metrics font))
	 (font-descriptor (font-descriptor font-metrics)))
    (setf (content font-object)
	  (make-instance 'dictionary
             :dict-values `(("/Type" . "/Font")
			    ("/Subtype" . ,(add-/ (font-type font-metrics)))
			    ("/BaseFont" . ,(add-/ (font-name (font-metrics font))))
			    ,@(when font-descriptor `(("/FirstChar" . 0)))
			    ,@(when font-descriptor `(("/LastChar" . 255)))
			    ,@(when font-descriptor `(("/Widths" . ,(pdf-widths font))))
			    ,@(when font-descriptor `(("/FontDescriptor" . ,font-descriptor)))
			    ("/Encoding" . ,encoding-object))))))

(defun find-font-object (font)
  (let ((font-object (cdr (assoc font (fonts *document*))))) 
    (unless font-object
      (setf font-object (make-instance 'font-object :font font))
      (push (cons font font-object) (fonts *document*)))
    font-object))

(defun add-font-to-page (font)
  (let ((font-object (cdr (assoc font (fonts *page*)))))
    (unless font-object
      (setf font-object (find-font-object font))
      (push (cons font font-object) (fonts *page*))
      (add-dict-value (font-objects *page*) (name font-object) font-object))
    font-object))

(defclass gstate-object (indirect-object)
  ((name :accessor name :initform (gen-name "/GS") :initarg :name)))

(defmethod initialize-instance :after ((gstate-object gstate-object) &rest init-options &key gstate &allow-other-keys)
  (setf (content gstate-object) (make-instance 'dictionary :dict-values '(("/Type" . "/ExtGState"))))
  (loop for (name value) on gstate by #'cddr
	do (add-dict-value (content gstate-object) (format nil "/~a" name) value)))

(defun find-gstate-object (&rest gstate)
  (let ((gstate-object (cdr (assoc gstate (gstates *document*) :test #'equal))))
    (unless gstate-object
      (setf gstate-object (make-instance 'gstate-object :gstate gstate))
      (push (cons gstate gstate-object) (gstates *document*)))
    gstate-object))

(defun add-gstate-to-page (&rest gstate)
  (let ((gstate-object (cdr (assoc gstate (gstates *page*) :test #'equal))))
    (unless gstate-object
      (setf gstate-object (apply #'find-gstate-object gstate))
      (push (cons gstate gstate-object) (gstates *page*))
      (add-dict-value (gstate-objects *page*) (name gstate-object) gstate-object))
    gstate-object))

(defclass image (indirect-object)
  ((name  :accessor name  :initform (gen-name "/CLI") :initarg :name)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defmethod initialize-instance :after ((image image) &rest init-options &key
				       bits width height (filter "ASCIIHexDecode")
				       (color-space "DeviceRGB")(bits-per-color 8)
				       no-compression
				       &allow-other-keys)
  (enforce-/ filter color-space)
  (setf (content image)
	(make-instance 'pdf-stream
	       :no-compression no-compression
	       :dict-values `(("/Type" . "/XObject")("/Subtype" . "/Image")
			      ("/Width" . ,width)("/Height" . ,height)
			      ("/Filter" . ,filter)
			      ("/ColorSpace" . ,color-space)
			      ("/BitsPerComponent" . ,bits-per-color))))
  (setf (content (content image)) bits))

(defun add-images-to-page (&rest images)
  (dolist (image images)
    (add-dict-value (xobjects *page*) (name image) image)))

(defclass annotation (indirect-object)
  ())

(defmethod initialize-instance :after ((annotation annotation) &rest init-options &key
				       rect type (border #(0 0 0))
				       &allow-other-keys)
  (enforce-/ type)
  (vector-push-extend annotation (annotations *page*))
  (setf (content annotation)
	(make-instance 'dictionary
		       :dict-values `(("/Type" . "/Annot")("/Subtype" . ,type)
				      ("/Rect" . ,rect)("/Border" . ,border)))))

(defclass annotation2 (indirect-object)
  ())

(defmethod initialize-instance :after ((annotation annotation2) &rest init-options &key
				       rect type text
				       &allow-other-keys)
  (enforce-/ type)
  (vector-push-extend annotation (annotations *page*))
  (setf (content annotation)
	(make-instance 'dictionary
		       :dict-values `(("/Type" . "/Annot")("/Subtype" . "/Text")
				      ("/Rect" . ,rect)("/Contents" . ,text)))))

(defmethod write-object ((obj dictionary) &optional root-level)
  (declare (ignorable root-level))
  (write-string "<< " *pdf-stream*)
  (loop for (key . val) in (dict-values obj) do
	(write-string key *pdf-stream*)
	(write-char #\Space *pdf-stream*)
	(write-object val)
	(write-char #\Newline *pdf-stream*))
  (write-line " >>" *pdf-stream*))

(defmethod write-object ((obj pdf-stream) &optional root-level)
  (declare (ignorable root-level))
  (when (and *compress-streams* (not (no-compression obj))
	     (> (length (content obj)) *min-size-for-compression*))
    (setf (content obj) (compress-string (content obj)))
    (let ((filter (get-dict-value obj "/Filter")))
      (if filter
	  (change-dict-value obj "/Filter" (vector "/FlateDecode" filter))
	  (add-dict-value obj "/Filter" "/FlateDecode"))))
  (call-next-method)
  (write-line "stream" *pdf-stream*)
  #+pdf-binary
  (write-sequence (content obj) *pdf-stream*)
  #-pdf-binary
  (if (stringp (content obj))
      (write-sequence (content obj) *pdf-stream*)
      (loop for c across (content obj) do
	(write-char (code-char c) *pdf-stream*)))
  (write-char #\Newline *pdf-stream*)
  (write-line "endstream" *pdf-stream*))

(defmethod write-object ((obj object-ref) &optional root-level)
  (declare (ignorable root-level))
  (format *pdf-stream* "~d ~d R" (obj-number obj)(gen-number obj)))

(defmethod write-object ((obj indirect-object) &optional root-level)
  (if root-level
    (progn
      (vector-push-extend (format nil "~10,'0d ~5,'0d n "
				  (file-position *pdf-stream*)(gen-number obj))
			  *xrefs*)
      (format *pdf-stream* "~d ~d obj~%" (obj-number obj)(gen-number obj))
      (write-object (content obj))
      (write-string "endobj" *pdf-stream*)
      (write-char #\Newline *pdf-stream*))
    (format *pdf-stream* "~d ~d R" (obj-number obj)(gen-number obj))))

(defmethod write-object ((list list) &optional root-level)
  (declare (ignorable root-level))
  (dolist (obj list)
    (write-object obj)
    (write-char #\Space *pdf-stream*)))

(defmethod write-object ((obj string) &optional root-level)
  (declare (ignorable root-level))
  (write-string obj *pdf-stream*))

(defmethod write-object ((obj symbol) &optional root-level)
  (declare (ignorable root-level))
  (write-string (symbol-name obj) *pdf-stream*))

(defmethod write-object ((obj function) &optional root-level)
  (declare (ignorable root-level))
  (write-object (funcall obj)))

(defmethod write-object ((obj t) &optional root-level)
  (declare (ignorable root-level))
  (format *pdf-stream* "~a" obj))

(defmethod write-object ((array array) &optional root-level)
  (declare (ignorable root-level))
  (write-string "[ " *pdf-stream*)
  (loop for obj across array do
	(write-object obj)
	(write-char #\Space *pdf-stream*))
  (write-char #\] *pdf-stream*))

(defmethod write-object ((obj named-reference) &optional root-level)
  (declare (ignorable root-level))
  (write-object (reference obj)))

(defmethod write-document ((s stream) &optional (document *document*))
   (let ((*xrefs* (make-array 10 :adjustable t :fill-pointer 0))
	 startxref
	 (*pdf-stream* s))
     (with-standard-io-syntax
       (process-outlines document)
       (vector-push-extend "0000000000 65535 f " *xrefs*)
       (write-line +pdf-header+ s)
       (loop for obj across (objects document)
	     for first = t then nil
	     if obj do (write-object obj t)
	     else do (unless first (vector-push-extend "0000000000 00001 f " *xrefs*)))
       (setf startxref (file-position s))
       (format *pdf-stream* "xref~%0 ~d~%" (length *xrefs*))
       (loop for xref across *xrefs*
	     do (write-line xref s))
       (format s "trailer~%<< /Size ~d~%/Root " (length *xrefs*));(1- (length (objects document))))
       (write-object (catalog document))
       (format s "/Info ")
       (write-object (docinfo document))
       (format s "~%>>~%startxref~%~d~%%%EOF~%" startxref))))

(defmethod write-document ((filename pathname) &optional (document *document*))
   (with-open-file (s filename :direction :output :if-exists :supersede
		      :external-format +external-format+)
     (write-document s document)))

(defmethod write-document ((filename string) &optional (document *document*))
  (write-document (pathname filename) document))

(defmacro with-document ((&rest args) &body body)
  `(let* ((*document* (make-instance 'document ,@args))
	  (*outlines-stack* (list (outline-root *document*)))
	  (*root-page* (root-page *document*)))
    ,@body))

(defmacro with-page ((&rest args) &body body)
  `(let* ((*page* (make-instance 'page ,@args)))
    (with-standard-io-syntax
	(setf (content (content-stream *page*))
	 (with-output-to-string (*page-stream*)
	   ,@body)))
     t))


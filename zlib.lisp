;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package #:pdf)

;;; UFFI zlib

#+use-uffi-zlib
(defun load-zlib (&optional force)
  (when force (setf *zlib-loaded* nil))
  (unless *zlib-loaded*
    (let ((zlib-path (find-zlib-path)))
      (if zlib-path
	  (progn
	    (format t "~&;;; Loading ~s" zlib-path)
	    (uffi:load-foreign-library zlib-path
				       :module "zlib" 
				       :supporting-libraries '("c"))
	    (uffi:def-function ("compress" c-compress)
		((dest (* :unsigned-char))
		 (destlen (* :long))
		 (source :cstring)
		 (source-len :long))
	      :returning :int
	      :module "zlib")
	    (setf *zlib-loaded* t *compress-streams* t))
	  (progn
	    (warn "Unable to load zlib. Disabling compression.")
	    (setf *compress-streams* nil))))))

#+use-uffi-zlib
(defun compress-string (source)
  "Returns two values: array of bytes containing the compressed data
 and the numbe of compressed bytes"
  (let* ((sourcelen (length source))
	 (destsize (+ 12 (ceiling (* sourcelen 1.01))))
	 (dest (uffi:allocate-foreign-string destsize :unsigned t))
	 (destlen (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destlen :long) destsize)
    (uffi:with-cstring (source-native source)
      (let ((result (c-compress dest destlen source-native sourcelen))
	    (newdestlen (uffi:deref-pointer destlen :long)))
	(unwind-protect
	     (if (zerop result)
		 (values (uffi:convert-from-foreign-string 
			  dest
;			  :external-format '(:latin-1 :eol-style :lf)
			  :length newdestlen
			  :null-terminated-p nil)
			 newdestlen)
		 (error "zlib error, code ~D" result))
	  (progn
	    (uffi:free-foreign-object destlen)
	    (uffi:free-foreign-object dest)))))))

;;; ABCL zlib

#+use-abcl-zlib
(defun load-zlib (&optional force)
  (declare (ignore force))
  (setf *compress-streams* t))

#+use-abcl-zlib
(defun compress-string (string)
  (let* ((string-bytes
	  (java:jcall
	   (java:jmethod "java.lang.String" "getBytes" "java.lang.String") string "UTF-8"))
	 (out-array (java:jnew (java:jconstructor "java.io.ByteArrayOutputStream")))
	 (compresser (java:jnew (java:jconstructor "java.util.zip.Deflater" "int")
				(java:jfield "java.util.zip.Deflater" "BEST_COMPRESSION")))
	 (defl-out-stream
	  (java:jnew
	   (java:jconstructor
	    "java.util.zip.DeflaterOutputStream" "java.io.OutputStream" "java.util.zip.Deflater")
	   out-array compresser)))
    (java:jcall (java:jmethod "java.util.zip.Deflater" "setInput" "[B") compresser string-bytes)
      (java:jcall (java:jmethod "java.util.zip.DeflaterOutputStream" "close") defl-out-stream)
      (java:jcall (java:jmethod "java.io.ByteArrayOutputStream" "toString") out-array)))

;;; salza zlib

#+use-salza-zlib
(defun load-zlib (&optional force)
  (declare (ignore force))
  (setf *compress-streams* t))

#+use-salza-zlib
(defun compress-string (string)
  (let* ((input (if (stringp string)
		    (deflate::string-to-octets string 0 (length string))
		    string))
	 (buffer-size (min 8192 (* 2 (length string))))
         (zlib-buffer (make-array buffer-size :element-type 'salza::octet))
         (chunks ()))
    (flet ((zlib-callback (zlib-stream)
	     (push (subseq (salza::zlib-stream-buffer zlib-stream) 
			   0 (salza::zlib-stream-position zlib-stream)) chunks)
	     (setf (salza::zlib-stream-position zlib-stream) 0)))
      (let ((zlib-stream (salza::make-zlib-stream zlib-buffer :callback #'zlib-callback)))
        (salza::zlib-write-sequence input zlib-stream)
        (salza::finish-zlib-stream zlib-stream)
	(nreverse chunks)))))

;;; no-zlib
#+use-no-zlib
(defun load-zlib (&optional force)
  (declare (ignore force))
  (setf *compress-streams* nil))

#+use-no-zlib
(defun compress-string (string)
  string)

;;; load it!

(load-zlib)


;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package pdf)

;Adapted from an UFFI example

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

#|
Unfinished Work!
Using compression by block to avoid the huge cstring allocation of compress.
If somebody has some time to finish it...

(uffi:def-struct zstream
  (next-in (* :unsigned-char))
  (avail-in :unsigned-int)
  (total-in :unsigned-long)
  (next-out (* :unsigned-char))
  (avail-out :unsigned-int)
  (total-out :unsigned-long)
  (msg (* :unsigned-char))
  (state :long)
  (zalloc :long)
  (zfree :long)
  (opaque :long)
  (data-type :int)
  (alder :unsigned-long)
  (reserved :unsigned-long))

(defconstant +z-no-compression+ 0)
(defconstant +z-best-speed+ 1)
(defconstant +z-best-compression+ 9)
(defconstant +z-default-compression+ -1)

(uffi:def-function ("deflateInit" deflate-init)
    ((stream (* (:struct zstream)))
     (level :int))
  :returning :int
  :module "zlib")

(defconstant +z-no-flush+ 0)
(defconstant +z-sync-flush+ 2)
(defconstant +z-full-flush+ 3)
(defconstant +z-finish+ 4)

(uffi:def-function ("deflate" deflate)
    ((stream (* (:struct zstream)))
     (flush :int))
  :returning :int
  :module "zlib")

(uffi:def-function ("deflateEnd" deflate-end)
    ((stream (* (:struct zstream))))
  :returning :int
  :module "zlib")

(defvar *z-block-size* 4096)

|#

;;; 
;;; deflate-stream-interface.lisp
;;; 
;;; Created: 2005-03-14 by Zach Beane <xach@xach.com>
;;; 
;;; Copyright (c) 2005 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; $Id: deflate-stream-interface.lisp,v 1.10 2005/03/23 19:55:20 xach Exp $

(in-package :salza-deflate)

(defun start-deflate-stream (deflate-stream)
    ;; The block header
    ;; BFINAL is always set, since right now dynamic codes are not
    ;; supported so we never need to start a new block
    (write-bits 1 1 deflate-stream)
    (write-bits +deflate-fixed-tables-code+ 2 deflate-stream))

(defun finish-deflate-stream (deflate-stream)
  "Compress any pending input in the deflate-stream to its internal
buffer and add the end-of-block code."
  (finish-compress deflate-stream)
  (write-literal #x100 deflate-stream)
  (flush-deflate-stream deflate-stream)
  (funcall (deflate-stream-callback deflate-stream)
           deflate-stream))

(defun deflate-write-sequence (sequence deflate-stream
                               &key (start 0) (end (length sequence)))
  (compress-sequence sequence deflate-stream start end))


(defun deflate-write-byte (octet deflate-stream)
  "Finish any pending byte in the deflate stream and write BYTE as the
next octet to DEFLATE-STREAM."
  (flush-deflate-stream deflate-stream)
  (write-bits octet 8 deflate-stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (<= char-code-limit 256)
    (pushnew :octet-characters *features*)))

(defun string-to-octets (string start end)
  "Convert STRING to a sequence of octets, if possible."
  (declare (type string string)
           (type buffer-offset start end)
           (optimize (speed 3) (safety 0)))
  #+(and sbcl (not octet-characters))
  (sb-ext:string-to-octets string :external-format :iso-8859-1 :start start :end end)
  #+(and allegro (not octet-characters))
  (excl:string-to-octets string :start start :end end :null-terminate nil)
  #+(and clisp (not octet-characters))
  (ext:convert-string-to-bytes string custom:*default-file-encoding* :start start :end end)
  #+(or octet-characters lispworks)
  (let* ((length (- end start))
         (result (make-array length :element-type 'octet)))
    (loop for i fixnum from start below end
          for j fixnum from 0
          do (setf (aref result j) (char-code (aref string i))))
    result)
  #+(and (not octet-characters) (not (or sbcl allegro clisp lispworks)))
  (error "Do not know how to convert a string to octets."))

(defun deflate-write-string (string deflate-stream
                             &key (start 0) (end (length string)))
  (deflate-write-sequence (string-to-octets string start end) deflate-stream))


;;; CRC32

(defun crc32-table ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((table (make-array 512 :element-type '(unsigned-byte 16))))
    (dotimes (n 256 table)
      (let ((c n))
        (declare (type (unsigned-byte 32) c))
        (dotimes (k 8)
          (if (logbitp 0 c)
              (setf c (logxor #xEDB88320 (ash c -1)))
              (setf c (ash c -1)))
          (setf (aref table (ash n 1)) (ldb (byte 16 16) c)
                (aref table (1+ (ash n 1))) (ldb (byte 16 0) c)))))))

(let ((table (crc32-table)))
  (defun crc32 (high low buf end)
    (declare (optimize (speed 3) (safety 0) #+lispworks (hcl:fixnum-safety 0))
             (type (unsigned-byte 16) high low)
             (type octet-vector buf)
             (type (simple-array (unsigned-byte 16) (*)) table)
             (fixnum end))
    (let ((len end))
      (declare (fixnum len))
      (dotimes (n len (values high low))
        (declare (fixnum n))
        (let ((index (logxor (logand low #xFF) (aref buf n))))
          (declare (type (integer 0 255) index))
          (let ((high-index (ash index 1))
                (low-index (1+ (ash index 1))))
            (declare (type (integer 0 511) high-index low-index))
            (let ((t-high (aref table high-index))
                  (t-low (aref table low-index)))
              (declare (type (unsigned-byte 16) t-high t-low))
              (setf low (logxor (ash (logand high #xFF) 8)
                                (ash low -8)
                                t-low))
              (setf high (logxor (ash high -8) t-high)))))))))


(defun crc32-sequence (sequence &key (end (length sequence)))
  "Return an octet sequence of four bytes containing the
crc32 checksum of SEQUENCE."
  (multiple-value-bind (high low)
      (crc32 #xFFFF #xFFFF sequence end)
    (setf high (logxor #xFFFF high)
          low (logxor #xFFFF low))
    (make-array 4 :element-type 'octet
                :initial-contents (list (ldb (byte 8 8) high)
                                        (ldb (byte 8 0) high)
                                        (ldb (byte 8 8) low)
                                        (ldb (byte 8 0) low)))))

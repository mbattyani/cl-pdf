;;; 
;;; zlib.lisp
;;; 
;;; Created: 2005-03-12 by Zach Beane <xach@xach.com>
;;; 
;;; zlib encapsulation of a deflate output stream.
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
;;; $Id: zlib.lisp,v 1.4 2005/03/16 02:29:56 xach Exp $


(in-package :salza)


;;; Adler checksum

(defconstant +adler32-base+ 65521)

(defun adler32 (adler-high adler-low buf start end)
  (declare (type (unsigned-byte 16) adler-high adler-low)
           (type (simple-array (unsigned-byte 8)) buf)
           (type buffer-offset start end)
           (optimize (speed 3) (safety 0)))
  (let ((s1 adler-low)
        (s2 adler-high))
    (declare (type (unsigned-byte 32) s1 s2))
    (dotimes (i (- end start) (values s2 s1))
      (declare (fixnum i))
      (setf s1 (mod (+ s1 (aref buf (+ start i))) +adler32-base+))
      (setf s2 (mod (+ s2 s1) +adler32-base+)))))


;;; Conditions

(define-condition zlib-buffer-full ()
  ((buffer :initarg :buffer :reader zlib-buffer-full-buffer)
   (zlib-stream :initarg :zlib-stream :reader zlib-buffer-full-zlib-stream))
  (:documentation "This condition is signalled in a continuable error
when the buffer backing the deflate-stream has reached the end. User code
should handle this condition, do something appropriate with the
buffer, and reset the zlib-stream position."))

(defstruct (zlib-stream
             (:constructor %make-zlib-stream (deflate-stream)))
  deflate-stream
  (adler-high 0 :type (unsigned-byte 16))
  (adler-low 1 :type (unsigned-byte 16)))

(defun zlib-stream-position (zlib-stream)
  (deflate-stream-pos (zlib-stream-deflate-stream zlib-stream)))

(defun (setf zlib-stream-position) (new-value zlib-stream)
  (setf (deflate-stream-pos (zlib-stream-deflate-stream zlib-stream))
        new-value))

(defun zlib-stream-buffer (zlib-stream)
  (deflate-stream-buffer (zlib-stream-deflate-stream zlib-stream)))

(defmethod print-object ((object zlib-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D" (zlib-stream-position object))))

(defconstant +zlib-compression-method+ 8
  "The DEFLATE method code, sent as the high nybble of the first byte
of the stream.")

(defconstant +zlib-compression-info+ 0
  "From the spec: For CM = 8, CINFO is the base-2 logarithm of the
LZ77 window size, minus eight (CINFO=7 indicates a 32K window size).")

(defconstant +zlib-compression-level+ 2
  "The default compression level code.")

(defconstant +zlib-preset-dictionary-flag+ 0
  "No preset dictionary.")

(defun write-zlib-stream-header (deflate-stream)
  (let* ((cm+flags (logior (ash +zlib-compression-info+ 12)
                           (ash +zlib-compression-method+ 8)
                           (ash +zlib-compression-level+ 6)))
         (check (logandc2 31 (mod cm+flags 31))))
    (setf cm+flags (logior cm+flags check))
    (deflate-write-byte (ash cm+flags -8) deflate-stream)
    (deflate-write-byte cm+flags deflate-stream)))


(defun make-zlib-stream (buffer &optional (start 0) (end (length buffer)))
  "Create and return a zlib-stream. START is the first offset in
BUFFER to which compressed data is written; END is the offset after
the last writable byte in BUFFER."
  (check-type buffer (simple-array octet))
  (let* ((deflate-stream (make-deflate-stream buffer start end)))
    (write-zlib-stream-header deflate-stream)
    (start-deflate-stream deflate-stream)
    (%make-zlib-stream deflate-stream)))

(defun zlib-write-sequence (sequence zlib-stream
                            &key (start 0) (end (length sequence)))
  "Compress SEQUENCE and write them to ZLIB-STREAM. May signal a
continuable error of type ZLIB-BUFFER-FULL."
  (multiple-value-bind (adler-high adler-low)
      (adler32 (zlib-stream-adler-high zlib-stream)
               (zlib-stream-adler-low zlib-stream)
               sequence start end)
    (setf (zlib-stream-adler-high zlib-stream) adler-high
          (zlib-stream-adler-low zlib-stream) adler-low)
    (let ((deflate-stream (zlib-stream-deflate-stream zlib-stream)))
      (handler-bind
          ((deflate-stream-buffer-full
            (lambda (c)
              (declare (ignore c))
              (cerror "Retry write"
                      'zlib-buffer-full
                      :zlib-stream zlib-stream
                      :buffer (deflate-stream-buffer deflate-stream))
              (continue))))
        (deflate-write-sequence sequence deflate-stream
          :start start :end end)))))

(defun zlib-write-string (string zlib-stream)
  "Write the octet representation of STRING to ZLIB-STREAM."
  (deflate-write-string string (zlib-stream-deflate-stream zlib-stream)))

(defun finish-zlib-stream (zlib-stream)
  "Conclude output to the zlib-stream, writing the terminating code
for the block to the buffer and and appending the four adler32
checksum bytes. May signal a continuable error of type
ZLIB-BUFFER-FULL."
  (let ((deflate-stream (zlib-stream-deflate-stream zlib-stream)))
    (handler-bind
        ((deflate-stream-buffer-full
          (lambda (c)
            (declare (ignore c))
            (cerror "Retry write"
                    'zlib-buffer-full
                    :zlib-stream zlib-stream
                    :buffer (deflate-stream-buffer deflate-stream))
            (continue))))
      (finish-deflate-stream deflate-stream)
      (let ((high (zlib-stream-adler-high zlib-stream))
            (low (zlib-stream-adler-low zlib-stream)))
        (deflate-write-byte (ldb (byte 8 8) high) deflate-stream)
        (deflate-write-byte (ldb (byte 8 0) high) deflate-stream)
        (deflate-write-byte (ldb (byte 8 8) low) deflate-stream)
        (deflate-write-byte (ldb (byte 8 0) low) deflate-stream)))))




;;; Convenience functions

(defun compress (input)
  "Return an octet sequence containing the bytes of INPUT compressed
to the zlib format."
  (check-type input (simple-array (unsigned-byte 8)))
  (let* ((max-size (+ 12 (ceiling (* (length input) 1.01))))
	 (buffer-size (min 8192 max-size))
         (buffer (make-array buffer-size :element-type 'octet))
         (zlib-stream (make-zlib-stream buffer))
         (output-end 0)
         (output (make-array max-size :adjustable nil :initial-element 0 :element-type 'octet)))
    (flet ((flush-output-buffer (&optional c)
             (declare (ignore c))
	     (replace output buffer
		      :start1 output-end
		      :end2 (zlib-stream-position zlib-stream))
             (incf output-end (zlib-stream-position zlib-stream))
             (setf (zlib-stream-position zlib-stream) 0)
             (continue)))
      (handler-bind
          ((zlib-buffer-full #'flush-output-buffer))
        (zlib-write-sequence input zlib-stream)
        (finish-zlib-stream zlib-stream)
        (flush-output-buffer)
        (subseq output 0 output-end)))))

(defun compress-string (string)
  "Return the zlib compressed sequence of STRING's octet sequence
representation."
  (compress (deflate::string-to-octets string 0 (length string))))


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
;;; $Id: zlib.lisp,v 1.15 2005/04/05 19:53:27 xach Exp $


(in-package :salza)


;;; Adler checksum

(defconstant +adler32-base+ 65521)

(defun adler32 (adler-high adler-low buf start end)
  (declare (optimize (speed 3) (safety 0) (debug 0)
                     #+lispworks (hcl:fixnum-safety 0))
           (type buffer-offset start end)
           (type fixnum adler-high adler-low)
           (type octet-vector buf))
  (cond ((> start end)
         (error "Invalid start and end values (start must be <= end)"))
        ((= start end)
         (values adler-high adler-low))
        (t
         (let ((length (- end start))
               (i 0)
               (k 0)
               (s1 adler-low)
               (s2 adler-high))
           (declare (type buffer-offset i k length)
                    (type fixnum s1 s2))
           (tagbody
            loop
              (setf k (min 16 length))
              (decf length k)
            sum
              (setf s1 (+ (aref buf (+ start i)) s1))
              (setf s2 (+ s1 s2))
              (decf k)
              (incf i)
              (unless (zerop k)
                (go sum))
              (setf s1 (mod s1 +adler32-base+))
              (setf s2 (mod s2 +adler32-base+))
              (unless (zerop length)
                (go loop)))
           (values s2 s1)))))


;;; Conditions

(define-condition zlib-buffer-full ()
  ((zlib-stream :initarg :zlib-stream :reader zlib-buffer-full-zlib-stream))
  (:documentation "When no callback is provided in MAKE-ZLIB-STREAM,
this condition is signalled in a continuable error when the buffer
backing the zlib-stream has reached the end. It is also called at the
end of output in FINISH-ZLIB-STREAM. User code should handle this
condition, do something appropriate with the buffer, and reset the
zlib-stream position."))

(defstruct (zlib-stream
             (:constructor %make-zlib-stream (deflate-stream callback)))
  deflate-stream
  callback
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


(defun default-callback (zlib-stream)
  (cerror "Retry write"
          'zlib-buffer-full
          :zlib-stream zlib-stream))

(defun make-zlib-stream (buffer &key (start 0) end callback)
  "Create and return a zlib-stream. START is the first offset in
BUFFER to which compressed data is written. END is the offset after
the last writable byte in BUFFER (if not provided, the length of
BUFFER is used). CALLBACK is a function to be called when BUFFER is
full and when the zlib-stream is finished. If no callback is provided,
a function that raises a continuable ZLIB-BUFFER-FULL error is used."
  (check-type buffer (simple-array octet))
  (setf end (or end (length buffer)))
  ;; XXX This seems a little silly, but it captures the binding of
  ;; zlib-stream in the lambda
  (let* ((zlib-stream nil)
         (zlib-callback (or callback #'default-callback))
         (deflate-callback (lambda (deflate-stream)
                             (declare (ignore deflate-stream))
                             (funcall zlib-callback zlib-stream)))
         (deflate-stream (make-deflate-stream buffer
                                              :pos start
                                              :end end
                                              :callback deflate-callback)))
    (setf zlib-stream (%make-zlib-stream deflate-stream callback))
    (write-zlib-stream-header deflate-stream)
    (start-deflate-stream deflate-stream)
    zlib-stream))

(defun zlib-write-sequence (sequence zlib-stream
                            &key (start 0) (end (length sequence)))
  "Compress SEQUENCE and write them to ZLIB-STREAM."
  (multiple-value-bind (adler-high adler-low)
      (adler32 (zlib-stream-adler-high zlib-stream)
               (zlib-stream-adler-low zlib-stream)
               sequence start end)
    (setf (zlib-stream-adler-high zlib-stream) adler-high
          (zlib-stream-adler-low zlib-stream) adler-low)
    (deflate-write-sequence sequence (zlib-stream-deflate-stream zlib-stream)
                            :start start :end end)))

(defun zlib-write-string (string zlib-stream)
  "Write the octet representation of STRING to ZLIB-STREAM."
  (deflate-write-string string (zlib-stream-deflate-stream zlib-stream)))

(defun finish-zlib-stream (zlib-stream)
  "Conclude output to the zlib-stream, writing the terminating code
for the block to the buffer and and appending the four adler32
checksum bytes. Call ZLIB-STREAM's callback as the final step."
  (let ((deflate-stream (zlib-stream-deflate-stream zlib-stream)))
    (finish-deflate-stream deflate-stream)
    (let ((high (zlib-stream-adler-high zlib-stream))
          (low (zlib-stream-adler-low zlib-stream)))
      (deflate-write-byte (ldb (byte 8 8) high) deflate-stream)
      (deflate-write-byte (ldb (byte 8 0) high) deflate-stream)
      (deflate-write-byte (ldb (byte 8 8) low) deflate-stream)
      (deflate-write-byte (ldb (byte 8 0) low) deflate-stream)
      (funcall (zlib-stream-callback zlib-stream) zlib-stream))))



;;; Convenience functions

(defun compress-sequence (input)
  "Return an octet sequence containing the bytes of INPUT compressed
to the zlib format."
  (check-type input octet-vector)
  (let* ((buffer-size 8192)
         (zlib-buffer (make-array buffer-size :element-type 'octet))
         (offset 0)
         (output (make-array buffer-size :adjustable t :initial-element 0)))
    (flet ((zlib-callback (zlib-stream)
             (let ((pos (zlib-stream-position zlib-stream)))
               (adjust-array output (+ offset pos))
               (replace output (zlib-stream-buffer zlib-stream)
                        :start1 offset
                        :end2 pos)
               (incf offset pos)
               (setf (zlib-stream-position zlib-stream) 0))))
      (let ((zlib-stream (make-zlib-stream zlib-buffer
                                           :callback #'zlib-callback)))
        (zlib-write-sequence input zlib-stream)
        (finish-zlib-stream zlib-stream)
        output))))

(defun compress-string (string)
  "Return the zlib compressed sequence of STRING's octet sequence
representation."
  (compress-sequence (deflate::string-to-octets string 0 (length string))))

(defun compress-stream (input output)
  "Read input from the stream INPUT and write it in ZLIB format to the
stream OUTPUT. Both streams must have element-types of '(unsigned-byte
8)."
  (flet ((flush-stream (zlib-stream)
           (write-sequence (zlib-stream-buffer zlib-stream) output
                           :end (zlib-stream-position zlib-stream))
           (setf (zlib-stream-position zlib-stream) 0)))
    (let* ((input-buffer (make-array 8192 :element-type 'octet))
           (output-buffer (make-array 8192 :element-type 'octet))
           (zlib-stream (make-zlib-stream output-buffer
                                          :callback #'flush-stream)))
      (loop
       (let ((end (read-sequence input-buffer input)))
         (zlib-write-sequence input-buffer zlib-stream :end end)
         (when (zerop end)
           (finish-zlib-stream zlib-stream)
           (return)))))))

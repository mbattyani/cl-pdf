;;; 
;;; gzip.lisp
;;; 
;;; Created: 2005-03-14 by Zach Beane <xach@xach.com>
;;; 
;;; An example use of the salza DEFLATE interface functions.
;;; 
;;; 
;;; $Id: gzip.lisp,v 1.2 2005/03/15 05:22:12 xach Exp $

(defpackage :gzip
  (:use :cl :salza-deflate)
  (:export :gzip))

(in-package :gzip)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +gzip-signature+
    (if (boundp '+gzip-signature+)
        (symbol-value '+gzip-signature+)
        #(#x1F #x8B))))

(defconstant +gzip-deflate-compression+ 8)
(defconstant +gzip-flags+ 0)

(defconstant +gzip-fast-compression+ 4)
(defconstant +gzip-unix-os+ 3)

(defun write-gzip-header (stream)
  (write-sequence +gzip-signature+ stream)
  (write-byte +gzip-deflate-compression+ stream)
  (write-byte +gzip-flags+ stream)
  ;; mtime
  (write-sequence #(0 0 0 0) stream)
  (write-byte +gzip-fast-compression+ stream)
  (write-byte +gzip-unix-os+ stream))

(defun write-gzip-data (input output)
  (let* ((buffer (make-array 8192 :element-type '(unsigned-byte 8)))
         (compress-buffer (make-array 8192
                                      :element-type '(unsigned-byte 8)))
         (deflate-stream (make-deflate-stream compress-buffer))
         (crc-high #xFFFF)
         (crc-low #xFFFF)
         (size 0))
    (flet ((write-compress-buffer (&optional c)
             (declare (ignore c))
             (write-sequence compress-buffer output
                             :end (deflate-stream-pos deflate-stream))
             (setf (deflate-stream-pos deflate-stream) 0)
             (continue))
           (write-uint32 (value)
             (write-byte (ldb (byte 8 0) value) output)
             (write-byte (ldb (byte 8 8) value) output)
             (write-byte (ldb (byte 8 16) value) output)
             (write-byte (ldb (byte 8 24) value) output)))
      (handler-bind
          ((deflate-stream-buffer-full #'write-compress-buffer))
        (start-deflate-stream deflate-stream)
        (loop
         (let ((read (read-sequence buffer input)))
           (incf size read)
           (deflate-write-sequence buffer deflate-stream :end read)
           (multiple-value-setq (crc-high crc-low)
             (crc32 crc-high crc-low buffer :end read))
           (when (< read 8192)
             (finish-deflate-stream deflate-stream)
             (write-compress-buffer)
             (setf crc-high (logxor crc-high #xFFFF)
                   crc-low (logxor crc-low #xFFFF))
             (write-uint32 (logior (ash crc-high 16) crc-low))
             (write-uint32 size)
             (return))))))))

(defun gzip (input-file output-file)
  (with-open-file (input input-file
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (with-open-file (output output-file
                     :direction :output
                     :if-exists :supersede
                     :element-type '(unsigned-byte 8))
      (write-gzip-header output)
      (write-gzip-data input output)
      (truename output))))
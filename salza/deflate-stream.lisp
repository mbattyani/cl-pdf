;;; 
;;; deflate-stream.lisp
;;; 
;;; Created: 2005-03-12 by Zach Beane <xach@xach.com>
;;; 
;;; An interface to the DEFLATE data compression format. See the
;;; "packages.lisp" file for the public interface.
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
;;; $Id: deflate-stream.lisp,v 1.2 2005/03/16 02:29:56 xach Exp $

(in-package :salza-deflate)

(declaim (inline deflate-stream-buffer))
(declaim (inline deflate-stream-pos))
(declaim (inline deflate-stream-byte))
(declaim (inline deflate-stream-end))
(declaim (inline deflate-stream-bits-left))

(define-condition deflate-stream-buffer-full ()
  ((deflate-stream :initarg :deflate-stream :reader deflate-stream-buffer-full-deflate-stream)))

(defvar *compressor-buffer-size* 65536)

(defstruct (deflate-stream
             (:constructor
              make-deflate-stream (buffer &optional (pos 0) (end (length buffer)))))
  buffer
  (pos 0 :type buffer-offset)
  (end 0 :type buffer-offset)
  (byte 0 :type octet)
  (bits-left 8 :type octet)
  (compress-buffer (make-array *compressor-buffer-size* :element-type 'octet)
                   :type (simple-array octet))
  (compress-pos 0 :type buffer-offset)
  (compress-positions (make-hash-table) :type hash-table))


(defmethod print-object ((object deflate-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D/~D ~D ~D/8"
            (deflate-stream-pos object)
            (deflate-stream-end object)
            (deflate-stream-byte object)
            (deflate-stream-bits-left object))))
            
(defun write-bits (code length deflate-stream)
  "Save LENGTH low bits of CODE to the buffer of DEFLATE-STREAM. If the end
of the deflate-stream buffer is reached, raise a continuable error of type
DEFLATE-STREAM-BUFFER-FULL."
  (declare (type (unsigned-byte 24) code)
           (type (integer 0 24) length)
           (type deflate-stream deflate-stream)
           (optimize (speed 3) (safety 0)))
  (symbol-macrolet
      ((byte (deflate-stream-byte deflate-stream))
       (bits-left (deflate-stream-bits-left deflate-stream))
       (pos (deflate-stream-pos deflate-stream))
       (buffer (deflate-stream-buffer deflate-stream))
       (end (deflate-stream-end deflate-stream)))
    (declare (type (simple-array octet) buffer)
             (type (integer 0 8) bits-left)
             (type buffer-offset pos end))
    (flet ((output-byte ()
             (setf (aref buffer pos) byte)
             (incf pos)
             (loop
              (when (< pos end) (return))
               (cerror "Resume output"
                       'deflate-stream-buffer-full
                       :deflate-stream deflate-stream))))
      (tagbody
       loop
         (cond ((> length bits-left)
                (setf byte
                      (logior byte
                              (ash (ldb (byte bits-left 0) code)
                                   (- 8 bits-left))))
                (output-byte)
                (decf length bits-left)
                (setf code (ash code (- bits-left)))
                (setf bits-left 8
                      byte 0)
                (go loop))
               ((= length bits-left)
                (setf byte (logior byte
                                   (ash (ldb (byte bits-left 0) code)
                                        (- 8 bits-left))))
                (output-byte)
                (setf bits-left 8
                      byte 0))
               (t
                (setf byte (logior byte (ash code (- 8 bits-left))))
                (decf bits-left length))))
      (setf (deflate-stream-bits-left deflate-stream) bits-left
            (deflate-stream-byte deflate-stream) byte))))

(defconstant +deflate-fixed-tables-code+ #b01)

(defun write-block-header (deflate-stream)
    ;; The block header
    ;; BFINAL is always set, since right now dynamic codes are not
    ;; supported so we never need to start a new block
    (write-bits 1 1 deflate-stream)
    (write-bits +deflate-fixed-tables-code+ 2 deflate-stream))

(defun flush-deflate-stream (deflate-stream)
  "If there is a pending unwritten byte in the deflate-stream, save it and
advance the stream position."
  (when (< (deflate-stream-bits-left deflate-stream) 8)
    (setf (aref (deflate-stream-buffer deflate-stream) (deflate-stream-pos deflate-stream))
          (deflate-stream-byte deflate-stream))
    (setf (deflate-stream-byte deflate-stream) 0
          (deflate-stream-bits-left deflate-stream) 8)
    (incf (deflate-stream-pos deflate-stream))))

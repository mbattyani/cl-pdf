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
;;; $Id: deflate-stream.lisp,v 1.11 2005/04/01 21:57:24 xach Exp $

(in-package :salza-deflate)

#-lispworks	; Supress lots of warning - these are special accessors on LW
(declaim (inline deflate-stream-buffer
                 deflate-stream-pos
                 deflate-stream-byte
                 deflate-stream-end
                 deflate-stream-bits-left))

(define-condition deflate-stream-buffer-full ()
  ((deflate-stream :initarg :deflate-stream :reader deflate-stream-buffer-full-deflate-stream)))

(defvar *compressor-buffer-size* 65536)

(defstruct (deflate-stream
             (:constructor
              %make-deflate-stream (buffer pos end callback)))
  (buffer nil :type (or octet-vector null))
  (callback nil :type (or function null))
  (pos 0 :type buffer-offset)
  (end 0 :type buffer-offset)
  (byte 0 :type octet)
  (bits-left 8 :type octet)
  (compress-buffer (make-array *compressor-buffer-size* :element-type 'octet)
                   :type octet-vector)
  (compress-pos 0 :type buffer-offset)
  (compress-positions (make-fixhash-table)))

(defun default-callback (deflate-stream)
  (cerror "Resume output"
          'deflate-stream-buffer-full
          :deflate-stream deflate-stream))

(defun make-deflate-stream (buffer
                            &key (pos 0) end (callback #'default-callback))
  (check-type buffer octet-vector)
  (initialize-huffman)
  (setf end (or end (length buffer)))
  (%make-deflate-stream buffer pos end callback))

(defmethod print-object ((object deflate-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D/~D ~D ~D/8"
            (deflate-stream-pos object)
            (deflate-stream-end object)
            (deflate-stream-byte object)
            (deflate-stream-bits-left object))))

;;; DI 2005-Apr-11: It seems CODE < 262136 (#x3FFF8)
(declaim (ftype (function (fixnum (integer 0 24) t) buffer-offset) write-bits))
(defun write-bits (code length deflate-stream)
  "Save LENGTH low bits of CODE to the buffer of DEFLATE-STREAM. If the end
of the deflate-stream buffer is reached, raise a continuable error of type
DEFLATE-STREAM-BUFFER-FULL."
  (declare (fixnum code)	;(type (unsigned-byte 24) code)
           (type (integer 0 24) length)
           (type deflate-stream deflate-stream)
           (optimize (speed 3) (safety 0) (debug 0)
                     #+lispworks (hcl:fixnum-safety 0)))
  (let ((byte (deflate-stream-byte deflate-stream))
        (bits-left (deflate-stream-bits-left deflate-stream))
        (pos (deflate-stream-pos deflate-stream))
        (buffer (deflate-stream-buffer deflate-stream))
        (end (deflate-stream-end deflate-stream)))
    (declare (type octet-vector buffer)
             (type (integer 0 8) bits-left)
             (type octet byte)
             (type buffer-offset pos end))
    (flet ((output-byte ()
             (setf (aref buffer pos) byte)
             (incf pos)
             ;(if (< pos end)
             ;    (values)
             ;    (cerror "Resume output" 'deflate-stream-buffer-full
             ;            :deflate-stream deflate-stream))))
             (loop
              (when (< pos end) (return))
              (setf (deflate-stream-pos deflate-stream) pos)
              (funcall (the function (deflate-stream-callback deflate-stream))
                       deflate-stream)
              (setf buffer (deflate-stream-buffer deflate-stream)
                    pos (deflate-stream-pos deflate-stream)))))
      (declare (inline output-byte))
      (tagbody
       loop
         ;(setf byte (logior byte (logand #xFF (ash code (- 8 bits-left)))))
         (setf byte (the octet (logior byte (the fixnum
                                       (logand #xFF (the fixnum
                                               (ash code (the fixnum
                                                    (- 8 bits-left)))))))))
         (cond ((> length bits-left)
                (output-byte)
                (decf length bits-left)
                (setf code (the fixnum (ash code (the fixnum (- bits-left))))
                      bits-left 8
                      byte 0)
                (go loop))
               ((= length bits-left)
                (output-byte)
                (setf bits-left 8
                      byte 0))
               (t
                (decf bits-left length))))
      (setf (deflate-stream-bits-left deflate-stream) bits-left
            (deflate-stream-byte deflate-stream) byte
            (deflate-stream-pos deflate-stream) pos))))

(defconstant +deflate-fixed-tables-code+ #b01)

#| DI 2005-Apr-11 : Identical to start-deflate-stream!
(defun write-block-header (deflate-stream)
    ;; The block header
    ;; BFINAL is always set, since right now dynamic codes are not
    ;; supported so we never need to start a new block
    (write-bits 1 1 deflate-stream)
    (write-bits +deflate-fixed-tables-code+ 2 deflate-stream))|#

(defun flush-deflate-stream (deflate-stream)
  "If there is a pending unwritten byte in the deflate-stream, save it and
advance the stream position."
  (when (< (deflate-stream-bits-left deflate-stream) 8)
    (setf (aref (deflate-stream-buffer deflate-stream) (deflate-stream-pos deflate-stream))
          (deflate-stream-byte deflate-stream))
    (setf (deflate-stream-byte deflate-stream) 0
          (deflate-stream-bits-left deflate-stream) 8)
    (incf (deflate-stream-pos deflate-stream))
    (loop
     (when (< (deflate-stream-pos deflate-stream)
              (deflate-stream-end deflate-stream))
       (return)))))


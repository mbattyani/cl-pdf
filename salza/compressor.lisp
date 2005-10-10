;;; 
;;; compressor.lisp
;;; 
;;; Created: 2005-03-12 by Zach Beane <xach@xach.com>
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
;;; $Id: compressor.lisp,v 1.13 2005/04/01 21:59:25 xach Exp $

(in-package :salza-deflate)

(defun compress (deflate-stream)
  "Compress pending input in DEFLATE-STREAM to its output buffer."
  (declare (optimize (speed 3) (safety 0) (debug 0)
                     #+lw-int32 (hcl:fixnum-safety 0) #+lw-int32 (float 0))
           (type deflate-stream deflate-stream))
  (let* ((input (deflate-stream-compress-buffer deflate-stream))
         (positions (deflate-stream-compress-positions deflate-stream))
         (i 0)
         (j 0)
         (length 0)
         (distance 0) 
         (end (min (deflate-stream-compress-pos deflate-stream)
                   (length input)))
         (trigram #.+ub24-0+))
    (declare (type octet-vector input)
             (type buffer-offset i j end)
             (type (integer 0 32768) distance)
             (type (integer 0 258) length)
             (type ub24 trigram))
    (labels ((save-trigram ()
               (setf (getfixhash (ub24-fixhash trigram) positions) i))
             (shift-in ()
               ;(setf trigram (logior (ash (logand #xFFFF trigram) 8) (aref input i)))
               (setf trigram (ub24<<push trigram (aref input i)))
               (incf i))
             (output-literal ()
               ;(write-literal (ash (logand #xFF0000 trigram) -16) deflate-stream)
               (write-literal (ub-octet 16 trigram) deflate-stream))
             (output-length/distance (length distance)
               (write-length length deflate-stream)
               (write-distance distance deflate-stream)))
      (declare (inline save-trigram shift-in output-literal
                       output-length/distance))
      (when (< end 4)
        (dotimes (k end)
          (write-literal (aref input k) deflate-stream))
        (return-from compress))
      (shift-in)
      (shift-in)
      (shift-in)
      (tagbody
       loop
         (setf j (getfixhash (ub24-fixhash trigram) positions))
         (when (or (zerop j) (> (the fixnum (- i j)) 32768))
           (output-literal)
           (save-trigram)
           (when (= i end)
             ;(write-literal (ash (logand #xFF00 trigram) -8) deflate-stream)
             (write-literal (ub-octet 8 trigram) deflate-stream)
             ;(write-literal (logand #xFF trigram) deflate-stream)
             (write-literal (ub-octet 0 trigram) deflate-stream)
             (return-from compress))
           (shift-in)
           (go loop))
         (setf length 3
               distance (- i j))
       match-loop
         (when (and (< i end)
                    (= (aref input i) (aref input j))
                    (< length 258))
           (save-trigram)
           (shift-in)
           (incf j)
           (incf length)
           (go match-loop))
         (output-length/distance length distance)
         (cond ((= i end)
                (return-from compress))
               ((> (the fixnum (+ 3 i)) end)
                (do ((k i (1+ k)))
                    ((>= k end))
                  (declare (fixnum k))
                  (write-literal (aref input k) deflate-stream))
                ;(dotimes (k (- end i))
                ;  (write-literal (aref input (+ k i)) deflate-stream))
                (return-from compress)))
         (dotimes (k 3)
           (declare (fixnum k))
           (save-trigram)
           (shift-in))
         (go loop)))))

(defun compress-input (deflate-stream)
  "Output the pending input of DEFLATE-STREAM to its bitstream. Resets the
position cache."
  (compress deflate-stream)
  (clrfixhash (deflate-stream-compress-positions deflate-stream))
  (setf (deflate-stream-compress-pos deflate-stream) 0))

(defun compress-sequence (sequence deflate-stream start end)
  "Add the octet sequence SEQUENCE to DEFLATE-STREAM. May signal a
continuable error of type DEFLATE-STREAM-BUFFER-FULL."
  (symbol-macrolet ((pos (deflate-stream-compress-pos deflate-stream))
                    (buffer (deflate-stream-compress-buffer deflate-stream)))
    (let ((space-left (- (length buffer) pos)))
      (loop
       (octet-replace buffer sequence
                      pos *compressor-buffer-size*
                      start (min end (+ start space-left)))
       (cond ((<= space-left (- end start))
              (incf start space-left)
              (incf pos space-left)
              (compress-input deflate-stream)
              (setf space-left (- (length buffer) pos)))
             (t
              (incf pos (- end start))
              (return)))))))
              
          
(defun finish-compress (deflate-stream)
  "Write out any pending input in COMPRESSOR to its bitstream. May
signal BITSTREAM-BUFFER-FULL."
  (compress-input deflate-stream))

#|
(setq trigram (sys:integer-to-int32 #x70fdfe))
(format nil "~x" (ub-octet 16 trigram))
(format nil "~x" (ub24<<push trigram #x1e))
(disassemble 'compress)

;;; Похоже, увеличивает сборку мусора в 5 раз и
(defmacro make-fixhash-table (&rest args) `(make-hash-table ,@args))
(defmacro getfixhash (k fixhash-table) `(gethash ,k ,fixhash-table 0))
(defmacro clrfixhash (fixhash-table) `(clrhash ,fixhash-table))
(defun (setf fixhash:getfixhash) (new-value k fixhash-table)
  (setf (gethash k fixhash-table) new-value))

(setq fht (make-fixhash-table))

(ash (logand #x7FFFF x) 8)
(defun test (x y)
  (declare (fixnum x)
           (optimize (speed 3) (safety 0) (debug 0) (hcl:fixnum-safety 0) (float 0)))
  (let ((i32 (sys:integer-to-int32 x)))
    (getfixhash (the fixnum (sys:int32-to-integer (sys:int32+ i32 most-negative-fixnum)))
                y)
    (write-literal (the (integer 0 258)
                        (sys:int32-to-integer (sys:int32-logand #xFF0000 x))) y)
) )

(disassemble 'test)
|#


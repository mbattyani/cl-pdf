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
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type deflate-stream deflate-stream))
  (let* ((input (deflate-stream-compress-buffer deflate-stream))
         (positions (deflate-stream-compress-positions deflate-stream))
         (i 0)
         (j 0)
         (length 0)
         (distance 0) 
         (end (min (deflate-stream-compress-pos deflate-stream)
                   (length input)))
         (trigram 0))
    (declare (type octet-vector input)
             (type buffer-offset i j end)
             (type (integer 0 32768) distance)
             (type (integer 0 258) length)
             (type (integer 0 #xFFFFFF) trigram))
    (labels ((save-trigram ()
               (setf (getfixhash trigram positions) i))
             (shift-in ()
               (setf trigram (logior (ash (logand #xFFFF trigram) 8)
                                     (aref input i)))
               (incf i))
             (output-literal ()
               (write-literal (ash (logand #xFF0000 trigram) -16)
                              deflate-stream))
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
         (setf j (getfixhash trigram positions))
         (when (or (zerop j) (> (- i j) 32768))
           (output-literal)
           (save-trigram)
           (when (= i end)
             (write-literal (ash (logand #xFF00 trigram) -8) deflate-stream)
             (write-literal (logand #xFF trigram) deflate-stream)
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
               ((> (+ 3 i) end)
                (dotimes (k (- end i))
                  (write-literal (aref input (+ k i)) deflate-stream))
                (return-from compress)))
         (dotimes (k 3)
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

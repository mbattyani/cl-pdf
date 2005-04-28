;;; 
;;; huffman.lisp
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
;;; $Id: huffman.lisp,v 1.5 2005/03/20 21:33:49 xach Exp $

(in-package :salza-deflate)
            
;;;
;;; Huffman codes are written out to the stream backwards, so we save
;;; them backwards too.
;;;

(declaim (ftype (function (fixnum fixnum) fixnum) reverse-bits))
(defun reverse-bits (word n)
  (declare (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))
           (fixnum word n))
  (let ((j 0))
    (declare (fixnum j))
    (dotimes (i n j)
      (declare (fixnum i))
      (setf j (logior (ash j 1) (logand #x1 word)))
      (setf word (ash word -1)))))

(defun fixed-huffman-table ()
  "Generate the fixed Huffman code table specified by RFC1951."
  (declare (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0)))
  (let ((table (make-array (* 288 2)))	;:element-type (mod 512)
        (i 0))
    (declare (fixnum i))
    (flet ((fill-range (length start end)
             (declare (fixnum length start end))
             (loop for j fixnum from start to end
                   do (setf (aref table i) (reverse-bits j length)
                            (aref table (incf i)) length)
                      (incf i))))
      (fill-range 8 #b00110000  #b10111111)
      (fill-range 9 #b110010000 #b111111111)
      (fill-range 7 #b0000000   #b0010111)
      (fill-range 8 #b11000000  #b11000111)
      table)))


;;; DEFLATE uses special Huffman codes to indicate that there is extra
;;; literal data after the code. The WRITE-LITERAL, WRITE-LENGTH, and
;;; WRITE-DISTANCE functions close over vectors that contain bit
;;; patterns at the even offsets and bit lengths at the odd offsets.
;;;
;;; Since we only deal with encoding with the fixed Huffman table
;;; described in the RFC right now, everything can be precomputed.

(declaim (ftype (function (t fixnum fixnum fixnum) fixnum) save-pair))
(defun save-pair (array i code length)
  "Store CODE and LENGTH in consecutive positions in ARRAY."
  (declare (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))
           (fixnum i length))
  (let ((index (ash i 1)))
    (declare (fixnum index))
    (setf (aref array index) code
          (aref array (1+ index)) length)))

(defun length-table (huffman-table)
  "Compute a table of the (Huffman + extra bits) values for all
possible lengths for the given HUFFMAN-TABLE."
  (declare (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0)))
  (let ((table (make-array (* 259 2)))	;:element-type '(or null (mod 8132))
        (code 257)
        (length 3)
        (extra-bit-counts '(0 0 0 0 0 0 0 0
                            1 1 1 1
                            2 2 2 2
                            3 3 3 3
                            4 4 4 4
                            5 5 5 5
                            0)))
    (flet ((save-value (extra-bit-count extra-value)
             (let ((huffman-value (aref huffman-table (ash code 1)))
                   (huffman-count (aref huffman-table (1+ (ash code 1)))))
               (save-pair table length 
                          (logior huffman-value (ash extra-value huffman-count))
                          (+ huffman-count extra-bit-count)))))
      (dolist (count extra-bit-counts)
        (dotimes (i (expt 2 count))
          (when (< length 258)
            (save-value count i)
            (incf length)))
        (incf code))
      (setf code 285)
      (save-value 0 0))
    table))

(defun distance-table ()
  "Compute a table of the (code + extra bits) values for all possible
distances as specified by RFC1951."
  (let ((table (make-array (* 32769 2))) ;:element-type '(or null (mod 262136))
        (code 0)
        (distance 1)
        (extra-bit-counts '(0 0 0 0
                            1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9
                            10 10 11 11 12 12 13 13)))
    (flet ((save-value (extra-bit-count extra-value)
             (save-pair table distance 
                        (logior (ash extra-value 5) (reverse-bits code 5))
                        (+ 5 extra-bit-count))))
      (dolist (count extra-bit-counts table)
        (dotimes (i (expt 2 count))
          (save-value count i)
          (incf distance))
        (incf code)))))

(declaim (ftype (function ((integer 0 258) t)	buffer-offset)	write-literal)
         (ftype (function ((integer 0 32768) t) buffer-offset)	write-distance)
         (ftype (function ((integer 0 258) t)	buffer-offset)	write-length))
(let ((lvtable (fixed-huffman-table)))
  (declare (type simple-vector lvtable))
  (defun write-literal (code bitstream)
    "Write the Huffman code for the literal CODE to BITSTREAM."
    (declare (optimize (speed 3) (safety 0))
             (type (integer 0 258) code))
    (write-bits (svref lvtable (ash code 1))
                (svref lvtable (1+ (ash code 1))) 
                bitstream)))

;;; DI 2005-Apr-12: Should be created at run-time instead of load-time!
(let ((lvtable (distance-table)))
  (declare (type simple-vector lvtable))
  (defun write-distance (distance bitstream)
    "Write the Huffman code and extra bits for distance DISTANCE to
bitstream."
    (declare (optimize (speed 3) (safety 0))
             (type (integer 0 32768) distance))
    (write-bits (svref lvtable (ash distance 1))
                (svref lvtable (1+ (ash distance 1)))
                bitstream)))

(let ((lvtable (length-table (fixed-huffman-table))))
  (declare (type simple-vector lvtable))
  (defun write-length (length bitstream)
    "Write the 5 bit code and extra bits for the length LENGTH to
BITSTREAM."
    (declare (optimize (speed 3) (safety 0))
             (type (integer 0 258) length))
    (write-bits (svref lvtable (ash length 1))
                (svref lvtable (1+ (ash length 1)))
                bitstream)))

;;; Stub called from make-deflate-stream if di-huffman is not used.
(defun initialize-huffman (&optional force)
  (declare (ignore force)))



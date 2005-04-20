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

;;; DEFLATE uses special Huffman codes to indicate that there is extra
;;; literal data after the code. The WRITE-LITERAL, WRITE-LENGTH, and
;;; WRITE-DISTANCE functions close over vectors that contain bit
;;; patterns at the even offsets and bit lengths at the odd offsets.
;;;
;;; Since we only deal with encoding with the fixed Huffman table
;;; described in the RFC right now, everything can be precomputed.

;;; DI 2005-Apr-12 Rationale:
;;; OT1H, tables *fixed-huffman* and *fixed-lenght* are quite small.
;;; So we populate them at load time.
;;; OTOH, distance tables are rather huge and should not be included into the
;;; delivered image. So we load them at run time by invoking initialize-huffman.

(in-package :salza-deflate)
            
(declaim (ftype (function (fixnum fixnum) fixnum) reverse-bits))
(defun reverse-bits (word n)
  (declare (optimize (speed 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))
           (fixnum word n))
  (let ((j 0))
    (declare (fixnum j))
    (dotimes (i n j)
      (declare (fixnum i))
      (setf j (logior (ash j 1) (logand #x1 word))
            word (ash word -1)))))

;;; These examples are for the fixed Huffman code listed in RFC1951,
;;; but it's possible to create length and distance tables from an
;;; arbitrary Huffman table.
;;;
;;; The length and distance tables use an "extra bits" scheme to store their
;;; values. The length table uses Huffman codes with lengths of 7 and 8
;;; with extra bits of up to 5. The distance table uses Huffman codes with
;;; lengths of 5 and extra bits of up to 13.
;;;
;;; These functions return tables of (unsigned-byte 28) values. The
;;; upper five bits contain the number of bits in the resulting
;;; Huffman+extra-value code, which is in the lower bits.
;;;
;;; For example, a length value of 35 maps to the Huffman code 273. It
;;; has 3 extra bits, all set to 0. The Huffman code (in the fixed
;;; table) for 273 is 17, encoded in 7 bits. For added fun, the
;;; Huffman codes are written out in bitwise-reversed order. So we
;;; have:
;;;
;;; Overall length: 7 Huffman bits + 3 extra bits = 10
;;; Overall value:  0 extra value shifted by 7, 17 Huffman value
;;;
;;; Resulting 28-bit byte: 01010  00000000     000  1000100
;;;                            ^         ^       ^        ^
;;;       5 bits of length info    unused   extra     Huff 
;;;
;;; Finding the bits and the bit length needed to output a specific
;;; length code, then, is a matter of a table lookup and two masks. In
;;; this case, 0001000100 would be written as output.
;;;
;;; The situation is similar for the distance table, except:
;;;
;;;   - it uses a fixed, 5 bit fixed-width code in reversed bit order,
;;;     even though it's not a Huffman code
;;;
;;;   - the extra bit counts vary from 0 to 13
;;;
;;; So the length table doesn't change if a dynamic Huffman code is used.
;;;

(defparameter *fixed-huffman* (make-array 288 :element-type '(unsigned-byte 16))
  "The bits for the fixed Huffman tree of RFC1951; table values are 16
bits wide, with the length of the code in the upper four bits and the
value of the code in the lower 12.")

(let ((i 0))
  (declare (optimize (space 3) (safety 0) (debug 0) #+lispworks (hcl:fixnum-safety 0))
           (fixnum i))
  (flet ((fill-range (length start end)
           (declare (fixnum length start end))
           (loop for j fixnum from start to end
                 do (setf (aref *fixed-huffman* i)
                          (logior (ash length 12) (reverse-bits j length)))
                    (incf i))))
    (fill-range 8 #b00110000  #b10111111)
    (fill-range 9 #b110010000 #b111111111)
    (fill-range 7 #b0000000   #b0010111)
    (fill-range 8 #b11000000  #b11000111)))

;;; Compute a table of the (Huffman + extra bits) values for all
;;; possible lengths for the *fixed-huffman*.

(defparameter *fixed-length* (make-array 259 :element-type '(unsigned-byte 28))
  "The bits for the (Huffman + extra bits) values for every possible
length; table values are 28 bits wide, with the length of the code in
the upper five bits and the value of the code in the lower 23.")

(let ((code 257)
      (length 3)
      (extra-bit-counts '(0 0 0 0 0 0 0 0
                          1 1 1 1
                          2 2 2 2
                          3 3 3 3
                          4 4 4 4
                          5 5 5 5
                          0)))
  (declare (optimize (space 3) (safety 0) (debug 0)))
  (flet ((save-value (extra-bit-count extra-value)
           (let* ((huffman-raw (aref *fixed-huffman* code))
                  (huffman-count (ldb (byte 4 12) huffman-raw))
                  (huffman-value (ldb (byte 12 0) huffman-raw)))
             (setf (aref *fixed-length* length)
                   (logior (ash (+ huffman-count extra-bit-count) 23)
                           (ash extra-value huffman-count)
                           huffman-value)))))
    (dolist (count extra-bit-counts)
      (dotimes (i (expt 2 count))
        (when (< length 258)
          (save-value count i)
          (incf length)))
      (incf code))
    (setf code 285)
    (save-value 0 0)))

(declaim (ftype (function ((integer 0 258) t)	buffer-offset)	write-literal)
         (ftype (function ((integer 0 32768) t) buffer-offset)	write-distance)
         (ftype (function ((integer 0 258) t)	buffer-offset)	write-length))

(let ((%initialized% nil)
      huffman-vals  huffman-lens		; temporary vectors
      length-vals   length-lens			; providing arguments to write-bits
      distance-vals distance-lens)		; in a very fast manner
  (declare (type simple-vector huffman-vals length-vals distance-vals)
           (type octet-vector  huffman-lens length-lens distance-lens))

  (defun initialize-huffman (&optional force)
   ;;; Initialize distance and temporary tables at run time
    (when (and %initialized% (not force))
      (return-from initialize-huffman))

    (setf huffman-vals (make-array (length *fixed-huffman*))
          huffman-lens (make-array (length *fixed-huffman*) :element-type 'octet))
    (loop for i fixnum from 0 below (length *fixed-huffman*)
          for raw of-type (unsigned-byte 16) = (aref *fixed-huffman* i)
          do (setf (svref huffman-vals i) (ldb (byte 12 0) raw)
                   (aref huffman-lens i) (ldb (byte 4 12) raw)))

    (setf length-vals (make-array (length *fixed-length*))
          length-lens (make-array (length *fixed-length*) :element-type 'octet))
    (loop for i from 0 below (length *fixed-length*)
          for raw of-type (unsigned-byte 28) = (aref *fixed-length* i)
          do (setf (svref length-vals i) (ldb (byte 23 0) raw)
                   (aref length-lens i) (ldb (byte 5 23) raw)))

    ;;; Compute a table of the (code + extra bits) values for all possible
    ;;; distances as specified by RFC1951.
    (setf distance-vals (make-array 32769) 	;(length *fixed-distance*)
          distance-lens (make-array 32769 :element-type 'octet))
    (let ((code 0)
          (distance 1)
          (extra-bit-counts '(0 0 0 0
                              1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9
                              10 10 11 11 12 12 13 13)))
      ;(flet ((save-value (extra-bit-count extra-value)
      ;         (setf (aref *fixed-distance* distance)
      ;               (logior (ash (+ 5 extra-bit-count) 23) (ash extra-value 5)
      ;                       (reverse-bits code 5)))
      (dolist (count extra-bit-counts)
        (dotimes (extra-value (expt 2 count))
          ;(save-value count i)
          (let ((raw (logior (ash (+ 5 count) 23) (ash extra-value 5)
                             (reverse-bits code 5))))
            (setf (svref distance-vals distance) (ldb (byte 23 0) raw)
                  (aref distance-lens distance) (ldb (byte 5 23) raw)))
          (incf distance))
        (incf code)))
    (setq %initialized% t))

  (defun write-literal (code bitstream)
    "Write the Huffman code for the literal CODE to BITSTREAM."
    (declare (optimize (speed 3) (safety 0))
             (type (integer 0 258) code))
    (write-bits (svref huffman-vals code) (aref huffman-lens code) bitstream))

  (defun write-length (length bitstream)
    "Write the 5 bit code and extra bits for the length LENGTH to BITSTREAM."
    (declare (optimize (speed 3) (safety 0))
             (type (integer 0 258) length))
    (write-bits (svref length-vals length) (aref length-lens length) bitstream))

   (defun write-distance (distance bitstream)
     "Write the Huffman code and extra bits for distance DISTANCE to BITSTREAM."
     (declare (optimize (speed 3) (safety 0))
              (type (integer 0 32768) distance))
     (write-bits (svref distance-vals distance) (aref distance-lens distance) bitstream))
)

#|
(initialize-huffman t)
|#


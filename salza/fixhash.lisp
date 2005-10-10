;;; 
;;; fixhash.lisp
;;; 
;;; Created: 2005-03-19 by Zach Beane <xach@xach.com>
;;; 
;;; A hashtable whose keys and values are known to be fixnums^Wof a
;;; fixed, relatively small size. Sadly, not small enough to be
;;; fixnums on LispWorks.
;;;
;;; This table isn't general; it assumes that the compressor never
;;; uses zero for a key.
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
;;; $Id: fixhash.lisp,v 1.7 2005/04/01 21:55:24 xach Exp $

(in-package :fixhash)

(defparameter *sizes*
  #(4096
    16384
    65536
    131072))

(defstruct fixhash-table
  (level 0 :type fixnum)
  (size 4096 :type fixnum)
  (keys/values (make-array (* 4096 2)
                           :element-type 'fixhash-integer
                           :initial-element 0)
               :type (simple-array fixhash-integer (*)))
  (last-key 0 :type fixhash-integer)
  (last-key-pos 0 :type fixnum))

(defmethod print-object ((fixhash-table fixhash-table) stream)
  (print-unreadable-object (fixhash-table stream :type t :identity t)
    (format stream "~D/~D"
            (fixhash-table-level fixhash-table)
            (fixhash-table-size fixhash-table))))

(defun rehash (table)
  (declare (optimize (speed 3) (safety 0)
                     #+lw-int32 (hcl:fixnum-safety 0)))
  (let ((level (fixhash-table-level table))
        (keys/values (fixhash-table-keys/values table))
        (size (fixhash-table-size table)))
    (declare (fixnum level size))
    (when (= 3 level)
      (error "Hash table full"))
    (let* ((new-size (svref *sizes* (incf level)))
           (new-keys/values (make-array (the fixnum (* new-size 2))
                                        :initial-element 0
                                        :element-type 'fixhash-integer)))
      (dotimes (i (the fixnum (* size 2)))
        (declare (fixnum i))
        (setf (aref new-keys/values i) (aref keys/values i)))
      (setf (fixhash-table-keys/values table) new-keys/values
            (fixhash-table-size table) new-size
            (fixhash-table-level table) level))))

(declaim (ftype (function (fixhash-integer t) fixhash-integer) getfixhash))
(defun getfixhash (k fixhash-table)
  (declare (optimize (speed 3) (safety 0) (debug 0)
                     #+lw-int32 (hcl:fixnum-safety 0))
           (type fixhash-integer k))
  (let* ((size (fixhash-table-size fixhash-table))
         (mask (1- size))
         (h1 (the fixnum (logand k mask)))
         (h2 (the fixnum (logior 1 (the fixnum (mod k mask)))))
         (j 0)
         (i*h2 0)
         (table (fixhash-table-keys/values fixhash-table)))
    (declare (type (integer 0 131072) size mask h1 h2 j i*h2))
    (dotimes (i size (and (rehash fixhash-table) 0))
      (declare (fixnum i))
      (incf i*h2 h2)
      ;(setf j (ash (logand mask (+ h1 i*h2)) 1))
      (setf j (the fixnum
                   (ash (the fixnum (logand mask (the fixnum (+ h1 i*h2)))) 1)))
      (let ((kt (aref table j)))
        (when (= k kt)
          (return (aref table (1+ j))))
        (when (zerop kt)
          (setf (fixhash-table-last-key fixhash-table) k
                (fixhash-table-last-key-pos fixhash-table) j)
          (return 0))))))

(declaim (ftype (function (fixhash-integer fixhash-integer t) fixhash-integer)
                (setf getfixhash)))
(defun (setf getfixhash) (new-value k fixhash-table)
  (declare (optimize (speed 3) (safety 0) (debug 0)
                     #+lw-int32 (hcl:fixnum-safety 0))
           (type fixhash-integer new-value k))
  (let ((last-key (fixhash-table-last-key fixhash-table))
        (last-key-pos (fixhash-table-last-key-pos fixhash-table))
        (table (fixhash-table-keys/values fixhash-table)))
    (if (= last-key k)
        (setf (aref table last-key-pos) k
              (aref table (1+ last-key-pos)) new-value)
        (let* ((size (fixhash-table-size fixhash-table))
               (mask (1- size))
               (h1 (the fixnum (logand k mask)))
               (h2 (the fixnum (logior 1 (the fixnum (mod k mask)))))
               (i*h2 0)
               (j 0))
          (declare (type (integer 0 131072) h2 h1 i*h2 size mask))
          (dotimes (i size)
            (declare (fixnum i))
            (incf i*h2 h2)
            ;(setf j (ash (logand mask (+ h1 i*h2)) 1))
            (setf j (the fixnum
                         (ash (the fixnum (logand mask (the fixnum (+ h1 i*h2)))) 1)))
            (let ((kt (aref table j)))
              (when (or (= k kt) (zerop kt))
                (setf (aref table j) k
                      (aref table (1+ j)) new-value)
                (return new-value))))))))

(defun clrfixhash (fixhash-table)
  (declare (optimize (speed 3) (safety 0)
                     #+lw-int32 (hcl:fixnum-safety 0)))
  (let ((table (fixhash-table-keys/values fixhash-table)))
    (dotimes (i (length table))
      (declare (fixnum i))
      (setf (aref table i) 0)))
  fixhash-table)

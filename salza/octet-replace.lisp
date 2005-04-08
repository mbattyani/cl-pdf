;;; 
;;; octet-replace.lisp
;;; 
;;; Created: 2005-03-18 by Zach Beane <xach@xach.com>
;;; 
;;; REPLACE is generally pretty slow. Since we are working with octet
;;; vectors, provide a different version that is faster.
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
;;; $Id: octet-replace.lisp,v 1.2 2005/03/23 20:14:19 xach Exp $

(in-package :salza-deflate)

(defun octet-replace (sequence1 sequence2 start1 end1 start2 end2)
  (declare (type octet-vector sequence1 sequence2)
           (fixnum start1 end1 start2 end2)
           (optimize (speed 3) (safety 0)))
  (let ((i (min (- end1 start1) (- end2 start2))))
    (declare (fixnum i))
    (loop
     (when (zerop i)
       (return-from octet-replace sequence1))
     (setf (aref sequence1 start1) (aref sequence2 start2))
     (incf start1)
     (incf start2)
     (decf i))))
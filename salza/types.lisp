;;; 
;;; types.lisp
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
;;; $Id: types.lisp,v 1.4 2005/04/01 22:09:58 xach Exp $

(in-package :salza-types)

(deftype octet ()
  '(unsigned-byte 8))

(deftype buffer-offset ()
  '(integer 0 #.(1- array-dimension-limit)))

(deftype octet-vector ()
  '(simple-array octet (*)))

;;; Macrology for porting effectively to various implementaions.
;;; LW 4.4 port by Dmitriy Ivanov

#+(and lispworks (not (or lispworks4.3 lispworks4.2 lispworks4.1 lispworks4.0)))
(eval-when (:compile-toplevel :execute)
  (pushnew :lw-int32 *features*))

;;; Type use for compression trigram (or general-purpose?)
(deftype ub24 ()
  #+lw-int32 'sys:int32
  #-lw-int32 '(unsigned-byte 24))

(defmacro ub-octet (position arg)
 ;;; General pseudo-unsigned-byte accessor a la ldb.
  ;; Args: position - bit position.
  #+lw-int32
  `(the octet (sys:int32-to-integer (sys:int32-logand ,(if (eql position 0)
                                                           arg
                                                           `(sys:int32>> ,arg ,position))
                                                      #xFF)))
  #-lw-int32
  `(the octet (ldb (byte 8 ,position) ,arg)))

(defmacro ub24<<push (ub24 octet)
  #+lw-int32
  `(sys:int32-logior (sys:int32<< (sys:int32-logand ,ub24 #xFFFF) 8) (the octet ,octet))
  #-lw-int32
  `(the ub24 (logior (the ub24 (ash (the ub24 (logand ,ub24 #xFFFF)) 8))
                     (the octet ,octet))))

(define-symbol-macro +ub24-0+
  #+lw-int32 sys:+int32-0+
  #-lw-int32 0)


(deftype fixhash-integer ()
  "#xFFFFFF is out of fixnum range on LispWorks."
  #+lw-int32 'fixnum
  #-lw-int32 '(integer 0 #xFFFFFF))

(defmacro ub24-fixhash (ub24)
  #+lw-int32
  `(the fixhash-integer (sys:int32-to-integer (sys:int32+ ,ub24 most-negative-fixnum)))
  #-lw-int32
  ub24)


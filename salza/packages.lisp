;;; 
;;; packages.lisp
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
;;; $Id: packages.lisp,v 1.9 2005/03/25 21:09:02 xach Exp $

(in-package :cl)

(defpackage :salza-types
  (:use :cl)
  (:export :octet
           :octet-vector
           :buffer-offset))
  
(defpackage :salza-deflate
  (:use :cl :salza-types :fixhash)
  (:nicknames :deflate)
  (:export :make-deflate-stream
           :deflate-stream-buffer
           :deflate-stream-pos
           :deflate-stream-callback

           :start-deflate-stream
           :deflate-write-byte
           :deflate-write-sequence
           :deflate-write-string
           :finish-deflate-stream

           :deflate-stream-buffer-full
           :deflate-stream-buffer-full-deflate-stream

           :crc32
           :crc32-sequence))


(defpackage :salza
  (:use :cl :salza-types :salza-deflate)
  (:nicknames :zlib)
  (:export :make-zlib-stream
           :zlib-write-sequence
           :zlib-write-string
           :zlib-stream-buffer
           :zlib-stream-position
           :zlib-stream-callback
           :finish-zlib-stream

           :compress-sequence
           :compress-string
           :compess-stream

           :zlib-buffer-full
           :zlib-buffer-full-zlib-stream))



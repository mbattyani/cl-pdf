;;; 
;;; zlib.asd
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
;;; $Id: salza.asd,v 1.4 2005/03/20 04:54:55 xach Exp $

(defpackage :salza-system
  (:use :cl :asdf))

(in-package :salza-system)

(defsystem :salza
  :components ((:file "fixhash")
               (:file "packages"
                      :depends-on ("fixhash"))
               (:file "types"
                      :depends-on ("packages"))
               (:file "deflate-stream"
                      :depends-on ("packages"
                                   "types"))
               (:file "huffman"
                      :depends-on ("packages"
                                   "types"
                                   "deflate-stream"))
               (:file "octet-replace"
                      :depends-on ("packages"))
               (:file "compressor"
                      :depends-on ("packages"
                                   "types"
                                   "deflate-stream"
                                   "huffman"
                                   "octet-replace"))
               (:file "deflate-stream-interface"
                      :depends-on ("packages"
                                   "compressor"
                                   "deflate-stream"))
               (:file "zlib"
                      :depends-on ("packages"
                                   "types"
                                   "deflate-stream"
                                   "huffman"
                                   "compressor"))))




;;; 
;;; png.lisp
;;; 
;;; Created: 2005-03-14 by Zach Beane <xach@xach.com>
;;; 
;;; An example use of the salza ZLIB interface functions.
;;;
;;; (setq png (make-instance 'png
;;;                          :color-type :truecolor
;;;                          :height 10
;;;                          :width 10
;;;                          :image-data <300 bytes of image data>))
;;;
;;; (write-png png "example.png")
;;;
;;;
;;; $Id: png.lisp,v 1.1 2005/03/15 05:06:03 xach Exp $

(defpackage :png
  (:use :cl :salza :salza-deflate)
  (:export :png
           :write-png))

(in-package :png)


;;; Chunks

(defclass chunk ()
  ((buffer :initarg :buffer :reader buffer)
   (pos :initform 4 :accessor pos)))

(defun chunk-write-byte (byte chunk)
  "Save one byte to CHUNK."
  (setf (aref (buffer chunk) (pos chunk)) byte)
  (incf (pos chunk)))

(defun chunk-write-uint32 (integer chunk)
  "Save INTEGER to CHUNK as four bytes."
  (dotimes (i 4)
    (setf (aref (buffer chunk) (pos chunk))
          (logand #xFF (ash integer (+ -24 (* i 8)))))
    (incf (pos chunk))))

(defun make-chunk (a b c d size)
  "Make a chunk that uses A, B, C, and D as the signature bytes, with
data size SIZE."
  (let ((buffer (make-array (+ size 4) :element-type '(unsigned-byte 8))))
    (setf (aref buffer 0) a
          (aref buffer 1) b
          (aref buffer 2) c
          (aref buffer 3) d)
    (make-instance 'chunk
                   :buffer buffer)))

(defun write-uint32 (integer stream)
  (dotimes (i 4)
    (write-byte (logand #xFF (ash integer (+ -24 (* i 8)))) stream)))

(defun write-chunk (chunk stream)
  (write-uint32 (- (pos chunk) 4) stream)
  (write-sequence (buffer chunk) stream :end (pos chunk))
  (write-sequence (crc32-sequence (buffer chunk) :end (pos chunk)) stream))


;;; PNGs

(defclass png ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (color-type :initform :truecolor :initarg :color-type :reader color-type)
   (bpp :initform 8 :initarg :bpp :reader bpp)
   (image-data :initarg :image-data :reader image-data)))

(defmethod initialize-instance :after ((png png) &rest args)
  (declare (ignore args))
  (assert (= (length (image-data png))
             (* (height png) (rowstride png)))))

(defgeneric write-ihdr (png stream))
(defgeneric ihdr-color-type (png))
(defgeneric write-idat (png stream))
(defgeneric write-iend (png stream))
(defgeneric write-png-header (png stream))
(defgeneric scanline-offset (png scanline))
(defgeneric rowstride (png))
(defgeneric samples/pixel (png))

(defmethod samples/pixel (png)
  (ecase (color-type png)
    (:grayscale 1)
    (:truecolor 3)
    (:indexed-color 1)
    (:grayscale-alpha 2)
    (:truecolor-alpha 4)))


(defmethod rowstride (png)
  (* (width png) (samples/pixel png)))

(defmethod scanline-offset (png scanline)
  (* scanline (rowstride png)))

(defmethod write-png-header (png stream)
  (write-sequence #(137 80 78 71 13 10 26 10) stream))

(defvar *color-types*
  '((:grayscale . 0)
    (:truecolor . 2)
    (:indexed-color . 3)
    (:grayscale-alpha . 4)
    (:truecolor-alpha . 6)))

(defmethod ihdr-color-type (png)
  (cdr (assoc (color-type png) *color-types*)))

(defmethod write-ihdr (png stream)
  (let ((chunk (make-chunk 73 72 68 82 13)))
    (chunk-write-uint32 (width png) chunk)
    (chunk-write-uint32 (height png) chunk)
    (chunk-write-byte (bpp png) chunk)
    (chunk-write-byte (ihdr-color-type png) chunk)
    ;; compression method
    (chunk-write-byte 0 chunk)
    ;; filtering
    (chunk-write-byte 0 chunk)
    ;; interlace
    (chunk-write-byte 0 chunk)
    (write-chunk chunk stream)))

(defmethod write-idat (png stream)
  (let* ((chunk (make-chunk 73 68 65 84 16384))
         (zlib-stream (make-zlib-stream (buffer chunk) 4))
         (filter-type (make-array 1
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
    (flet ((write-full-chunk (c)
             (declare (ignore c))
             (setf (pos chunk) (zlib-stream-position zlib-stream))
             (write-chunk chunk stream)
             (fill (buffer chunk) 0 :start 4)
             (setf (zlib-stream-position zlib-stream) 4)
             (continue)))
      (handler-bind
          ((zlib-buffer-full #'write-full-chunk))
        (dotimes (i (height png))
          (let* ((start-offset (scanline-offset png i))
                 (end-offset (+ start-offset (rowstride png))))
            (zlib-write-sequence filter-type zlib-stream)
            (zlib-write-sequence (image-data png) zlib-stream
                                 :start start-offset
                                 :end end-offset)))
        (finish-zlib-stream zlib-stream))
      (setf (pos chunk) (zlib-stream-position zlib-stream))
      (write-chunk chunk stream))))
    

(defmethod write-iend (png stream)
  (let ((chunk (make-chunk 73 69 78 68 0)))
    (write-chunk chunk stream)))

(defmethod write-png (png file &key (if-exists :supersede))
  (with-open-file (stream file
                   :direction :output
                   :if-exists if-exists
                   :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
    (write-png-header png stream)
    (write-ihdr png stream)
    (write-idat png stream)
    (write-iend png stream)
    (truename file)))

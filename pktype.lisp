;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Touched: Sat Oct 22 07:46:04 2005 +0530 <enometh@meer.net>
;;;   Time-stamp: <05/10/22 08:12:22 madhu>
;;;
;;;; vim: ft=lisp
;;;; Touched: <26-Sep-03 03:42:39 IST, madhu>
;;;;
;;;; PK files are well described in `web2c/pktype.web' in TeX sources.
;;;; This is a blind translation in CL of that idiomatic pascal program
;;;; and produces the exact same output as `pktype'. In addition, it also
;;;; computes raster info for encoding the font as Type 3, in pdf dicts
;;;;
;;;; Translated 2003 Madhu. Bugs-To: <enometh@net.meer>

(defpackage "PK"
  (:use "CL")
  (:export "PKTYPE"
	   "GET-PKFILE"
	   "PK-FONT-SIZE"
	   "PK-FONT-SCALE"
	   "PK-CHAR-WIDTH"
	   "PK-FONT-NAME"
	   "PK-CHAR-DETAILS"))
(in-package "PK")

(defvar *pk-file-stream* nil) ; stream of (unsigned-byte 8) elements

(define-symbol-macro pk-byte (read-byte *pk-file-stream*))

(define-symbol-macro pk-loc (file-position *pk-file-stream*))

(define-symbol-macro get-16 ; 2 byte value
  (let ((a pk-byte) (b pk-byte))
     (declare (type (unsigned-byte 8) a b))
     (+ (* 256 a) b)))

(define-symbol-macro get-32 ; signed 4 byte value
  (let ((a get-16) (b get-16))
    (declare (type (unsigned-byte 16) a b))
    (+ (* 65536 (if (> a 32767) (- a 65536) a)) b)))

(defconstant +pk-id+ 	89 "version of .PK file described")

(defconstant +pk-pre+	247 "preamble (i[1] k[1] x[k] ds[4] cs[5] hppp[4]")

(defconstant +pk-xxx+ 	'(240 241 242 243))

(defconstant +pk-xxx1+	240 "\special commands (k[1] x[k])") ;k+2 byte no-op

;(defconstant +pk-xxx2+ 241 "\special commands (k[2] x[k])") ;0<=k<=65536

;(defconstant +pk-xxx3+ 242 "\special commands (k[3] x[k])") ;0<=k<2^{24}

;(defconstant +pk-xxx4+ 243 "\special commands (k[4] x[k])") ;k huge.

(defconstant +pk-yyy+	244 "\numspecial commands (y[4])")

(defconstant +pk-post+	245 "postamble") ;padded to 4-byte boundary with no-ops

(defconstant +pk-no-op+	246 "no operation")

;(defconstant +pk-undefined-ops+ '(248 249 250 251 252 253 254 255))

(defun say (&rest args) (apply #'format t args))

;; XXX this speeds things up
#-nil
(defmacro say (&rest args) (declare (ignore args)))

(defvar *%font-dsize* nil)

(defvar *%resolution* nil)

(defun pk-read-preamble ()
  (unless (= pk-byte +pk-pre+) (error "Bad PK file: pre command missing!"))
  (unless (= pk-byte +pk-id+) (error "Wrong version of PK file!"))
  (let ((comment (loop with comment-length = pk-byte
		       repeat comment-length collect pk-byte)))
    (declare (ignorable comment))
    (say "~&'~a'~&" (map 'string #'code-char comment)))
  (let ((design-size get-32)) ; in 1/2^{20} points
    (declare (ignorable design-size)) ;XXX
    (setf *%font-dsize* (/ design-size 1048576.0))
    (say "Design size = ~a~&" design-size))
  (let ((check-sum get-32))
    (declare (ignorable check-sum))
    (say "Checksum = ~a~&" check-sum))
  (let ((hppp get-32) ; horizontal resolution, points per inch, and
	(vppp get-32)) ; vertical resolution; multiplied by 2^16
    (unless (= hppp vppp) (warn "aspect ratio not 1:1"))
    (setf *%resolution* (/ hppp 65535.0))
    (say "Resolution: horizontal = ~a  vertical = ~a  (~a dpi)~&"
	 hppp vppp (round (/ (* 72.27 hppp) 65535))))
  (values))

(defun pk-read-command (flagbyte) ; skip-specials
  (cond ((= flagbyte +pk-no-op+)
	 (say "~a:  No op~&" (1- pk-loc)))
	((= flagbyte +pk-yyy+)
	 (let ((c get-32))
	   (declare (ignorable c))
	   (say "~a:  Num Special: ~a~&" (1- pk-loc) c)));bug
	((member flagbyte +pk-xxx+ :test #'eql)
	 (say "~a:  Special: " (1- pk-loc))
	 (let ((k 0))
	   (loop for j from +pk-xxx1+ to flagbyte do
		 (setf k (+ (* k 256) pk-byte)))
	   (loop for j below k collecting pk-byte into bytes
		 finally (say "'~a'~&" (map 'string #'code-char bytes)))))
	(t (error "got ~a. expected one of ~a" flagbyte
		  (adjoin +pk-no-op+ (adjoin +pk-yyy+ +pk-xxx+))))))

(defun pk-read-postamble (flagbyte)
  (say "~a:  Postamble~&" (1- pk-loc)) (assert (= flagbyte +pk-post+))
  (handler-bind ((end-of-file (lambda (c) (declare (ignore c))
				(return-from pk-read-postamble :EOF))))
    (loop for i = pk-byte do
	  (if (= i +pk-no-op+)
	    (say "~a:  No op~&" (1- pk-loc))
	    (error "bad byte at end of file: ~a" i)))))

(defvar *bit-weight* 0 "weight of current bit")

(defvar *input-byte* 0 "byte currently being read")

(defvar *nybble* 0 "current nybble")

(define-symbol-macro get-nyb
  (progn
    (when (zerop *bit-weight*)
      (setf *input-byte* pk-byte)
      (setf *bit-weight* 16))
    (multiple-value-bind (n r) (floor *input-byte* *bit-weight*)
      (setf *input-byte* r)
      (setf *bit-weight* (floor (/ *bit-weight* 16)))
      n)))

(define-symbol-macro get-bit
  (progn
     (setf *bit-weight* (floor *bit-weight* 2))
     (when (= *bit-weight* 0)
       (setf *input-byte* pk-byte)
       (setf *bit-weight* 128))
     (let ((r (>= *input-byte* *bit-weight*)))
       (when r (decf *input-byte* *bit-weight*))
       r)))

(defclass chardesc () ; bastard struct. todo class-allocate the `local-vars'
  (car dx dy x-off y-off width height tfm-width
       turn-on flag-byte dyn-f
       packet-length end-of-packet
       raster rastersize p-bit p b rw))

(defun  read-long-character-preamble (ch)
  (with-slots (car dx dy x-off y-off width height tfm-width packet-length end-of-packet) ch
    (setf packet-length get-32)
    (setf car get-32) ; character code
    (setf end-of-packet (+ pk-loc packet-length))
    (incf packet-length 9)
    (setf tfm-width get-32)
    (setf dx get-32)
    (setf dy get-32)
    (setf width get-32)
    (setf height get-32)
    (setf x-off get-32)
    (setf y-off get-32)))

(defun read-extended-short-character-preamble (ch)
  (with-slots (car dx dy x-off y-off width height tfm-width packet-length end-of-packet flag-byte) ch
    (setf packet-length (+ (* 65536 (- flag-byte 4)) get-16))
    (setf car pk-byte)
    (setf end-of-packet (+ pk-loc packet-length))
    (incf packet-length 4)
    (let ((i pk-byte)) (setf tfm-width (+ (* i 65536) get-16)))
    (setf dx (* 65536 get-16))
    (setf dy 0)
    (setf width get-16)
    (setf height get-16)
    (setf x-off get-16)
    (setf y-off get-16)
    (if (> x-off 32767) (decf x-off 65536))
    (if (> y-off 32767) (decf y-off 65536))))

(defun read-short-char-preamble (ch)
  (with-slots (car dx dy x-off y-off width height tfm-width packet-length end-of-packet flag-byte) ch
    (setf packet-length (+ (* 256 flag-byte) pk-byte))
    (setf car pk-byte)
    (setf end-of-packet (+ pk-loc packet-length))
    (incf packet-length 3)
    (let ((i pk-byte)) (setf tfm-width (+ (* 65536 i) get-16)))
    (setf dx (* 65536 pk-byte))
    (setf dy 0)
    (setf width pk-byte)
    (setf height pk-byte)
    (setf x-off pk-byte)
    (setf y-off pk-byte)
    (if (> x-off 127) (decf x-off 256))
    (if (> y-off 127) (decf y-off 256))))

(defun %init-raster (ch)
  (with-slots (width height raster rastersize p-bit p b rw) ch
    (setf rw (floor (+ width 7) 8)) ; width (of `width') in bytes
    (setf rastersize (* height rw))
    (when (<= rastersize 0)
      (warn "hmm: rastersize=~a <=1" rastersize)
      (setf rastersize 1))
    (setf p-bit 7)  ; position of current bit in byte (for +power+)
    (setf p 0)      ; current index into `raster'
    (setf b 0)      ; current byte
    (setf raster (make-array rastersize :element-type '(unsigned-byte 16)))))

(defun %debug-raster (ch)
  (with-slots (height width raster p rw) ch
    (setf p 0)
    (format t "~&")
    (loop repeat height do
	  (loop repeat rw do
		(format t "~8,'0b" (aref raster p))
		(incf p))
	  (format t "~&"))))

(defconstant +power+ #(0 1 3 7 15 31 63 127 255))

(defmacro power (weight) `(aref +power+ ,weight))

(defun get-raster-by-bits (ch) ; not tested yet
  (with-slots (height width raster p-bit p b rastersize) ch
    (assert (zerop p))
    (loop repeat height do
	  (say " ")
	  (setf p-bit 7 b 0)
	  (loop repeat width for on = get-bit do
		(cond (on (incf b (1+ (power p-bit)))
			  (say "*"))
		      (t (say ".")))
		(decf p-bit)
		(when (< p-bit 0)
		  (setf (aref raster p) b) (incf p)
		  (setf p-bit 7 b 0)))
	  (unless (= p-bit 7)
	    (setf (aref raster p) b)
	    (incf p)) ; loop resets rest
	  (say "~%"))
    (assert (= p rastersize) ()
	    "~D mismatch rastersize ~D" p rastersize)))


(defvar *term-pos*)

(defun send-out (repeat-count-p value ch)
  (with-slots (turn-on) ch
    (let ((i 10) (len 1))
      (loop while (>= value i) do (incf len) (setf i (* i 10)))
      (when (or repeat-count-p (not turn-on))
	(incf len 2))
      (cond ((> (+ *term-pos* len) 78)
	     (setf *term-pos* (+ len 2))
	     (say " ~%  "))
	    (t (incf *term-pos* len)))
      (cond (repeat-count-p (say "[~D]" value))
	    (turn-on (say "~D" value))
	    (t (say "(~D)" value))))))

(defun pk-packed-num (ch)
  (with-slots (dyn-f turn-on) ch
    (let ((i get-nyb) j)
      (declare (special repeat-count))
      (cond ((zerop i)
	     (loop do (setf j get-nyb) (incf i) until (/= j 0))
	     (loop while (> i 0) do (setf j (+ (* j 16) get-nyb)) (decf i))
	     (+ j -15 (* (- 13 dyn-f) 16) dyn-f))
	    ((<= i dyn-f) i)
	    ((< i 14) (+ (* (- i dyn-f 1) 16) get-nyb dyn-f 1))
	    (t (unless (zerop repeat-count)
		 (error "Second repeat count for this row"))
	       (setf repeat-count 1)
	       (if (= i 14) (setf repeat-count (pk-packed-num ch)))
	       (send-out t repeat-count ch)
	       (pk-packed-num ch))))))

(defun  create-normally-packed-raster (ch)
  (setf *term-pos* 2)
  (say "  ")
  (with-slots (height width turn-on dyn-f raster p-bit p b rw) ch
    (let ((rows-left height) (h-bit width) (repeat-count 0))
      (declare (special repeat-count))
      (assert (= 0 b))
      (setf p-bit 8 b 0 p 0)
      (loop
	while (> rows-left 0) do
	(let ((count (pk-packed-num ch)))
	  (send-out nil count ch)
	  (loop while (> count 0) do
	      (cond ((and (< count p-bit) (< count h-bit))
		     (if turn-on (incf b (- (power p-bit) (power (- p-bit count)))))
		     (decf h-bit count)
		     (decf p-bit count)
		     (setf count 0))
		    ((and (>= count h-bit) (<= h-bit p-bit))
		     (if turn-on
		       (incf b (- (power p-bit) (power (- p-bit h-bit)))))
		     (setf (aref raster p) b) (incf p) (setf b 0)
		     (loop repeat repeat-count do
			   (loop repeat rw do
				 (setf (aref raster p) (aref raster (- p rw)))
				 (incf p)))
		     (decf rows-left (1+ repeat-count))
		     (setf repeat-count 0)
		     (decf count h-bit)
		     (setf p-bit 8)
		     (setf h-bit width))
		    (t
		      (if turn-on (incf b  (power p-bit)))
		      (setf (aref raster p) b) (incf p) (setf b 0)
		      (decf count p-bit)
		      (decf h-bit p-bit)
		      (setf p-bit 8))))
	  (setf turn-on (not turn-on))))
      (say " ~%")
      (unless (and (zerop rows-left) (= width h-bit))
	(error "Bad PK file: More bits than required!")))))

(defun read-and-translate-raster-description (ch)
  (setf *bit-weight* 0)
  (%init-raster ch)
  (with-slots (dyn-f) ch
    (if (= dyn-f 14)
      (get-raster-by-bits ch)
      (create-normally-packed-raster ch))))

(defvar *%chars* (make-array 256)) ; array of ALL chars (last call to PKTYPE)

(defun pk-read-char (flagbyte) ; unpack-and-write-character
  (say "~a:  Flag byte = ~a" (1- pk-loc) flagbyte)
  (let ((ch (make-instance 'chardesc)))
    (with-slots (turn-on flag-byte dyn-f) ch
      (multiple-value-bind (n r) (floor flagbyte 16)
	(setf dyn-f n) (setf flag-byte r))
      (when (setf turn-on (>= flag-byte 8))
	(decf flag-byte 8))
      (cond ((= flag-byte 7) (read-long-character-preamble ch))
	    ((> flag-byte 3) (read-extended-short-character-preamble ch))
	    (t (read-short-char-preamble ch))))
    (with-slots (packet-length dyn-f tfm-width dx dy height width x-off y-off car) ch
      (setf (aref *%chars* car) ch)
      ;; ^ this imposes limit on car code range (0 255)
      (say "  Character = ~a  Packet length = ~a~&" car packet-length)
      (say "  Dynamic packing variable = ~a~&" dyn-f)
      (say "  TFM width = ~a  dx = ~a ~@[  dy = ~a~]~&" tfm-width dx
	   (unless (zerop dy) dy))
      (say "  Height = ~a  Width = ~a  X-offset = ~a  Y-offset = ~a~&"
	   height width x-off y-off))
    (read-and-translate-raster-description ch)
    (with-slots (end-of-packet) ch
      (unless (= pk-loc end-of-packet)
	(error "Bad PK File: Bad packet length")))))

(defun pktype (file)
  (with-open-file (*pk-file-stream* file :direction :input :element-type '(unsigned-byte 8))
    (pk-read-preamble)
    (loop for flagbyte = pk-byte until (= flagbyte +pk-post+) do
	  (if (<= flagbyte 239)
	    (pk-read-char flagbyte) ; flagbyte introduced a char
	    (pk-read-command flagbyte)) ; flagbyte introduced a command
	  finally (pk-read-postamble flagbyte))))

#+nil
(pk:pktype  #p"skt/sktbs10.864pk")

;;;
;;;
;;; afterthought: caching wrapper around pktype

(defclass pkfile()
  ((font-size :accessor pk-font-size :initarg font-size)
   (font-name :accessor pk-font-name :initarg font-name)
   (resolution :accessor pk-resolution :initarg resolution) ;; points/inch
   (font-scale :accessor pk-font-scale)
   (chars :accessor chars :initarg chars)))

(defmacro round-to ((digits) value)
  "round VALUE to DIGITS decimal places"
  (check-type digits (integer 0 10))
  (let ((var (gensym)) (precision (expt 10 digits)))
    `(let ((,var ,value))
       (if (integerp ,var) ,var
	 (coerce (/ (round (* ,precision ,var)) ,precision) 'single-float)))))

#+nil
(pprint (macroexpand-1 '(round-to (3) pi)))

(defmethod initialize-instance :after ((pkfile pkfile) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value pkfile 'font-scale)
        (round-to (5)
		  (/ .01 (/ (pk-resolution pkfile) (pk-font-size pkfile))))))

(defmethod pk-char-width ((pkfile pkfile) (car integer)) ; points/inch
  (check-type car (integer 0 255))
  (let ((tfm-width (/ (slot-value (aref (chars pkfile) car) 'tfm-width)
                      1048576)))
    (round-to (2)
	      (/ tfm-width (pk-font-scale pkfile)))))

(defvar *pkhash* (make-hash-table :test #'equal)) ; XXX cache

(defmethod get-pkfile ((pkfile pathname) &optional (reread nil))
  (let ((p (gethash (truename pkfile) *pkhash*)))
    (cond ((and (not reread) p)
	   (warn "load-pk-font: ~s already loaded: ~s" pkfile p) p)
          (t (when p
	       (warn "load-pk-font: ~s: reread overwrites ~s" pkfile p))
	     (let ((*%chars* (make-array 256)) *%font-dsize* *%resolution*)
               (pk:pktype pkfile)
               (setf (gethash (truename pkfile) *pkhash*)
                     (make-instance 'pkfile ; ugh! infer font name from file
                                    'font-name (pathname-name pkfile) ;XXX
                                    'font-size *%font-dsize*
                                    'resolution *%resolution*
                                    'chars *%chars*)))))))

(defmethod pk-char-details ((pkfile pkfile) (car integer))
  ;; XXX return a list: specifically for destructuring
  (check-type car (integer 0 255))
  (with-slots (x-off y-off width height raster rastersize rw)
      (aref (chars pkfile) car)
    (list x-off y-off width height raster rastersize rw)))

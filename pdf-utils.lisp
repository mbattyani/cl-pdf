;;; cl-pdf copyright 2002 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package pdf)

;;;text functions

(defmacro in-text-mode (&body body)
  `(unwind-protect
     (progn (write-line "BT" *page-stream*)
	    ,@body)
    (write-line "ET" *page-stream*)))

(defun set-font (font size)
  (format *page-stream* "~a ~,2f Tf~%" (name font) size))

(defmacro def-pdf-op (name (&rest args) format)
  (if args
    `(defun ,name ,args (format *page-stream* ,format ,@args))
    `(defun ,name () (write-line ,format *page-stream*))))

(def-pdf-op move-text (dx dy) "~,3f ~,3f Td~%")

(def-pdf-op draw-text (string) "(~a) Tj~%")

(def-pdf-op draw-text-on-next-line (string) "(~a) '~%")

(def-pdf-op set-text-rendering-mode (mode) "~d Tr~%")

(def-pdf-op set-char-spacing (space) "~,3f Tc~%")

(def-pdf-op set-text-x-scale (scale) "~,3f Tz~%")

(def-pdf-op set-text-leading (space) "~,3f TL~%")

(def-pdf-op set-text-rise (rise) "~,3f Ts~%")

(def-pdf-op move-to-next-line () " T*~%")

(def-pdf-op set-text-matrix (a b c d e f) "~,3f ~,3f ~,3f ~,3f ~,3f ~,3f Tm~%")

(defun draw-and-adjust-string (strings)
  (write-string "[ " *page-stream*)
  (dolist (item strings)
    (if (numberp item)
       (format *page-stream* "~,3f " item)
       (format *page-stream*"(~a) " item)))
  (write-line "] TJ" *page-stream*))

;;; graphic functions

(defmacro with-saved-state (&body body)
  `(unwind-protect
     (progn (write-line "q" *page-stream*)
	    ,@body)
    (write-line "Q" *page-stream*)))

(def-pdf-op set-transform-matrix (a b c d e f) "~,3f ~,3f ~,3f ~,3f ~,3f ~,3f cm~%")

(def-pdf-op translate (dx dy) "1.0 0.0 0.0 1.0 ~,3f ~,3f cm~%")

(defun rotate (deg)
  (let* ((angle (/ (* pi deg) 180.0))
	 (s (sin angle))
	 (c (cos angle)))
    (format *page-stream* "~,3f ~,3f ~,3f ~,3f 0.0 0.0 cm~%" c s (- s) c)))

(def-pdf-op scale (sx sy) " ~,3f 0.0 0.0 ~,3f 0.0 0.0 cm~%")

(defun skew (x-deg y-deg)
  (format *page-stream* " 1.0 ~,3f ~,3f 1.0 0.0 0.0 cm~%"
	  (tan (/ (* pi x-deg) 180.0))(tan (/ (* pi y-deg) 180.0))))

(def-pdf-op set-line-width (width) "~,3f w~%")

(def-pdf-op set-line-cap (mode) "~d J~%")

(def-pdf-op set-line-join (mode) "~d j~%")

(def-pdf-op set-dash-pattern (dash-array phase) "[~{~d~^ ~}] ~d~%")

(def-pdf-op set-mitter-limit (limit) "~,3f M~%")

(def-pdf-op move-to (x y) "~,3f ~,3f m~%")

(def-pdf-op line-to (x y) "~,3f ~,3f l~%")

(def-pdf-op bezier-to (x1 y1 x2 y2 x3 y3) "~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c~%")

(def-pdf-op bezier2-to (x2 y2 x3 y3) "~,3f ~,3f ~,3f ~,3f v~%")

(def-pdf-op bezier3-to (x1 y1 x3 y3) "~,3f ~,3f ~,3f ~,3f y~%")

(def-pdf-op close-path () " h")

(def-pdf-op basic-rect (x y dx dy) "~,3f ~,3f ~,3f ~,3f re~%")

(defun paint-image (image)
  (format *page-stream* "~a Do~%" (name image)))

(def-pdf-op stroke () " S")

(def-pdf-op close-and-stroke () " s")

(def-pdf-op fill-path () " f")

(def-pdf-op close-and-fill () " h f")

(def-pdf-op even-odd-fill () " f*")

(def-pdf-op fill-and-stroke () " B")

(def-pdf-op even-odd-fill-and-stroke () " B*")

(def-pdf-op close-fill-and-stroke () " b")

(def-pdf-op close-even-odd-fill-and-stroke () " b*")

(def-pdf-op end-path-no-op  () " n")

(def-pdf-op clip-path () " W")

(def-pdf-op even-odd-clip-path () " W*")

(def-pdf-op set-gray-stroke (gray) "~,3f G~%")

(def-pdf-op set-gray-fill (gray) "~,3f g~%")

(def-pdf-op set-rgb-stroke (r g b) "~,3f ~,3f ~,3f RG~%")

(def-pdf-op set-rgb-fill (r g b) "~,3f ~,3f ~,3f rg~%")

(def-pdf-op set-cymk-stroke (c y m k) "~,3f ~,3f ~,3f ~,3f K~%")

(def-pdf-op set-cymk-fill (c y m k) "~,3f ~,3f ~,3f ~,3f k~%")


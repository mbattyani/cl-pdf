;;; -*- lisp -*- system definition

(in-package asdf)

(defsystem :iterate
    :components ((:file "package")
		 (:file "iterate" :depends-on ("package"))))

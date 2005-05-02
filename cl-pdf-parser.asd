;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package asdf)

;comment this out if you want to use the Lispworks parser instead of cl-yacc
(pushnew :use-cl-yacc *features*)

#+use-cl-yacc
(load (merge-pathnames "cl-yacc/yacc.asd" *load-truename*))

(defsystem :cl-pdf-parser
  :name "cl-pdf-parser"
  :author "Marc Battyani <marc.battyani@fractalconcept.com>"
  :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
  :description "PDF parser"
  :long-description "PDF parser"
  :components ((:file "pdf-parser" :depends-on ()))
  :depends-on (:cl-pdf :yacc)
  )

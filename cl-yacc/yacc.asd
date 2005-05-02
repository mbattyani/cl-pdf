;;; -*-Lisp-*-

(asdf:defsystem #:yacc
  :name "yacc"
  :author "Juliusz Chroboczek <jch@pps.jussieu.fr>"
  :licence "MIT/X11"
  :description "A LALR(1) parser generator for Common Lisp"
  
  :components ((:file "yacc"))
  )

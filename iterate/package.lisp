;;; Some of the 7 extremely random interface commands:

;;; use strings here so that there's no iterate symbol in USER.

(defpackage #:iterate
  (:use #:cl)
  (:nicknames #:ITER)
  (:export #:iterate #:iter #:display-iterate-clauses
	   #:defsynonym #:dsetq #:declare-variables
	   #:defmacro-clause #:defmacro-driver #:defclause-sequence))

(in-package #:iterate)

;;; work around sbcl's obnoxious standard compliance

(defmacro defconst (name value &optional (doc ""))
   `(eval-when (:compile-toplevel :load-toplevel :execute)
      (unless (boundp ',name)
        ,(if doc
             `(defconstant ,name ,value ,doc)
           `(defconstant ,name ,value)))))

(defconst +clause-names+
  '(initially after-each finally else if-first-time
    finally-protected
    finish leave next-iteration next terminate
    repeat for as generate generating in
    sum summing multiply multiplying
    maximize minimize maximizing minimizing counting
    always never thereis finding collect collecting
    with while until adjoining nconcing appending
    nunioning unioning reducing accumulate accumulating))

(eval-when (:compile-toplevel :load-toplevel :evaluate)
  (export +clause-names+ '#:iterate))

;;; arch-tag: "b8bb0bb6-313c-11d8-abb9-000c76244c24"

cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

CL-PDF is a cross-platform Common Lisp library for generating PDF files.
It does not need any third-party tools from Adobe or others.
It is used by cl-typesetting to provide a complete typesetting system.

It is released with a FreeBSD style license so it is usable for commercial work.

Currently there is no docs.
To install it:
   1 Customize config.lisp
   2 Get UFFI and install it if you want to use zlib compression on the pdf files.
     Or get an implementation specific zlib in the contrib directory.
     Or disable the compression in config.lisp
   3 Load the cl-pdf library using the asdf or mk:defsystem files.
   4 Use it... You can look at exemples.lisp and at pdf-utils.lisp to see how to use it.

In case of problems, first disable the zlib compression and try again...
You need to get a post 23-dec-2002 UFFI version.

Please contact me if you have some time to help.
For comments, bugs, typos, etc... e-mail to: marc.battyani@fractalconcept.com

You can look at some CL-PDF examples here:
http://www.fractalconcept.com/asp/html/e-cl-pdf.html


Marc Battyani

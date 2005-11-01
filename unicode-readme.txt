;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

Unicode readme

To use Unicode fonts you need to have the font metrics and the character
encoding of the font. For this you must have the .afm and .ufm font metrics
files. As generally these files don't exist for TrueType fonts you have to
generate them with an utility like afm2pt1. You can get it here:
http://ttf2pt1.sourceforge.net/
I've put a windows executable here:
http://www.fractalconcept.com/fcweb/download/ttf2pt1.zip

Use it like this (using the times font as an example):
ttf2pt1 -a -F times.ttf times
This will generate the needed times.afm and times.ufm files.

You can then load the font:
(pdf:load-ttu-font #P"times.ufm" #P"times.ttf")

And now you can use it:
PDF 222 > (pdf:get-font "TimesNewRomanPSMT")
#<pdf:font timesnewromanpsmt 25558C54>

To see the exact name of the font, just look inside the .afm file.

If you only need some characters of a font, you can use fontforge to tweak it.

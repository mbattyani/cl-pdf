;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

Unicode readme

To use Unicode fonts you need to have the font metrics and the character
encoding of the font. For this you must have an unicode font metrics (.ufm file).

The easiest way to do that is to use the ttx tool from https://github.com/fonttools/fonttools
to convert the ttf font file into xml then use the contrib/ttx2ufm.pl script to convert it into an
ufm file as explained below:

#  Convert ttx to ufm
#  Erik Ronström 2019
#
#  cl-pdf uses an extended version of the afm file format to read font metrics
#  for unicode fonts. The ufm files can be produced from ttf files using a
#  special version of ttf2pt1, but unfortunately ttf2pt1 is old and cannot
#  handle many newer fonts well. For example, it fails to load kern Apple-flavoured
#  kern tables, and it prints 'Unknown' as font name for many fonts.
#
#  The ttx tool, on the other hand, seems to do a much better job in parsing
#  ttf files, so the idea is to use that and convert the resulting ttx files
#  info ufm files.
#
#  Usage:
#  ttx Times.ttf
#  ttx2ufm Times.ttx > Times.ufm

You can then load the font:
(pdf:load-ttu-font #P"times.ufm" #P"times.ttf")

And now you can use it:
PDF 222 > (pdf:get-font "TimesNewRomanPSMT")
#<pdf:font timesnewromanpsmt 25558C54>

To see the exact name of the font, just look inside the .ufm file.

If you only need some characters of a font, you can use fontforge to tweak it.

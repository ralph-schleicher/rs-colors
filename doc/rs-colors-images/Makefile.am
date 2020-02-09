## Makefile.am --- make file for RS-COLORS.

# Copyright (C) 2020 Ralph Schleicher

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#
#    * The name of the author may not be used to endorse or promote
#      products derived from this software without specific prior
#      written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
# IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

## Code:

imagedir = $(infodir)/rs-colors-images
image_DATA = rs-colors-conversion.png srgb-color-cube-RGB-views.png \
srgb-color-cube-HSV-views.png srgb-color-cube-HSL-views.png \
srgb-color-cube-CIERGB-views.png srgb-color-cube-CIEXYZ-views.png \
srgb-color-cube-CIExyY-views.png srgb-color-cube-CIELuv-views.png \
srgb-color-cube-CIELab-views.png

EXTRA_DIST = rs-colors-conversion.tex rs-colors-conversion.pdf

$(srcdir)/rs-colors-conversion.png: $(srcdir)/rs-colors-conversion.pdf
	cd $(srcdir) && \
	convert -trim -density 96 rs-colors-conversion.pdf rs-colors-conversion.png

$(srcdir)/rs-colors-conversion.pdf: $(srcdir)/rs-colors-conversion.tex
	cd $(srcdir) && \
	pdflatex -halt-on-error -file-line-error rs-colors-conversion.tex && \
	pdfcrop rs-colors-conversion.pdf rs-colors-conversion.pdf && \
	rm -f rs-colors-conversion.log rs-colors-conversion.aux

## Makefile.am ends here

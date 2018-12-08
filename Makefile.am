## Makefile.am --- make file for RS-COLORS.

# Copyright (C) 2014 Ralph Schleicher

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

SUBDIRS = doc

rs_colors_internal_sources = \
rs-colors-internal.asd \
rs-colors-internal.lisp \
utilities.lisp \
$(nil)

rs_colors_sources = \
rs-colors.asd \
rs-colors.lisp \
types.lisp \
generic-rgb.lisp \
generic-cmy.lisp \
generic-cmyk.lisp \
cie-xyz.lisp \
cie-xyy.lisp \
cie-white-points.lisp \
cie-luv.lisp \
cie-lab.lisp \
cie-lch.lisp \
cie-rgb.lisp \
srgb.lisp \
adobe-rgb.lisp \
io.lisp \
color-difference.lisp \
$(nil)

nobase_dist_rs_colors_DATA = \
$(rs_colors_internal_sources) \
$(rs_colors_sources) \
$(nil)

BUILT_SOURCES = rs-colors-internal.asd rs-colors.asd

rs-colors-internal.asd: $(srcdir)/rs-colors-internal.asd.in $(srcdir)/Makefile.in
	cd $(srcdir) && \
	( sed -e 's/\@VERSION\@/$(PACKAGE_VERSION)/g' rs-colors-internal.asd.in | \
	  sh ./asd-components.sh $(rs_colors_internal_sources) ) > $@~ && \
	mv -f $@~ $@

rs-colors.asd: $(srcdir)/rs-colors.asd.in $(srcdir)/Makefile.in
	cd $(srcdir) && \
	( sed -e 's/\@VERSION\@/$(PACKAGE_VERSION)/g' rs-colors.asd.in | \
	  sh ./asd-components.sh $(rs_colors_sources) ) > $@~ && \
	mv -f $@~ $@

.PHONY: sync
sync: all
	~/src/github/github.sh rs-colors

## Makefile.am ends here

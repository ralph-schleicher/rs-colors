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

EXTRA_DIST = rs-colors.pdf

info_TEXINFOS = rs-colors.texi
rs_colors_TEXINFOS = package.texi rs-texinfo.texi

$(srcdir)/package.texi: $(srcdir)/../configure.ac
	cd $(srcdir) && \
	( echo 'set PACKAGE $(PACKAGE_NAME)' ; \
	  echo 'set VERSION $(PACKAGE_VERSION)' ; \
	  echo 'set ADDRESS $(PACKAGE_BUGREPORT)' ; \
	  echo 'set TARNAME $(PACKAGE_TARNAME)' ) | \
	sed -e 's,@,@@,g' -e 's,^,@,' > package.texi

dot: $(srcdir)/color-conversion.png
$(srcdir)/color-conversion.png: $(srcdir)/color-conversion.dot
	dot   -Tpng -o $(srcdir)/color-conversion.dot.png   $(srcdir)/color-conversion.dot
	neato -Tpng -o $(srcdir)/color-conversion.neato.png $(srcdir)/color-conversion.dot
	twopi -Tpng -o $(srcdir)/color-conversion.twopi.png $(srcdir)/color-conversion.dot
	circo -Tpng -o $(srcdir)/color-conversion.circo.png $(srcdir)/color-conversion.dot
	fdp   -Tpng -o $(srcdir)/color-conversion.fdp.png   $(srcdir)/color-conversion.dot

## Makefile.am ends here

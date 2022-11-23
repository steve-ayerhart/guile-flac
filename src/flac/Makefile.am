## Copyright (C) 2022 Steve Ayerhart <steve@ayerh.art>
##
## This file is part of Guile-FLAC.
##
## Guile-FLAC is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-FLAC is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-FLAC.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/am/guile.am

SOURCES = \
	version.scm \
	decover.scm \
	format.scm \
	metadata.scm \
	reader.scm

EXTRA_DIST += \
	version.scm.in

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/flac/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/flac/

SUFFIXES = .in

substitute = sed -e 's,[@]VERSION[@],$(PACKAGE_VERSION),g'

version.scm: version.scm.in
	@echo "  GEN      $@"
	@$(substitute) "$^" > "$@"

CLEANFILES += \
	version.scm

## Makefile.am ends here.

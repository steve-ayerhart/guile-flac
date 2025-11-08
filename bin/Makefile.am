## Makefile.am for gflac binaries
##
## Copyright (C) 2025 Steve Ayerhart <steve@ayerh.art>
##
## This file is part of Guile-FLAC.
##
## Guile-FLAC is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.

bin_SCRIPTS = gflac

do_subst = sed \
  -e 's|[@]GUILE_SITEDIR[@]|$(guilesitedir)|g'

gflac: gflac.in Makefile
	$(do_subst) < $(srcdir)/gflac.in > $@
	chmod +x $@

CLEANFILES = gflac
EXTRA_DIST = gflac.in

## Makefile.am ends here

flaclib_LTLIBRARIES = flac.la

AM_CFLAGS = -I. $(WARN_CFLAGS) $(DEBUG_FLAGS) -fPIC

C_FILES = \
	metadata.c

H_FILES = $(C_FILES:.c=.h)
X_FILES = $(C_FILES:.c=.x)

flac_la_SOURCES = $(C_FILES) $(H_FILES)

flac_la_CFLAGS = $(GUILE_CFLAGS) $(FLAC_CFLAGS) $(AM_CFLAGS)
flac_la_LIBADD = $(GUILE_LIBS) $(FLAC_LIBS)
flac_la_LDFLAGS = -module -avoid-version

MAINTERCLEANFILES = Makefile.in

SUFFIXES = .x .c .scm

.c.x:
	guile-snarf $(flac_la_CFLAGS) $< > $@ || { rm $@; false; }

BUILT_SOURCES = $(X_FILES) flac/config.scm

flaclibdir = $(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/extensions

GOBJECTS = $(SOURCES:%.scm=%.go) flac.la

flac/config.scm: Makefile flac/config.scm.in
	mkdir -p flac
	sed -e "s|@flaclibdir\@|$(flaclibdir)|" \
		$(srcdir)/flac/config.scm.in > flac/config.scm

EXTRA_DIST = $(SOURCES)
GUILE_WARNINGS = -Wunbound-variable Warity-mismatch -Wformat

moddir = $(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir = $(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/ccache

SOURCES = \
	flac.scm \
	flac/metadata.scm

CLEANFILES = $(GOBJECTS) $(X_FILES) flac/config.scm

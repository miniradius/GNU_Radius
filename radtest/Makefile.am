# This file is part of GNU Radius.
# Copyright (C) 2000-2025 Free Software Foundation, Inc.
#
# Written by Sergey Poznyakoff
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# GNU Radius is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

bin_PROGRAMS    = radtest
radtest_SOURCES = main.c gram.c input.c radtest.h runtime.c completion.c
radtest_LDADD   = @LIBGNURADIUS@ @LIBRADEXTRA@ ../lib/libradextra.a @READLINE_LIBS@ @LIBINTL@
noinst_HEADERS  = gram.h
AM_CPPFLAGS     = @RADIUS_INCLUDE_PATH@ -I$(srcdir) 

BUILT_SOURCES = gram.c gram.h input.c
EXTRA_DIST    = gram.y input.l

gram.c gram.h: gram.y
input.c: input.l

AM_YFLAGS = -vdt 
.l.c:
	$(FLEX) -o $@ $(AM_LFLAGS) $<
.y.c:
	$(BISON) -o $@ $(AM_YFLAGS) $<

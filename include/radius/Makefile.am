# This file is part of GNU Radius.
# Copyright (C) 2004-2025 Free Software Foundation, Inc.
#
# Written by Sergey Poznyakoff
#
# GNU Radius is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# GNU Radius is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Radius.  If not, see <http://www.gnu.org/licenses/>. 

all: dictionary.h

dictionary.h: $(top_srcdir)/raddb/dict/standard
	$(AWK) -f $(top_srcdir)/scripts/dict.awk $(top_srcdir)/raddb/dict/standard > dictionary.h

pkginclude_HEADERS = \
 argcv.h\
 debug.h\
 dictionary.h\
 envar.h\
 list.h\
 md4.h\
 md5.h\
 mem.h\
 raddbm.h\
 radius.h\
 radpaths.h\
 radscm.h\
 radutmp.h\
 sha1.h\
 strbuf.h\
 symtab.h\
 types.h\
 version.h

EXTRA_DIST = version.h.in
MAINTAINERCLEANFILES = dictionary.h


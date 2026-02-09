# parseopt - generic option parser library         -*- autoconf -*-
# Copyright (C) 2023-2025 Sergey Poznyakoff
#
# Parseopt is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# Parseopt is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with parseopt. If not, see <http://www.gnu.org/licenses/>.

# _PARSEOPT_MANGLE_OPTION(NAME)
# -------------------------
# Convert NAME to a valid m4 identifier, by replacing invalid characters
# with underscores, and prepend the _PARSEOPT_OPTION_ suffix to it.
m4_define([_PARSEOPT_MANGLE_OPTION],
[[_PARSEOPT_OPTION_]m4_bpatsubst($1, [[^a-zA-Z0-9_]], [_])])

# _PARSEOPT_GET_OPTION(OPTION)
# ----------------------------
# Expand to the value of OPTION, if such is defined, and to an empty string
# otherwise.
m4_define([_PARSEOPT_GET_OPTION],
[_PARSEOPT_IF_OPTION_SET([$1],[m4_defn(_PARSEOPT_MANGLE_OPTION([$1]))])])

m4_define([_PARSEOPT_DEFINE_OPTION],
[m4_define(_PARSEOPT_MANGLE_OPTION([$1]),[$2])])

# _PARSEOPT_SET_OPTION(NAME[=VAL])
# --------------------------------
# Set option NAME to VAL.
m4_define([_PARSEOPT_SET_OPTION],
[_PARSEOPT_DEFINE_OPTION(m4_if(m4_index([$1],[=]),-1,[[$1]],[m4_bpatsubst([$1],[\([^=]*\)=\(.*\)],[[\1],[\2]])]))])

# _PARSEOPT_IF_OPTION_SET(NAME,IF-SET,IF-NOT-SET)
# -----------------------------------------------
# Check if option NAME is set.
m4_define([_PARSEOPT_IF_OPTION_SET],
[m4_ifset(_PARSEOPT_MANGLE_OPTION([$1]),[$2],[$3])])

# _PARSEOPT_IF_OPTION_DEF(NAME,IF-SET,IF-NOT-SET)
# -----------------------------------------------
# Check if option NAME is set.
m4_define([_PARSEOPT_IF_OPTION_DEF],
[m4_ifdef(_PARSEOPT_MANGLE_OPTION([$1]),[$2],[$3])])

# _PARSEOPT_OPTION_SWITCH(NAME1,IF-SET1,[NAME2,IF-SET2,[...]],[IF-NOT-SET])
# -------------------------------------------------------------------------
# If NAME1 is defined, run IF-SET1.  Otherwise, if NAME2 is defined, run
# IF-SET2. Continue the process for all name-if-set pairs within [...].
# If none of the options is set, run IF-NOT-SET.
m4_define([_PARSEOPT_OPTION_SWITCH],
[m4_if([$4],,[_PARSEOPT_IF_OPTION_DEF($@)],dnl
[$3],,[_PARSEOPT_IF_OPTION_DEF($@)],dnl
[_PARSEOPT_IF_OPTION_DEF([$1],[$2],dnl
[_PARSEOPT_OPTION_SWITCH(m4_shift2($@))])])])

# _PARSEOPT_SET_OPTIONS(OPTION[,OPTION...])
# -----------------------------------------
m4_define([_PARSEOPT_SET_OPTIONS],
[m4_if([$1],,,[_PARSEOPT_SET_OPTION([$1])_PARSEOPT_SET_OPTIONS(m4_shift($@))])])

# PARSEOPT_SETUP([subdir],[flag[, flag...])
# -----------------------------------------
# Parameters:
#   subdir   Submodule directory.  Defaults to parseopt.
#   flags    Each flag is either NAME or NAME=VALUE
#
# Following flags are defined:
#   static   Build a static convenience library libparseopt.a.  This is the
#            default.
#   shared   Build a libtool shared convenience library libparseopt.la
#   install  Build installable shared (or static) library.  This is not
#            implemented so far.
#   tests    Build parseopt testsuite
#   CPPFLAGS=FLAGS
#            Add FLAGS to the list of cpp flags used when building
#            libparseopt
#   LIBADD=LIBS
#            Pass LIBS to the ldd command line when building shared
#            version of libparseopt.
#
# This macro defines the following substitution variables for use in
# Makefile.am files:
#
#   PARSEOPT_DIR       Submodule directory as defined by the `subdir'
#                      parameter.
#   PARSEOPT_BUILD_TYPE
#                      Build type (one of: static, shared, install)
#   PARSEOPT_INCLUDES  Options to pass to cpp wheb compiling with parseopt.
#   PARSEOPT_LDADD     Additional arguments to pass to ldd when linking with
#                      parseopt.

AC_DEFUN([PARSEOPT_SETUP],[
  m4_pushdef([parseoptdir],m4_if($1,[.],,$1,,[parseopt/],$1/))
  _PARSEOPT_SET_OPTIONS(m4_shift($@))
  AC_SUBST([PARSEOPT_DIR],[parseoptdir])
  AC_SUBST([PARSEOPT_INCLUDES],['-I$(top_srcdir)/]parseoptdir')
  m4_pushdef([parseopt_test],[_PARSEOPT_IF_OPTION_DEF([tests],[test])])
  AC_SUBST([PARSEOPT_LDADD],['-L$(top_builddir)/]parseoptdir -lparseopt')
  AC_SUBST([PARSEOPT_EXTRA_CPPFLAGS],[_PARSEOPT_GET_OPTION([CPPFLAGS])])
  AC_SUBST([PARSEOPT_EXTRA_LIBADD],[_PARSEOPT_GET_OPTION([LIBADD])])
  AC_SUBST([PARSEOPT_BUILD_TYPE])
  _PARSEOPT_OPTION_SWITCH(
  [install],[
    LT_INIT
    PARSEOPT_BUILD_TYPE=install
    PARSEOPT_LDADD=['$(top_builddir)/]parseoptdir[libparseopt.la']
    AC_CONFIG_FILES(parseoptdir[Makefile]:parseoptdir[mk/install]parseopt_test[.in])],
  [shared],[
    LT_INIT
    PARSEOPT_BUILD_TYPE=shared
    PARSEOPT_LDADD=['$(top_builddir)/]parseoptdir[libparseopt.la']
    AC_CONFIG_FILES(parseoptdir[Makefile]:parseoptdir[mk/shared]parseopt_test[.in])],
  [
    AC_PROG_RANLIB
    PARSEOPT_BUILD_TYPE=static
    PARSEOPT_LDADD=['$(top_builddir)/]parseoptdir[libparseopt.a']
    AC_CONFIG_FILES(parseoptdir[Makefile]:parseoptdir[mk/static]parseopt_test[.in])])
  m4_if(parseopt_test,[],[],[
  AC_CONFIG_TESTDIR(parseoptdir[t])
  AC_CONFIG_FILES(parseoptdir[t/Makefile])
  AM_MISSING_PROG([AUTOM4TE], [autom4te])])

  m4_popdef([parseopt_test])
  m4_popdef([parseoptdir])
])

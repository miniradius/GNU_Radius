# -*- buffer-read-only: t -*- vi: set ro:
# DO NOT EDIT! GENERATED AUTOMATICALLY!
# DO NOT EDIT! GENERATED AUTOMATICALLY!
# Copyright (C) 2002-2008 Free Software Foundation, Inc.
#
# This file is free software, distributed under the terms of the GNU
# General Public License.  As a special exception to the GNU General
# Public License, this file may be distributed as part of a program
# that contains a configuration script generated by Autoconf, under
# the same distribution terms as the rest of that program.
#
# Generated by gnulib-tool.
#
# This file represents the compiled summary of the specification in
# gnulib-cache.m4. It lists the computed macro invocations that need
# to be invoked from configure.ac.
# In projects using CVS, this file can be treated like other built files.


# This macro should be invoked from ./configure.ac, in the section
# "Checks for programs", right after AC_PROG_CC, and certainly before
# any checks for libraries, header files, types and library functions.
AC_DEFUN([gl_EARLY],
[
  m4_pattern_forbid([^gl_[A-Z]])dnl the gnulib macro namespace
  m4_pattern_allow([^gl_ES$])dnl a valid locale name
  m4_pattern_allow([^gl_LIBOBJS$])dnl a variable
  m4_pattern_allow([^gl_LTLIBOBJS$])dnl a variable
  AC_REQUIRE([AC_PROG_RANLIB])
  AC_REQUIRE([AC_GNU_SOURCE])
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
])

# This macro should be invoked from ./configure.ac, in the section
# "Check for header files, types and library functions".
AC_DEFUN([gl_INIT],
[
  AM_CONDITIONAL([GL_COND_LIBTOOL], [true])
  gl_cond_libtool=true
  m4_pushdef([AC_LIBOBJ], m4_defn([gl_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([gl_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([gl_LIBSOURCES]))
  m4_pushdef([gl_LIBSOURCES_LIST], [])
  m4_pushdef([gl_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='gnu'
changequote(,)dnl
LTALLOCA=`echo "$ALLOCA" | sed 's/\.[^.]* /.lo /g;s/\.[^.]*$/.lo/'`
changequote([, ])dnl
AC_SUBST([LTALLOCA])
  gl_FUNC_ALLOCA
  gl_ARGP
  m4_ifdef([AM_XGETTEXT_OPTION],
    [AM_XGETTEXT_OPTION([--flag=argp_error:2:c-format])
     AM_XGETTEXT_OPTION([--flag=argp_failure:4:c-format])])
  gl_DIRNAME
  gl_DOUBLE_SLASH_ROOT
  gl_HEADER_ERRNO_H
  gl_ERROR
  m4_ifdef([AM_XGETTEXT_OPTION],
    [AM_XGETTEXT_OPTION([--flag=error:3:c-format])
     AM_XGETTEXT_OPTION([--flag=error_at_line:5:c-format])])
  gl_EXITFAIL
  gl_FLOAT_H
  gl_FUNC_GETDELIM
  gl_STDIO_MODULE_INDICATOR([getdelim])
  gl_FUNC_GETLINE
  gl_STDIO_MODULE_INDICATOR([getline])
  gl_GETOPT
  dnl you must add AM_GNU_GETTEXT([external]) or similar to configure.ac.
  AM_GNU_GETTEXT_VERSION([0.17])
  AC_SUBST([LIBINTL])
  AC_SUBST([LTLIBINTL])
  gl_INLINE
  gl_LOCALCHARSET
  LOCALCHARSET_TESTS_ENVIRONMENT="CHARSETALIASDIR=\"\$(top_builddir)/$gl_source_base\""
  AC_SUBST([LOCALCHARSET_TESTS_ENVIRONMENT])
  AC_FUNC_MALLOC
  AC_DEFINE([GNULIB_MALLOC_GNU], 1, [Define to indicate the 'malloc' module.])
  gl_FUNC_MALLOC_POSIX
  gl_STDLIB_MODULE_INDICATOR([malloc-posix])
  gl_FUNC_MEMPCPY
  gl_STRING_MODULE_INDICATOR([mempcpy])
  AC_FUNC_OBSTACK
  dnl Note: AC_FUNC_OBSTACK does AC_LIBSOURCES([obstack.h, obstack.c]).
  gl_FUNC_RAWMEMCHR
  gl_STRING_MODULE_INDICATOR([rawmemchr])
  gl_FUNC_REALLOC_POSIX
  gl_STDLIB_MODULE_INDICATOR([realloc-posix])
  gl_REGEX
  gl_SIZE_MAX
  gl_FUNC_SLEEP
  gl_UNISTD_MODULE_INDICATOR([sleep])
  gt_TYPE_SSIZE_T
  AM_STDBOOL_H
  gl_STDINT_H
  gl_STDIO_H
  gl_STDLIB_H
  gl_STRCASE
  gl_FUNC_STRCHRNUL
  gl_STRING_MODULE_INDICATOR([strchrnul])
  gl_FUNC_STRERROR
  gl_STRING_MODULE_INDICATOR([strerror])
  gl_HEADER_STRING_H
  gl_HEADER_STRINGS_H
  gl_FUNC_STRNDUP
  gl_STRING_MODULE_INDICATOR([strndup])
  gl_FUNC_STRNLEN
  gl_STRING_MODULE_INDICATOR([strnlen])
  gl_SYSEXITS
  gl_UNISTD_H
  gl_FUNC_VASNPRINTF
  gl_FUNC_VASPRINTF
  gl_STDIO_MODULE_INDICATOR([vasprintf])
  m4_ifdef([AM_XGETTEXT_OPTION],
    [AM_XGETTEXT_OPTION([--flag=asprintf:2:c-format])
     AM_XGETTEXT_OPTION([--flag=vasprintf:2:c-format])])
  gl_FUNC_VSNPRINTF
  gl_STDIO_MODULE_INDICATOR([vsnprintf])
  gl_WCHAR_H
  gl_WCTYPE_H
  gl_XALLOC
  gl_XSIZE
  gl_XSTRNDUP
  m4_ifval(gl_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([gl_LIBSOURCES_DIR])[ ||
      for gl_file in ]gl_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([gl_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([gl_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([gl_LIBSOURCES_DIR])
  m4_popdef([gl_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    gl_libobjs=
    gl_ltlibobjs=
    if test -n "$gl_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $gl_LIBOBJS; do echo "$i"; done | sed "$sed_drop_objext" | sort | uniq`; do
        gl_libobjs="$gl_libobjs $i.$ac_objext"
        gl_ltlibobjs="$gl_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([gl_LIBOBJS], [$gl_libobjs])
    AC_SUBST([gl_LTLIBOBJS], [$gl_ltlibobjs])
  ])
  gltests_libdeps=
  gltests_ltlibdeps=
  m4_pushdef([AC_LIBOBJ], m4_defn([gltests_LIBOBJ]))
  m4_pushdef([AC_REPLACE_FUNCS], m4_defn([gltests_REPLACE_FUNCS]))
  m4_pushdef([AC_LIBSOURCES], m4_defn([gltests_LIBSOURCES]))
  m4_pushdef([gltests_LIBSOURCES_LIST], [])
  m4_pushdef([gltests_LIBSOURCES_DIR], [])
  gl_COMMON
  gl_source_base='tests'
  m4_ifval(gltests_LIBSOURCES_LIST, [
    m4_syscmd([test ! -d ]m4_defn([gltests_LIBSOURCES_DIR])[ ||
      for gl_file in ]gltests_LIBSOURCES_LIST[ ; do
        if test ! -r ]m4_defn([gltests_LIBSOURCES_DIR])[/$gl_file ; then
          echo "missing file ]m4_defn([gltests_LIBSOURCES_DIR])[/$gl_file" >&2
          exit 1
        fi
      done])dnl
      m4_if(m4_sysval, [0], [],
        [AC_FATAL([expected source file, required through AC_LIBSOURCES, not found])])
  ])
  m4_popdef([gltests_LIBSOURCES_DIR])
  m4_popdef([gltests_LIBSOURCES_LIST])
  m4_popdef([AC_LIBSOURCES])
  m4_popdef([AC_REPLACE_FUNCS])
  m4_popdef([AC_LIBOBJ])
  AC_CONFIG_COMMANDS_PRE([
    gltests_libobjs=
    gltests_ltlibobjs=
    if test -n "$gltests_LIBOBJS"; then
      # Remove the extension.
      sed_drop_objext='s/\.o$//;s/\.obj$//'
      for i in `for i in $gltests_LIBOBJS; do echo "$i"; done | sed "$sed_drop_objext" | sort | uniq`; do
        gltests_libobjs="$gltests_libobjs $i.$ac_objext"
        gltests_ltlibobjs="$gltests_ltlibobjs $i.lo"
      done
    fi
    AC_SUBST([gltests_LIBOBJS], [$gltests_libobjs])
    AC_SUBST([gltests_LTLIBOBJS], [$gltests_ltlibobjs])
  ])
])

# Like AC_LIBOBJ, except that the module name goes
# into gl_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([gl_LIBOBJ], [
  AS_LITERAL_IF([$1], [gl_LIBSOURCES([$1.c])])dnl
  gl_LIBOBJS="$gl_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into gl_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([gl_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [gl_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([gl_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([gl_LIBSOURCES_DIR], [gnu])
      m4_append([gl_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# Like AC_LIBOBJ, except that the module name goes
# into gltests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([gltests_LIBOBJ], [
  AS_LITERAL_IF([$1], [gltests_LIBSOURCES([$1.c])])dnl
  gltests_LIBOBJS="$gltests_LIBOBJS $1.$ac_objext"
])

# Like AC_REPLACE_FUNCS, except that the module name goes
# into gltests_LIBOBJS instead of into LIBOBJS.
AC_DEFUN([gltests_REPLACE_FUNCS], [
  m4_foreach_w([gl_NAME], [$1], [AC_LIBSOURCES(gl_NAME[.c])])dnl
  AC_CHECK_FUNCS([$1], , [gltests_LIBOBJ($ac_func)])
])

# Like AC_LIBSOURCES, except the directory where the source file is
# expected is derived from the gnulib-tool parameterization,
# and alloca is special cased (for the alloca-opt module).
# We could also entirely rely on EXTRA_lib..._SOURCES.
AC_DEFUN([gltests_LIBSOURCES], [
  m4_foreach([_gl_NAME], [$1], [
    m4_if(_gl_NAME, [alloca.c], [], [
      m4_define([gltests_LIBSOURCES_DIR], [tests])
      m4_append([gltests_LIBSOURCES_LIST], _gl_NAME, [ ])
    ])
  ])
])

# This macro records the list of files which have been installed by
# gnulib-tool and may be removed by future gnulib-tool invocations.
AC_DEFUN([gl_FILE_LIST], [
  build-aux/config.rpath
  build-aux/gitlog-to-changelog
  build-aux/link-warning.h
  lib/alloca.c
  lib/alloca.in.h
  lib/argp-ba.c
  lib/argp-eexst.c
  lib/argp-fmtstream.c
  lib/argp-fmtstream.h
  lib/argp-fs-xinl.c
  lib/argp-help.c
  lib/argp-namefrob.h
  lib/argp-parse.c
  lib/argp-pin.c
  lib/argp-pv.c
  lib/argp-pvh.c
  lib/argp-xinl.c
  lib/argp.h
  lib/asnprintf.c
  lib/asprintf.c
  lib/basename.c
  lib/config.charset
  lib/dirname.c
  lib/dirname.h
  lib/errno.in.h
  lib/error.c
  lib/error.h
  lib/exitfail.c
  lib/exitfail.h
  lib/float+.h
  lib/float.in.h
  lib/getdelim.c
  lib/getline.c
  lib/getopt.c
  lib/getopt.in.h
  lib/getopt1.c
  lib/getopt_int.h
  lib/gettext.h
  lib/intprops.h
  lib/localcharset.c
  lib/localcharset.h
  lib/malloc.c
  lib/mempcpy.c
  lib/obstack.c
  lib/obstack.h
  lib/printf-args.c
  lib/printf-args.h
  lib/printf-parse.c
  lib/printf-parse.h
  lib/rawmemchr.c
  lib/rawmemchr.valgrind
  lib/realloc.c
  lib/ref-add.sin
  lib/ref-del.sin
  lib/regcomp.c
  lib/regex.c
  lib/regex.h
  lib/regex_internal.c
  lib/regex_internal.h
  lib/regexec.c
  lib/size_max.h
  lib/sleep.c
  lib/stdbool.in.h
  lib/stdint.in.h
  lib/stdio-write.c
  lib/stdio.in.h
  lib/stdlib.in.h
  lib/strcasecmp.c
  lib/strchrnul.c
  lib/strchrnul.valgrind
  lib/strerror.c
  lib/string.in.h
  lib/strings.in.h
  lib/stripslash.c
  lib/strncasecmp.c
  lib/strndup.c
  lib/strnlen.c
  lib/sysexits.in.h
  lib/unistd.in.h
  lib/vasnprintf.c
  lib/vasnprintf.h
  lib/vasprintf.c
  lib/vsnprintf.c
  lib/wchar.in.h
  lib/wctype.in.h
  lib/xalloc-die.c
  lib/xalloc.h
  lib/xmalloc.c
  lib/xsize.h
  lib/xstrndup.c
  lib/xstrndup.h
  m4/alloca.m4
  m4/argp.m4
  m4/codeset.m4
  m4/dirname.m4
  m4/dos.m4
  m4/double-slash-root.m4
  m4/errno_h.m4
  m4/error.m4
  m4/exitfail.m4
  m4/extensions.m4
  m4/float_h.m4
  m4/getdelim.m4
  m4/getline.m4
  m4/getopt.m4
  m4/gettext.m4
  m4/glibc2.m4
  m4/glibc21.m4
  m4/gnulib-common.m4
  m4/iconv.m4
  m4/include_next.m4
  m4/inline.m4
  m4/intdiv0.m4
  m4/intl.m4
  m4/intldir.m4
  m4/intlmacosx.m4
  m4/intmax.m4
  m4/intmax_t.m4
  m4/inttypes-pri.m4
  m4/inttypes_h.m4
  m4/lcmessage.m4
  m4/lib-ld.m4
  m4/lib-link.m4
  m4/lib-prefix.m4
  m4/localcharset.m4
  m4/lock.m4
  m4/longlong.m4
  m4/malloc.m4
  m4/mbstate_t.m4
  m4/mempcpy.m4
  m4/nls.m4
  m4/po.m4
  m4/printf-posix.m4
  m4/printf.m4
  m4/progtest.m4
  m4/rawmemchr.m4
  m4/realloc.m4
  m4/regex.m4
  m4/size_max.m4
  m4/sleep.m4
  m4/ssize_t.m4
  m4/stdbool.m4
  m4/stdint.m4
  m4/stdint_h.m4
  m4/stdio_h.m4
  m4/stdlib_h.m4
  m4/strcase.m4
  m4/strchrnul.m4
  m4/strerror.m4
  m4/string_h.m4
  m4/strings_h.m4
  m4/strndup.m4
  m4/strnlen.m4
  m4/sysexits.m4
  m4/threadlib.m4
  m4/uintmax_t.m4
  m4/unistd_h.m4
  m4/vasnprintf.m4
  m4/vasprintf.m4
  m4/visibility.m4
  m4/vsnprintf.m4
  m4/wchar.m4
  m4/wchar_t.m4
  m4/wctype.m4
  m4/wint_t.m4
  m4/xalloc.m4
  m4/xsize.m4
  m4/xstrndup.m4
])

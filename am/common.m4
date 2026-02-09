dnl RA_FLUSHLEFT -- remove all whitespace at the beginning of lines
dnl This is useful for c-code which may include cpp statements
dnl
define([RA_FLUSHLEFT],
 [changequote(`,')dnl
patsubst(`$1', `^[ 	]+')
changequote([,])])dnl

dnl RA_RESULT_ACTIONS -- generate shell code for the result of a test
dnl   $1 -- CVAR  -- cache variable to check
dnl   $2 -- NAME  -- if not empty, used to generate a default value TRUE:
dnl                  `AC_DEFINE(HAVE_NAME)'
dnl   $2 -- TRUE  -- what to do if the CVAR is not `no'
dnl   $3 -- FALSE -- what to do otherwise; defaults to `:'
dnl
AC_DEFUN([RA_RESULT_ACTIONS], [
[if test "$$1" != "" -a "$$1" != no; then
  ]ifelse([$3], ,
          [AC_DEFINE(HAVE_]translit($2, [a-z ./<>], [A-Z___])[,1,[FIXME])],
          [$3])[
else
  ]ifelse([$4], , [:], [$4])[
fi]])dnl

AC_SUBST(RADIUSD_LDADD_LIST)
AC_DEFUN([RA_RADIUSD_LDADD],
 RADIUSD_LDADD_LIST="$RADIUSD_LDADD_LIST [$1]")


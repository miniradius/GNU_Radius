/* This file is part of GNU Radius
   Copyright (C) 2000-2025 Free Software Foundation, Inc.

   Written by Sergey Poznyakoff

   GNU Radius is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GNU Radius is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Radius.  If not, see <http://www.gnu.org/licenses/>. */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <sys/types.h>
#include <stdio.h>
#include <radiusd.h>
#include <radcli.h>

static char *sys_def[] = {
#if defined(__alpha)
	"__alpha",
#endif
#if defined(__osf__)
	"__osf__",
#endif
#if defined(aix)
	"aix",
#endif
#if defined(bsdi)
	"bsdi",
#endif
#if defined(__FreeBSD__)
	"FreeBSD",
#endif
#if defined(__NetBSD__)
	"NetBSD",
#endif
#if defined(__OpenBSD__)
	"OpenBSD",
#endif
#if defined(sun)
	"sun",
#endif
#if defined(sys5)
	"sys5",
#endif
#if defined(unixware)
	"unixware",
#endif
#if defined(__linux__)
	"linux",
#endif
	NULL
};

static char *debug_flag_str[] = {
#if defined(EXTRA_DEBUG_MODE)
	"EXTRA_DEBUG_MODE",
#endif
	NULL
};

static char *compile_flag_str[] = {
#if defined(PWD_SHADOW)
# if PWD_SHADOW == SHADOW
	"PWD_SHADOW=SHADOW",
# else
	"PWD_SHADOW=OSFC2",
# endif
#endif
#if defined(USE_SERVER_GUILE)
	"USE_SERVER_GUILE",
#endif
#if defined(USE_PAM)
	"USE_PAM",
#endif
#if defined(USE_DBM)
# if USE_DBM == DBM_DBM
	"USE_DBM=DBM",
# elif USE_DBM == DBM_NDBM
	"USE_DBM=NDBM",
# endif
#endif /* USE_DBM */
#ifdef USE_SQL_MYSQL
	"USE_SQL_MYSQL",
#endif
#ifdef USE_SQL_POSTGRES
	"USE_SQL_POSTGRES",
#endif
#ifdef USE_SQL_ODBC
	"USE_SQL_ODBC",
#endif
#if defined(USE_SNMP)
# if defined(SNMP_COMPAT_0_96)
	"USE_SNMP=COMPAT_0_96",
# else
	"USE_SNMP",
# endif
#endif
#if defined(USE_LIVINGSTON_MENUS)
	"USE_LIVINGSTON_MENUS",
#endif
#if defined(DENY_SHELL)
	"DENY_SHELL",
#endif
#if defined(USE_LOADABLE_MODULES)
	"USE_LOADABLE_MODULES",
#endif
	NULL
};

char *server_id;

/* ************************************************************************ */

/* FIXME?:
 * Allow for expandable characters:
 *  %s - Server type (Auth/Acct/Stat)
 *  %v - Server version
 *  %h - hostname
 */
char *
make_server_ident(void)
{
	if (server_id)
		return grad_estrdup(server_id);
	else {
		const char *msg = _("GNU RADIUS server version ");
		int len = strlen(msg) + sizeof(VERSION);
		char *p = grad_emalloc(len);
		sprintf(p, "%s%s", msg, VERSION);
		return p;
	}
}

/*
 * Display the version number and built-in defaults.
 */
void
version_hook(WORDWRAP_FILE wf, struct parseopt *po)
{
	int i;

	radius_version_hook(wf, po);
	wordwrap_flush(wf);

	/*
	 * FIXME: The output below is supposed to be machine-radable, so
	 * no special alignment is needed. However, wordwrap makes no
	 * provisions for clearing right margin. It is also not possible
	 * to obtain file descriptor it is using. For the time being, let's
	 * assume it is always 0 and use stdio functions.
	 */
#ifdef BUILD_TARGET
	printf(_("Build target: %s\n"), BUILD_TARGET);
#endif
	printf("\n");

	printf(_("Compilation platform: "));
	for (i = 0; sys_def[i]; i++)
		printf("%s ", sys_def[i]);

	printf(_("\nDebugging flags: "));
	for (i = 0; debug_flag_str[i]; i++) {
		printf("%s ", debug_flag_str[i]);
	}

	printf(_("\nCompilation flags: "));
	for (i = 0; compile_flag_str[i]; i++) {
		printf("%s ", compile_flag_str[i]);
	}
	printf("\n");
	printf(_("Compilation defaults:\n"));
	printf(_("Ports in use:\n"));
	printf(" AUTH: %d\n", RADIUS_AUTH_PORT);
	printf(" ACCT: %d\n", RADIUS_ACCT_PORT);
	printf(_("Paths:\n"));
	printf(_(" configuration directory: %s\n"), RADIUS_DIR);
	printf(_(" logging directory:       %s\n"), RADLOG_DIR);
	printf(_(" accounting directory:    %s\n"), RADACCT_DIR);
	printf(_(" pidfile directory:       %s\n"), RADPID_DIR);
	printf(_("\nReport bugs to <%s>\n"), grad_bug_report_address);
}

void
show_compilation_defaults(void)
{
	int i;

	for (i = 0; compile_flag_str[i]; i++)
		printf("%s\n", compile_flag_str[i]);
}

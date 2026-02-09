/* This file is part of GNU Radius.
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

#if defined(HAVE_CONFIG_H)
# include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pwd.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <ctype.h>
#include <netinet/in.h>

#include <common.h>
#include <radius/radutmp.h>
#include <radcli.h>

void local_who();
void radius_who();
void print_header();
int want_rad_record(struct radutmp *rt);

/* UTMP stuff. Uses utmpx on svr4 */
#if defined(__svr4__) || defined(__sgi)
#  include <utmpx.h>
#  include <sys/fcntl.h>
#  define utmp utmpx
#  define UT_NAMESIZE   32
#  define UT_LINESIZE   32
#  define UT_HOSTSIZE   257
#  undef UTMP_FILE
#  define UTMP_FILE UTMPX_FILE
#  undef WTMP_FILE
#  define WTMP_FILE WTMPX_FILE
#else
#  include <utmp.h>
#endif
#ifdef __osf__
#  define UT_NAMESIZE   32
#  define UT_LINESIZE   32
#  define UT_HOSTSIZE   64
#endif
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(bsdi)
#  ifndef UTMP_FILE
#    define UTMP_FILE "/var/run/utmp"
#  endif
#  define ut_user ut_name
#endif

#define P_CONSOLE -1  /* Special radutmp type value for local users */

int  fingerd;             /* Are we run as fingerd */
int  secure;              /* Secure mode: do not answer queries w/o username */
int  showlocal;           /* Display local users as well */
int  display_header = 1;  /* Display header line */
int  showall;             /* Display all records */

char *username = NULL;

char *filename = NULL;    /* radutmp filename */
char *eol = "\n";         /* line delimiter */

static struct {
	char *name;
	char *fmt;
} fmtdef[] = {
	{ "default",
	  "(login 10 Login) (gecos 17 Name) (protocol 5 Proto) (nas-port 5 TTY) (time 9 When) (nas-address 9 From) (framed-address 16 Location)" },
	{ "sid",
	  "(login 10 Login) (session-id 17 SID) (protocol 5 Proto) (nas-port 5 TTY) (time 9 When) (nas-address 9 From) (framed-address 16 Location)" },
	{ "clid",
	  "(login 10 Login) (clid 17 CLID) (protocol 5 Proto) (nas-port 5 TTY) (time 9 When) (nas-address 9 From) (framed-address 16 Location)" },
	{ "long",
	  "(login 32 Login) (session-id 32 SID) (protocol 5 Proto) (nas-port 5 Port) (time 27 Date) (nas-address 32 NAS) (clid 17 CLID) (duration 7 Duration) (framed-address 16 Location) (realm 16 Realm)" },
	{ "gnu",
	  "User: (login)(newline)\
In real life: (gecos)(newline)\
Logged in: (time :format %c)(newline)\
NAS: (nas-address)(newline)\
Port: (nas-port)(newline)\
CLID: (clid)(newline)\
Protocol: (protocol)(newline)\
Session ID: (session-id)(newline)\
Uptime: (duration)(newline)\
Assigned IP: (framed-address)(newline)\
Realm: (realm)(newline)" },

	{ NULL }
};

static char *
lookup_format(char *name)
{
	int i;
	for (i = 0; fmtdef[i].name; i++)
		if (strcmp(fmtdef[i].name, name) == 0)
			return fmtdef[i].fmt;
	return name;
}

char *fmtspec = NULL;
format_data_t *form;

static int
optset_fmt(struct parseopt *po, struct optdef *opt, char *arg)
{
	fmtspec = lookup_format(arg);
	return 0;
}

static int
optset_fmt_clid(struct parseopt *po, struct optdef *opt, char *arg)
{
	fmtspec = lookup_format("clid");
	return 0;
}

static int
optset_fmt_sid(struct parseopt *po, struct optdef *opt, char *arg)
{
	fmtspec = lookup_format("sid");
	return 0;
}

static int
optset_fmt_long(struct parseopt *po, struct optdef *opt, char *arg)
{
	fmtspec = lookup_format("long");
	return 0;
}

static struct optdef options[] = {
	{
		.opt_flags = OPTFLAG_DOC,
		.opt_doc = N_("radwho specific switches:")
	},

	{
		.opt_name = "all",
		.opt_doc = N_("print all entries, not only active ones"),
		.opt_set = optset_incr,
		.opt_ptr = &showall
	},
	{
		.opt_name = "A",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "calling-id",
		.opt_doc = N_("display CLID in second column"),
		.opt_set = optset_fmt_clid
	},
	{
		.opt_name = "c",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "date-format",
		.opt_doc = N_("change date representation format"),
		.opt_argdoc = N_("DATEFMT"),
		.opt_set = optset_string_copy,
		.opt_ptr = &grad_printutmp_date_format
	},
	{
		.opt_name = "D",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "empty",
		.opt_doc = N_("print STRING instead of an empty column"),
		.opt_argdoc = N_("STRING"),
		.opt_set = optset_string_copy,
		.opt_ptr = &grad_printutmp_empty_string
	},
	{
		.opt_name = "e",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "file",
		.opt_doc = N_("Use FILE instead of /var/log/radutmp"),
		.opt_argdoc = N_("FILE"),
		.opt_set = optset_string_copy,
		.opt_ptr = &filename
	},
	{
		.opt_name = "f",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "finger",
		.opt_doc = N_("act as a finger daemon"),
		.opt_set = optset_true,
		.opt_ptr = &fingerd
	},
	{
		.opt_name = "F",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "no-header",
		.opt_doc = N_("do not display header line"),
		.opt_set = optset_false,
		.opt_ptr = &display_header
	},
	{
		.opt_name = "H",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "session-id",
		.opt_doc = N_("display session ID in the second column"),
		.opt_set = optset_fmt_sid
	},
	{
		.opt_name = "i",
		.opt_flags = OPTFLAG_ALIAS
	},
/*
	{
		.opt_name = "ip-strip-domain",
		.opt_doc = N_("display hostnames without domain part"),
	},
	{
		.opt_name = "I",
		.opt_flags = OPTFLAG_ALIAS
	},
*/
	{
		.opt_name = "long",
		.opt_doc = N_("Long output. All fields will be printed."),
		.opt_set = optset_fmt_long,
	},
	{
		.opt_name = "l",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "local-also",
		.opt_doc = N_("display also local users"),
		.opt_set = optset_true,
		.opt_ptr = &showlocal,
	},
	{
		.opt_name = "u",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "no-resolve",
		.opt_doc = N_("do not resolve hostnames."),
		.opt_set = optset_false,
		.opt_ptr = &grad_resolve_hostnames
	},
	{
		.opt_name = "n",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "format",
		.opt_doc = N_("change output format"),
		.opt_argdoc = N_("FORMAT"),
		.opt_set = optset_fmt
	},
	{
		.opt_name = "o",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "secure",
		.opt_doc = N_("secure mode: requires that the username be specified"),
		.opt_set = optset_true,
		.opt_ptr = &secure
	},
	{
		.opt_name = "s",
		.opt_flags = OPTFLAG_ALIAS
	},

	{ NULL }
}, *optdef[] = { options, NULL };

static struct parseopt po = {
	.po_descr = N_("display who is logged on by Radius"),
	.po_optdef = optdef,
};


int
main(int argc, char **argv)
{
	char inbuf[128];
	char *path;
	char *p, *q;

	set_progname(argv[0]);
	grad_app_setup();
	grad_parseopt(&po, argc, argv, NULL, NULL);

	if (!fmtspec)
		fmtspec = getenv("RADWHO_FORMAT");

	if (!fmtspec)
		fmtspec = lookup_format("default");

	form = grad_utent_compile_form(fmtspec);
	if (!form)
		exit(1);

	if (!filename)
		filename = grad_utmp_file;

	/* Read the dictionary files */
	grad_dict_init();
	/* Read the "naslist" file. */
	path = grad_mkfilename(grad_config_dir, RADIUS_NASLIST);
	if (grad_nas_read_file(path))
		exit(1);
	free(path);
	/* Read realms */
	path = grad_mkfilename(grad_config_dir, RADIUS_REALMS);
	grad_read_realms(path, 0, 0, NULL);
	free(path);

	/*
	 *      See if we are "fingerd".
	 */
	if (strstr(argv[0], "fingerd"))
		fingerd++;

	if (fingerd) {
		eol = "\r\n";
		/*
		 *      Read first line of the input.
		 */
		fgets(inbuf, sizeof(inbuf), stdin);
		p = inbuf;
		while(*p == ' ' || *p == '\t') p++;
		if (*p == '/' && *(p + 1)) p += 2;
		while(*p == ' ' || *p == '\t') p++;
		for(q = p; *q && *q != '\r' && *q != '\n'; q++)
			;
		*q = 0;
		if (*p)
			username = p;

		/*
		 *      See if we fingered a specific user.
		 */
		if (secure && username == 0) {
			printf(_("must provide username\n"));
			exit(1);
		}
	}

	if (showlocal)
		local_who();

	radius_who();

	fflush(stdout);
	fflush(stderr);

	return 0;
}

void
tty_to_port(struct radutmp *rt, char *tty)
{
	char *p;

	p = tty + strlen(tty) - 1;
	while (p >= tty && isdigit(*p))
		p--;
	rt->nas_port = atoi(p+1);
	rt->porttype = 0;/*FIXME*/
}

void
local_who(void)
{
	FILE  *fp;
	struct utmp ut;
	struct radutmp rt;

	if ((fp = fopen(UTMP_FILE, "r")) == NULL) {
		grad_log(GRAD_LOG_ERR, _("can't open file: %s"),
			 UTMP_FILE);
		return;
	}

	print_header();

	memset(&rt, 0, sizeof(rt));
	rt.nas_address = rt.framed_address = htonl(INADDR_LOOPBACK);

	while(fread(&ut, sizeof(ut), 1, fp) == 1) {
#ifdef USER_PROCESS
		if (ut.ut_user[0] && ut.ut_line[0] &&
		    ut.ut_type == USER_PROCESS) {
#else
		if (ut.ut_user[0] && ut.ut_line[0]) {
#endif
			rt.type = P_CONSOLE;
			strncpy(rt.login, ut.ut_name, RUT_NAMESIZE);
			strncpy(rt.orig_login, ut.ut_host, RUT_NAMESIZE);
#if defined __svr4__ || defined __sgi
			rt.time = ut.ut_xtime;
#else
			rt.time = ut.ut_time;
#endif
			tty_to_port(&rt, ut.ut_line);
			if (want_rad_record(&rt))
				grad_utent_print(form, &rt, 1);
		}
	}
	fclose(fp);
}

void
radius_who(void)
{
	radut_file_t file;
	struct radutmp *up;

	print_header();

	/*
	 *      Show the users logged in on the terminal server(s).
	 */
	if ((file = grad_ut_setent(filename, 0)) == NULL)
		return ;

	while ((up = grad_ut_getent(file)) != NULL) {
		if (up->type == P_ACCT_DISABLED)
			printf(_("System accounting is disabled\n"));
		else if (want_rad_record(up))
			grad_utent_print(form, up, 1);
	}
	grad_ut_endent(file);
}

void
print_header(void)
{
	if (display_header) {
		grad_utent_print_header(form);
		display_header = 0;
	}
}

int
want_rad_record(struct radutmp *rt)
{
	if ((username && strcmp(rt->login, username))
	    || rt->type == P_ACCT_ENABLED)
		return 0;

	switch (showall) {
	case 0:
		return rt->type != P_IDLE;
	case 1:
		return rt->login[0] != 0;
	case 2:
	default:
		return (rt->type == P_IDLE && rt->login[0] != 0);
	}
}

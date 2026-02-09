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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <netinet/in.h>
#include <limits.h>

#include <common.h>
#include <radcli.h>
#include <radius/radutmp.h>

#define LOCK_LEN sizeof(struct radutmp)

static int confirm_flag;
static char *utmpfile;
static char *nas;
static int port = -1;

static int
optset_port(struct parseopt *po, struct optdef *opt, char *arg)
{
	unsigned long n;
	char *p;
	if (*arg == 's' || *arg == 'S')
		++arg;
	errno = 0;
	n = strtoul(arg, &p, 10);
	if (errno || *p || n > INT_MAX) {
		po->po_error(po, PO_MSG_ERR, _("%s: invalid port"), arg);
		exit(po->po_ex_usage);
	}
	*(int*)opt->opt_ptr = n;
	return 0;
}

static struct optdef options[] = {
	{
		.opt_doc = N_("radzap specific switches:"),
		.opt_flags = OPTFLAG_DOC
	},
	{
		.opt_name = "confirm",
		.opt_doc = N_("ask for confirmation before zapping"),
		.opt_set = optset_true,
		.opt_ptr = &confirm_flag
	},
	{
		.opt_name = "c",
		.opt_flags = OPTFLAG_ALIAS
	},
	{
		.opt_name = "log-directory",
		.opt_doc = N_("set logging directory"),
		.opt_argdoc = N_("DIR"),
		.opt_set = optset_string_copy,
		.opt_ptr = &grad_log_dir
	},
	{
		.opt_name = "l",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "file",
		.opt_doc = N_("operate on FILE instead of /var/log/radutmp"),
		.opt_argdoc = N_("FILE"),
		.opt_set = optset_string_copy,
		.opt_ptr = &utmpfile
	},
	{
		.opt_name = "f",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "nas",
		.opt_doc = N_("zap user from given NAS"),
		.opt_argdoc = N_("NASNAME"),
		.opt_set = optset_string_copy,
		.opt_ptr = &nas,
	},
	{
		.opt_name = "n",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "port",
		.opt_doc = N_("zap user coming from given port"),
		.opt_argdoc = N_("NUMBER"),
		.opt_set = optset_port,
		.opt_ptr = &port
	},
	{
		.opt_name = "p",
		.opt_flags = OPTFLAG_ALIAS
	},

	{
		.opt_name = "quiet",
		.opt_doc = N_("do not ask for confirmation before zapping"),
		.opt_set = optset_false,
		.opt_ptr = &confirm_flag
	},
	{
		.opt_name = "q",
		.opt_flags = OPTFLAG_ALIAS
	},

	{ NULL }
}, *optdef[] = { options, NULL };

static struct parseopt po = {
	.po_descr = N_("delete Radius login records"),
	.po_optdef = optdef,
	.po_argdoc = N_("[OPTIONS] [USER]")
};

static int confirm(struct radutmp *);
static int write_wtmp(struct radutmp *);

/*
 *      Zap a user, or all users on a NAS, from the radutmp file.
 */
int
radzap(grad_netdef_t *netdef, int port, char *user, time_t t)
{
	struct radutmp  *up;
	radut_file_t    file;

	if (t == 0)
		time(&t);

	if ((file = grad_ut_setent(grad_utmp_file, 0)) == NULL) {
		grad_log(GRAD_LOG_ERR|GRAD_LOG_PERROR,
			 _("can't open file %s"), grad_utmp_file);
		exit(1);
	}
	/*
	 *      Find the entry for this NAS / portno combination.
	 */
	while ((up = grad_ut_getent(file)) != NULL) {
		if ((netdef
		     && !grad_ip_in_net_p(netdef, htonl(up->nas_address)))
		    || (port >= 0 && port != up->nas_port)
		    || (user != NULL && strcmp(up->login, user))!= 0
		    ||  up->type != P_LOGIN) {
			continue;
		}
		if (!confirm(up))
			continue;

		up->type = P_IDLE;
		up->time = t;
		grad_ut_putent(file, up);
		write_wtmp(up);
	}
	grad_ut_endent(file);

	return 0;
}

static int
confirm(struct radutmp *utp)
{
	char buf[GRAD_MAX_LONGNAME];
	grad_nas_t *cl;
	char *s = NULL;

	if ((cl = grad_nas_lookup_ip(ntohl(utp->nas_address))) != NULL)
		s = cl->shortname;
	if (s == NULL || s[0] == 0)
		s = grad_ip_gethostname(ntohl(utp->nas_address),
					buf, sizeof(buf));

	printf(_("radzap: zapping %s from %s, port %d"),
	       utp->login,
	       s,
	       utp->nas_port);
	if (confirm_flag) {
		printf(": Ok?");
		fgets(buf, sizeof(buf), stdin);
		if (buf[0] != 'y' && buf[0] != 'Y') {
			printf(_("Not confirmed\n"));
			return 0;
		} else
			return 1;
	}
	printf("\n");
	return 1;
}

static int
write_wtmp(struct radutmp *ut)
{
	return grad_radwtmp_putent(grad_wtmp_file, ut);
}

/*
 *      Zap a user from the radutmp and radwtmp file.
 */
int
main(int argc, char **argv)
{
	grad_netdef_t netdef, *netdefp = NULL;
	time_t  t;
	char    *path;
	char *s;

	set_progname(argv[0]);
	grad_app_setup();

	if ((s = getenv("RADZAP_CONFIRM")) != NULL)
		confirm_flag = atoi(s);
	grad_parseopt(&po, argc, argv, &argc, &argv);
	if (argc > 1) {
		grad_log(GRAD_LOG_CRIT, _("too many arguments"));
		exit(po.po_ex_usage);
	}
	if (argc == 0 && !nas && port == -1) {
		grad_log(GRAD_LOG_ERR,
			 _("at least one port, nas or user must be specified"));
		exit(po.po_ex_usage);
	}
	if (utmpfile)
		grad_utmp_file = utmpfile;

	/*
	 *      Read the "naslist" file.
	 */
	path = grad_mkfilename(grad_config_dir, RADIUS_NASLIST);
	if (grad_nas_read_file(path) < 0)
		exit(1);
	free(path);

	if (nas) {
		grad_nas_t *np;
		np = grad_nas_lookup_name(nas);
		if (np) {
			netdefp = &np->netdef;
		} else {
			if (grad_ip_getnetaddr(nas, &netdef)) {
				fprintf(stderr,
					_("%s: host not found.\n"), nas);
				return 1;
			}
			netdefp = &netdef;
		}
	}

	t = time(NULL);
	radzap(netdefp, port, argv[0], t);
	return 0;
}

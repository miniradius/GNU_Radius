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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>
#include <common.h>
#include "parseopt/parseopt.h"

static struct optdef rad_common_option[] = {
	{
		.opt_doc = N_("Common options"),
		.opt_flags = OPTFLAG_DOC,
	},

	{
		.opt_name = "directory",
		.opt_doc = N_("Set path to the configuration directory"),
		.opt_argdoc = N_("DIR"),
		.opt_set = optset_string_copy,
		.opt_ptr = &grad_config_dir
	},
	{
		.opt_name = "d",
		.opt_flags  = OPTFLAG_ALIAS
	},
	{ NULL }
};

static int copyright_year = 2025;
static char gplv3[] = N_("\
\n\
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\nThis is free software: you are free to change and redistribute it.\n\
There is NO WARRANTY, to the extent permitted by law.\n\
\n\
");

void
radius_version_hook(WORDWRAP_FILE wf, struct parseopt *po)
{
	wordwrap_printf(wf, "%s (%s) %s\n",
			po->po_program_name, PACKAGE_NAME, PACKAGE_VERSION);
	wordwrap_printf(wf, "Copyright %s 1999-%d Sergey Poznyakoff\n",
			_("(C)"),
			copyright_year);
	wordwrap_puts(wf, gplv3);
}

void
grad_parseopt(struct parseopt const *po_orig, int argc, char **argv,
	      int *rargc, char ***rargv)
{
	struct parseopt po;
	int n;

	po = *po_orig;

	for (n = 0; po_orig->po_optdef[n]; n++);
	po.po_optdef = grad_ecalloc(n + 2, sizeof(*po.po_optdef));
	memcpy(po.po_optdef, po_orig->po_optdef,
	       n * sizeof(po_orig->po_optdef[0]));
	po.po_optdef[n] = rad_common_option;
	po.po_package_name = PACKAGE_NAME;
	po.po_package_url = PACKAGE_URL;
	po.po_bugreport_address = PACKAGE_BUGREPORT;
	if (!po.po_version_hook)
		po.po_version_hook = radius_version_hook;

	if (parseopt_getopt(&po, argc, argv) == OPT_ERR) {
		grad_log(GRAD_LOG_CRIT, "parseopt_getopt failed");
		exit(1);
	}
	parseopt_argv(&po, &argc, &argv);
	if (!rargc) {
		if (argc > 0) {
			grad_log(GRAD_LOG_CRIT, _("too many arguments"));
			exit(po.po_ex_usage);
		}
	} else {
		*rargc = argc;
		*rargv = argv;
	}
	grad_path_init();
}

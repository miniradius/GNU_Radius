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

#if defined(HAVE_CONFIG_H)
# include <config.h>
#endif

#define _XPG4_2
#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <ctype.h>
#include <setjmp.h>
#include <errno.h>
#include <sys/uio.h>
#include <signal.h>

#include <radiusd.h>
#include <radius/radutmp.h>
#include <checkrad.h>
#include <rewrite.h>

#include <snmp/asn1.h>
#include <snmp/snmp.h>

struct check_instance {
	/* Data from the radutmp entry */
	char          *name;       /* User name */
	uint32_t nas_ip;      /* NAS IP address */
	int           port;        /* NAS port number */
	char          *sid;        /* Session ID */
	uint32_t framed_ip;   /* Framed IP address */

	/* Data from the nastypes entry */
	int           method;      /* Method to be used */
	char          *func;       /* Name of the rewrite function */
	grad_envar_t  *args;       /* Method arguments */

	/* Additional data */
	char          *nasname;    /* Name of the NAS (for diagnostics) */
	grad_strbuf_t strbuf;      /* Memory allocation stack */

	/* Results */
	int           timeout;     /* Was the timeout hit? */
	int           result;      /* Result of the test */
};

char *
slookup(struct check_instance *checkp, char *name, char *defval)
{
	return grad_envar_lookup_str(checkp->args, name, defval);
}

int
ilookup(struct check_instance *checkp, char *name, int defval)
{
	return grad_envar_lookup_int(checkp->args, name, defval);
}

struct check_instance *
create_instance(struct check_instance *cptr, grad_nas_t *nas, struct radutmp *up)
{
	RADCK_TYPE *radck_type;

	if ((radck_type = find_radck_type(nas->nastype)) == NULL) {
		grad_log(GRAD_LOG_ERR,
			 _("unknown NAS type: %s (nas %s)"),
			 nas->nastype,
			 nas->shortname);
		return NULL;
	}
	cptr->name = up->orig_login[0] ? up->orig_login : up->login;
	cptr->port = up->nas_port;
	cptr->sid  = up->session_id;
	cptr->framed_ip = ntohl(up->framed_address);
	cptr->nas_ip = ntohl(up->nas_address);
	cptr->result = -1;
	cptr->timeout = 0;
	cptr->nasname = nas->shortname[0] ? nas->shortname : nas->longname;

	cptr->method = radck_type->method;
	cptr->args = grad_envar_merge_lists((grad_envar_t*) nas->args,
					    radck_type->args);
	cptr->func = slookup(cptr, "function", NULL);
	cptr->strbuf = grad_strbuf_create();
	return cptr;
}

void
free_instance(struct check_instance *cptr)
{
	grad_strbuf_free(cptr->strbuf);
	grad_envar_free_list(&cptr->args);
}

int
compare(struct check_instance *checkp, char *str)
{
	grad_value_t val;

	if (rewrite_invoke(Integer, &val,
			   checkp->func, NULL,
			   "ssis",
			   str,
			   checkp->name,
			   checkp->port,
			   checkp->sid))
		return -1;
	return val.datum.ival;
}


char *
checkrad_xlat_new(struct check_instance *checkp, char *template)
{
	grad_request_t *req;
	char *str;

	/* Create a temporary request */
	req = grad_request_alloc();
	grad_avl_add_pair(&req->avlist,
			  grad_avp_create_string(DA_USER_NAME, checkp->name));
	grad_avl_add_pair(&req->avlist,
			  grad_avp_create_integer(DA_NAS_PORT_ID,
						  checkp->port));
	grad_avl_add_pair(&req->avlist,
			  grad_avp_create_string(DA_ACCT_SESSION_ID,
						 checkp->sid));
	grad_avl_add_pair(&req->avlist,
			  grad_avp_create_integer(DA_FRAMED_IP_ADDRESS,
						  checkp->framed_ip));
	grad_avl_add_pair(&req->avlist,
			  grad_avp_create_integer(DA_NAS_IP_ADDRESS,
						  checkp->nas_ip));

	str = util_xlate(checkp->strbuf, template, req);

	grad_request_free(req);

	return str;
}

/* Translate traditional OID specs.
 * Replace: %u  -- username
 *          %s  -- session id
 *          %p  -- port no
 *          %P  -- port no + 1
 */
char *
checkrad_xlat_old(struct check_instance *checkp, char *str)
{
	char *ptr;
	int len;
	char buf[24];

	while (*str) {
		if (*str == '%') {
			switch (str[1]) {
			case 'u':
				ptr = checkp->name;
				break;
			case 's':
				ptr = checkp->sid;
				break;
			case 'd':
				snprintf(buf, sizeof(buf), "%lu",
					   strtol(checkp->sid, NULL, 16));
				ptr = buf;
				break;
			case 'p':
				snprintf(buf, sizeof(buf), "%d",
					checkp->port);
				ptr = buf;
				break;
			case 'P':
				snprintf(buf, sizeof(buf), "%d",
					 checkp->port + 1);
				ptr = buf;
				break;
			case 'i':
				grad_ip_iptostr(checkp->framed_ip, buf);
				ptr = buf;
				break;
			default:
				ptr = NULL;
				grad_strbuf_grow(checkp->strbuf, str, 2);
			}
			if (ptr) {
				len = strlen(ptr);
				grad_strbuf_grow(checkp->strbuf, ptr, len);
			}
			str += 2;
		} else {
			grad_strbuf_grow_char(checkp->strbuf, *str);
			str++;
		}
	}
	return grad_strbuf_finish(checkp->strbuf, 0);
}

char *
checkrad_xlat(struct check_instance *checkp, char *str)
{
	if (str[0] == '=')
		return checkrad_xlat_new(checkp, str+1);

	return checkrad_xlat_old(checkp, str);
}

/*ARGSUSED*/
static int
converse(int type, struct snmp_session *sp, struct snmp_pdu *pdu, void *closure)
{
	int rc = 0;
	struct snmp_var *vlist;
	struct check_instance *checkp = (struct check_instance *)closure;
	char buf[64];

	if (type == SNMP_CONV_TIMEOUT) {
		grad_log(GRAD_LOG_NOTICE,
			 _("timed out in waiting SNMP response from NAS %s"),
			 checkp->nasname);
		checkp->timeout++;
		return 1;
	}
	if (type != SNMP_CONV_RECV_MSG)
		return 1;

	for (vlist = pdu->var; rc == 0 && vlist; vlist = vlist->next)
		switch (vlist->type) {
		case SMI_STRING:
			rc = compare(checkp, (char*)vlist->var_str);
			GRAD_DEBUG(2, "(STRING) %s: %d", vlist->var_str, rc);
			break;
		case SMI_INTEGER:
		case SMI_COUNTER32:
		case SMI_COUNTER64:
			snprintf(buf, sizeof(buf), "%d", vlist->var_int);
			rc = compare(checkp, buf);
			GRAD_DEBUG(2, "(INT) %d: %d", vlist->var_int, rc);
			break;
		case SMI_IPADDRESS:
			grad_ip_iptostr(*(uint32_t*) vlist->var_str, buf);
			rc = compare(checkp, buf);
			GRAD_DEBUG(2, "(IPADDR) %#x: %d",
				    *(uint32_t*)vlist->var_str, rc);
			break;
		}

	checkp->result = rc;

	return 1;
}

int
snmp_check(struct check_instance *checkp, grad_nas_t *nas)
{
	int rc = -1;
	struct snmp_pdu *pdu;
	struct snmp_session *sp;
	struct snmp_var *var;
	char *community;
	int retries;
	int timeout;
	char *peername;
	int remote_port;
	oid_t oid;
	char *snmp_oid;

	if ((snmp_oid = slookup(checkp, "oid", NULL)) == NULL) {
		grad_log(GRAD_LOG_ERR, _("no snmp_oid"));
		return -1;
	}
	snmp_oid = checkrad_xlat(checkp, snmp_oid);
	if (!snmp_oid) /* The diagnostics has already been issued */
		return -1;
	oid = oid_create_from_string(snmp_oid);
	if (!oid) {
		grad_log(GRAD_LOG_ERR,
			 _("invalid OID: %s"), snmp_oid);
		return -1;
	}

	if ((community = slookup(checkp, "password", NULL)) == NULL &&
	    (community = slookup(checkp, "community", NULL)) == NULL)
		community = "public";

	retries = ilookup(checkp, "retries", 3);
	timeout = ilookup(checkp, "timeout", 2);
	peername = slookup(checkp, "host", nas->longname);
	remote_port = ilookup(checkp, "port", 161);

	sp = snmp_session_create(community, peername, remote_port,
				 converse, checkp);
	if (!sp) {
		grad_log(GRAD_LOG_ERR,
			 _("can't create snmp session: %s"),
			 snmp_strerror(snmp_errno));
		snmp_free(oid);
		return -1;
	}

	if (snmp_session_open(sp, myip, 0, timeout, retries)) {
		grad_log(GRAD_LOG_ERR,
			 _("can't open snmp session: %s"),
			 snmp_strerror(snmp_errno));
		snmp_free(oid);
		return -1;
	}

	if ((pdu = snmp_pdu_create(SNMP_PDU_GET)) == NULL) {
		grad_log(GRAD_LOG_ERR,
			 _("can't create SNMP PDU: %s"),
			 snmp_strerror(snmp_errno));
		snmp_free(oid);
		snmp_session_close(sp);
		return -1;
	}

	if ((var = snmp_var_create(oid)) == NULL) {
		grad_log(GRAD_LOG_ERR,
			 _("can't create SNMP PDU: %s"),
			 snmp_strerror(snmp_errno));
		snmp_free(oid);
		snmp_session_close(sp);
		snmp_pdu_free(pdu);
		return -1;
	}

	snmp_pdu_add_var(pdu, var);
	snmp_free(oid);

	GRAD_DEBUG(1, "snmpget: %s:%d %s %s",
		    peername,
		    remote_port,
		    community,
		    snmp_oid);

	checkp->result = rc;

	snmp_query(sp, pdu);

	rc = checkp->result;
	snmp_session_close(sp);

	GRAD_DEBUG(1, "result: %d", rc);
	return rc;
}


static jmp_buf to_env;

static void
alrm_handler(int sig)
{
	longjmp(to_env, 1);
}

int
finger_check(struct check_instance *checkp, grad_nas_t *nas)
{
	char *name;
	int namelen;
	FILE *fp;
	int c, lastc;
	struct addrinfo *res, hints;
	struct sockaddr_in sin;
	int port, rc;
	int s;
	struct iovec iov[3];
	struct msghdr msg;
	int found = 0;
	char *peername;
	char *ptr;
	void (*handler)(int) = SIG_IGN;
	unsigned int to;

	name = checkrad_xlat(checkp, slookup(checkp, "arg", "%u"));
	if (!name) /* The diagnostics has already been issued */
		return -1;

	/* Use at most RUT_NAMESIZE bytes from the user name */
	namelen = strlen(name);
	if (namelen > RUT_NAMESIZE) {
		namelen = RUT_NAMESIZE;
		name[RUT_NAMESIZE] = 0;
	}

	peername = slookup(checkp, "host", nas->longname);
	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_INET;
	rc = getaddrinfo(peername, "finger", &hints, &res);
	if (rc) {
		grad_log(GRAD_LOG_ERR, "%s: %s", peername, gai_strerror(rc));
		return -1;
	}
	if (res->ai_addrlen != sizeof(sin)) {
		grad_log(GRAD_LOG_ERR, _("unknown host: %s"), peername);
		freeaddrinfo(res);
		return -1;
	}
	memcpy(&sin, res->ai_addr, sizeof(sin));
	freeaddrinfo(res);

	if ((port = ilookup(checkp, "port", 0)) == 0)
		port = ntohs(sin.sin_port);
	else
		sin.sin_port = htons(port);

	if ((s = socket(sin.sin_family, SOCK_STREAM, 0)) < 0) {
		grad_log(GRAD_LOG_ERR|GRAD_LOG_PERROR, "socket");
		return -1;
	}

	GRAD_DEBUG(1, "finger %s@%s:%d", name, peername, ntohs(port));

	/* have network connection; identify the host connected with */
	memset(&msg, 0, sizeof(msg));
	msg.msg_name = (void *)&sin;
	msg.msg_namelen = sizeof sin;
	msg.msg_iov = iov;
	msg.msg_iovlen = 0;

	/* send the name followed by <CR><LF> */
	iov[msg.msg_iovlen].iov_base = name;
	iov[msg.msg_iovlen++].iov_len = namelen;
	iov[msg.msg_iovlen].iov_base = "\r\n";
	iov[msg.msg_iovlen++].iov_len = 2;

	/* "tcp=0" can be used to disable T/TCP compatibility to finger
	 * broken hosts
	 */
	if (ilookup(checkp, "tcp", 1) &&
	    connect(s, (struct sockaddr *)&sin, sizeof(sin))) {
		grad_log(GRAD_LOG_ERR|GRAD_LOG_PERROR, "connect");
		return -1;
	}

	if (sendmsg(s, &msg, 0) < 0) {
		grad_log(GRAD_LOG_ERR|GRAD_LOG_PERROR, "sendmsg");
		close(s);
		return -1;
	}

	/*
	 * Read from the remote system; If no data arrives, we will exit
	 * by alarm.
	 *
	 * All high bits get stripped, newlines are skipped.
	 */
	lastc = 0;
	if ((fp = fdopen(s, "r")) != NULL) {
		if (setjmp(to_env)) {
			grad_log(GRAD_LOG_NOTICE,
			       _("timed out in waiting for finger response from NAS %s"),
			       checkp->nasname);
			fclose(fp);
			alarm(0);
			grad_set_signal(SIGALRM, handler);
			return checkp->result = -1;
		}

		to = ilookup(checkp, "timeout", 10);
		handler = grad_set_signal(SIGALRM, alrm_handler);
		alarm(to);

		while ((c = getc(fp)) != EOF) {
			if (c == 0x0d) {
				if (lastc == '\r')      /* ^M^M - skip dupes */
					continue;
				c = '\n';
				lastc = '\r';
			} else {
				if (!isprint(c) && !isspace(c)) {
					c &= 0x7f;
					c |= 0x40;
				}
				if (lastc != '\r' || c != '\n')
					lastc = c;
				else {
					lastc = '\n';
					continue;
				}
			}
			grad_strbuf_grow_char(checkp->strbuf, c);
			if (c == '\n') {
				/* Make sure no alarm arrives while
				 * processing data
				 */
				to = alarm(0);
				grad_set_signal(SIGALRM, handler);

				ptr = grad_strbuf_finish(checkp->strbuf, 0);
				GRAD_DEBUG(2, "got : %s", ptr);
				found = compare(checkp, ptr);
				if (found)
					break;

				/* restore alarm settings */
				grad_set_signal(SIGALRM, alrm_handler);
				alarm(to);
			}
		}

		if (!found && lastc != '\n') {
			/* Make sure no alarm arrives while
			 * processing data
			 */
			alarm(0);
			grad_set_signal(SIGALRM, handler);

			grad_strbuf_grow_char(checkp->strbuf, '\n');
			ptr = grad_strbuf_finish(checkp->strbuf, 0);
			GRAD_DEBUG(2, "got : %s", ptr);
			found = compare(checkp, ptr);
		}

		if (ferror(fp)) {
			grad_log(GRAD_LOG_ERR|GRAD_LOG_PERROR, "finger");
		}
		fclose(fp);
	}

	/* restore alarm settings */
	alarm(0);

	GRAD_DEBUG(1, "result: %d", found);
	checkp->result = found;
	return found;
}

/*ARGSUSED*/
int
ext_check(struct check_instance *checkp, grad_nas_t *nas)
{
	char *s, *path;
	int rc;

	s = slookup(checkp, "path", NULL);
	if (!s) {
		grad_log(GRAD_LOG_ERR, _("path variable not set"));
		return -1;
	}
	path = checkrad_xlat_new(checkp, s);
	if (!path) /* The diagnostics has already been issued */
		return -1;

	rc = radius_exec_command(path);
	switch (rc) {
	case 0:
	case 1:
		return rc;
	default:
		return -1;
	}
	return -1;
}

int
guile_check(struct check_instance *checkp, grad_nas_t *nas)
{
	char *s, *expr;
	int rc;

	s = slookup(checkp, "expr", NULL);
	if (!s) {
		grad_log(GRAD_LOG_ERR, _("expr variable not set"));
		return -1;
	}
	expr = checkrad_xlat_new(checkp, s);
	if (!expr) /* The diagnostics has already been issued */
		return -1;

	rc = scheme_eval_boolean_expr(expr);
	switch (rc) {
	case 0:
	case 1:
		return rc;
	default:
		return -1;
	}
	return -1;
}

int
checkrad(grad_nas_t *nas, struct radutmp *up)
{
	struct check_instance checkp;
	int rc = -1;

	if (!create_instance(&checkp, nas, up))
		return -1;

	switch (checkp.method) {
	case METHOD_FINGER:
		rc = finger_check(&checkp, nas);
		break;
	case METHOD_SNMP:
		rc = snmp_check(&checkp, nas);
		break;
	case METHOD_EXT:
		rc = ext_check(&checkp, nas);
		break;
	case METHOD_GUILE:
		rc = guile_check(&checkp, nas);
		break;
	default:
		grad_insist_fail("bad method");
	}
	free_instance(&checkp);
	return rc;
}

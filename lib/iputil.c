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
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <netdb.h>
#include <pwd.h>
#include <time.h>
#include <ctype.h>
#include <signal.h>

#include <radlib.h>

int grad_resolve_hostnames = 1;

/*
 * Check for valid IP address in standard dot notation.
 */
static int
good_ipaddr(const char *addr)
{
	int     dot_count;
	int     digit_count;

	dot_count = 0;
	digit_count = 0;
	while (*addr != 0 && *addr != ' ') {
		if (*addr == '.') {
			if (++dot_count > 3)
				break;
			digit_count = 0;
		} else if (!(isdigit(*addr) && ++digit_count <= 3)) {
			return -1;
		}
		addr++;
	}

	return (dot_count != 3);
}

/*
 *      Return a printable host name (or IP address in dot notation)
 *      for the supplied IP address.
 */
char *
grad_ip_gethostname(uint32_t ipaddr, char *namebuf, size_t size)
{
	struct sockaddr_in s_in;
	int flags = grad_resolve_hostnames ? 0 : NI_NUMERICHOST;

	s_in.sin_family = AF_INET;
	s_in.sin_port = 0;
	s_in.sin_addr.s_addr = htonl(ipaddr);

	if (getnameinfo ((struct sockaddr *)&s_in,
			 sizeof(s_in),
			 namebuf, size,
			 NULL, 0, flags))
		return NULL;
	return namebuf;
}

uint32_t
ip_gethostaddr(const char *host, int isip)
{
	struct addrinfo *res, *ap, hints;
	struct sockaddr_in *sin;
	uint32_t ret = 0;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_INET;
	hints.ai_flags = AI_PASSIVE;
	if (isip)
		hints.ai_flags |= AI_NUMERICHOST;

	if (getaddrinfo(host, NULL, &hints, &res))
		return 0;

	for (ap = res; ap; ap = ap->ai_next) {
		if (ap->ai_addrlen == sizeof(struct sockaddr_in)) {
			sin = (struct sockaddr_in*)ap->ai_addr;
			ret = ntohl(sin->sin_addr.s_addr);
			break;
		}
	}
	freeaddrinfo(res);
	return ret;
}

/*
 * Return an IP address in host long notation from a host
 * name or address in dot notation.
 */
uint32_t
grad_ip_gethostaddr(const char *host)
{
	return ip_gethostaddr(host, 0);
}

/*
 * Return an IP address in standard dot notation for the
 * provided address in host long notation.
 */
char *
grad_ip_iptostr(uint32_t ipaddr, char *bufp)
{
	static char buffer[GRAD_IPV4_STRING_LENGTH];
	if (!bufp)
		bufp = buffer;
	sprintf(bufp, "%u.%u.%u.%u",
		(u_int) ((ipaddr >> 24) & 0xff),
		(u_int) ((ipaddr >> 16) & 0xff),
		(u_int) ((ipaddr >> 8) & 0xff),
		(u_int) (ipaddr & 0xff));
	return bufp;
}

/*
 *      Return an IP address in host long notation from
 *      one supplied in standard dot notation.
 */
uint32_t
grad_ip_strtoip(const char *ip_str)
{
	return ip_gethostaddr(ip_str, 1);
}

int
grad_ip_getnetaddr(const char *str, grad_netdef_t *netdef)
{
	char *p = strchr(str, '/');
	if (!p) {
		uint32_t ip = grad_ip_gethostaddr(str);
		if (ip == 0)
			return 1;
		netdef->netmask = 0xfffffffful;
		netdef->ipaddr = ip;
	} else {
		char buf[GRAD_IPV4_STRING_LENGTH];
		size_t len = p - str;

		if (len >= GRAD_IPV4_STRING_LENGTH)
			return 1;
		memcpy(buf, str, len);
		buf[len] = 0;
		netdef->ipaddr = grad_ip_strtoip(buf);

		if (good_ipaddr(p+1) == 0)
			netdef->netmask = grad_ip_strtoip(p+1);
		else {
			char *endp;
			uint32_t n = strtoul(p+1, &endp, 0);
			if (*endp || n > 32)
				return 1;
			n = 32 - n;
			if (n == 32)
				netdef->netmask = 0;
			else
				netdef->netmask = (0xfffffffful >> n) << n;
		}
		netdef->ipaddr &= netdef->netmask;
	}
	return 0;
}

int
grad_ip_in_net_p(const grad_netdef_t *netdef, uint32_t ipaddr)
{
	return netdef->ipaddr == (ipaddr & netdef->netmask);
}

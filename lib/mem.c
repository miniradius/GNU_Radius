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
#include <radlib.h>

void
grad_memerr(void)
{
	grad_log(GRAD_LOG_CRIT, _("out of memory: aborting"));
	abort();
}

void *
grad_emalloc(size_t size)
{
	char *p = malloc(size);
	if (!p)
		grad_memerr();
	memset(p, 0, size);
	return p;
}

void *
grad_ecalloc(size_t nmemb, size_t size)
{
	char *p = calloc(nmemb, size);
	if (!p)
		grad_memerr();
	return p;
}

void *
grad_erealloc(void *ptr, size_t size)
{
	ptr = realloc(ptr, size);
	if (!ptr)
		grad_memerr();
	return ptr;
}

char *
grad_estrdup(const char *s)
{
	char *p;
	if (!s)
		return NULL;
	if ((p = strdup(s)) == NULL)
		grad_memerr();
	return p;
}

/*
 * Create a copy of binary string S of size SIZE.  Terminate allocated
 * value with \0.  This function is suitable for duplicating both string
 * and binary RADIUS attributes.
 */
char *
grad_ebstrndup(const char *s, size_t size)
{
	char *p;
	if (!s)
		return NULL;
	p = grad_emalloc(size + 1);
	memcpy(p, s, size);
	p[size] = 0;
	return p;
}

char *
grad_string_replace(char **str, const char *new_value)
{
	char *p = *str;
	*str = grad_estrdup(new_value);
	free(p);
	return *str;
}

void
grad_destroy(void **pptr)
{
	if (*pptr) {
		free(*pptr);
		*pptr = NULL;
	}
}

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

#ifndef _gnu_radius_mem_h
#define _gnu_radius_mem_h

void grad_memerr(void);

void *grad_emalloc(size_t);
void *grad_ecalloc(size_t nmemb, size_t size);
void *grad_erealloc(void *, size_t);
char *grad_estrdup(const char *);
char *grad_ebstrndup(const char *s, size_t size);
char *grad_string_replace(char **str, const char *value);
void grad_destroy(void **);

#endif

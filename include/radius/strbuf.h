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

#ifndef _gnu_radius_strbuf_h
#define _gnu_radius_strbuf_h

typedef struct grad_strbuf *grad_strbuf_t;

grad_strbuf_t grad_strbuf_create(void);
void grad_strbuf_free(grad_strbuf_t acc);
void grad_strbuf_grow(grad_strbuf_t acc, const char *buf, size_t size);
static inline void grad_strbuf_grow_char(grad_strbuf_t acc, int c) {
	char __c = c;
	grad_strbuf_grow(acc, &__c, 1);
}
void grad_strbuf_grow_escape(struct grad_strbuf *sb, const char *buf, size_t len);
static inline void
grad_strbuf_grow_string(struct grad_strbuf *sb, const char *buf)
{
	grad_strbuf_grow(sb, buf, strlen(buf));
}
static inline void
grad_strbuf_grow_string_escape(struct grad_strbuf *sb, const char *buf)
{
	grad_strbuf_grow_escape(sb, buf, strlen(buf));
}
char *grad_strbuf_finish(grad_strbuf_t acc, int steal);
void grad_strbuf_free_string(grad_strbuf_t acc, char *str);
void grad_strbuf_clear(grad_strbuf_t sb);
void grad_strbuf_reset(struct grad_strbuf *sb);

#endif

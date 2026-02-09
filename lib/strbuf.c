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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <radius/mem.h>
#include <radius/list.h>
#include <radius/strbuf.h>

struct strsegm {
	char *buf;              /* Text buffer */
	size_t size;            /* Buffer size */
	size_t len;             /* Actual number of bytes in buffer */
	DLIST_ENTRY(strsegm) link;
};
#define STRBUF_BUFSIZE 1024
static inline int strsegm_freesize(struct strsegm *e) {
	return e->size - e->len;
}

typedef DLIST_HEAD(,strsegm) STRSEGM_HEAD;

struct grad_strbuf
{
	STRSEGM_HEAD cur;  /* Current build list */
	STRSEGM_HEAD mem;  /* List of already allocated elements */
};

static struct strsegm *
strsegm_alloc(STRSEGM_HEAD *head, size_t size)
{
	struct strsegm *p = grad_emalloc(sizeof(*p));
	p->buf = grad_emalloc(size);
	p->size = size;
	p->len = 0;
	DLIST_INSERT_TAIL(head, p, link);
	return p;
}

static struct strsegm *
strbuf_current(struct grad_strbuf *sb)
{
	struct strsegm *segm;

	if (DLIST_EMPTY(&sb->cur))
		return strsegm_alloc(&sb->cur, STRBUF_BUFSIZE);
	segm = DLIST_LAST(&sb->cur);
	if (strsegm_freesize(segm) == 0)
		segm = strsegm_alloc(&sb->cur, STRBUF_BUFSIZE);
	return segm;
}

static void
strsegm_append(struct strsegm *segm, const char *p, size_t size)
{
	memcpy(segm->buf + segm->len, p, size);
	segm->len += size;
}

static void
strsegm_tailor(struct strsegm *segm)
{
	if (segm->size > segm->len) {
		char *p = grad_erealloc(segm->buf, segm->len);
		segm->buf = p;
		segm->size = segm->len;
	}
}

static void
strsegm_free(struct strsegm *segm)
{
	free(segm->buf);
	free(segm);
}

static void
strsegm_head_free(STRSEGM_HEAD *head)
{
	while (!DLIST_EMPTY(head)) {
		struct strsegm *segm = DLIST_FIRST(head);
		DLIST_REMOVE_HEAD(head, link);
		strsegm_free(segm);
	}
}

struct grad_strbuf *
grad_strbuf_create(void)
{
	struct grad_strbuf *sb = grad_emalloc(sizeof(*sb));
	DLIST_INIT(&sb->cur);
	DLIST_INIT(&sb->mem);
	return sb;
}

void
grad_strbuf_free(struct grad_strbuf *sb)
{
	if (sb) {
		grad_strbuf_clear(sb);
		free(sb);
	}
}

void
grad_strbuf_grow(struct grad_strbuf *sb, const char *buf, size_t size)
{
	while (size) {
		struct strsegm *segm = strbuf_current(sb);
		size_t rest = strsegm_freesize(segm);
		if (rest > size)
			rest = size;
		strsegm_append(segm, buf, rest);
		buf += rest;
		size -= rest;
	}
}

void
grad_strbuf_grow_escape(struct grad_strbuf *sb, const char *buf, size_t len)
{
	for (; len; len--, buf++) {
		switch (*buf) {
		case '"':
		case '\'':
		case '\\':
			grad_strbuf_grow_char(sb, '\\');
			grad_strbuf_grow_char(sb, *buf);
			break;

		default:
			if (isprint(*buf))
				grad_strbuf_grow_char(sb, *buf);
			else {
				char oct[4];
				snprintf(oct, sizeof(oct), "%03o",
					 *(u_char*)buf);
				grad_strbuf_grow_char(sb, '\\');
				grad_strbuf_grow(sb, buf, 3);
			}
		}
	}
}

char *
grad_strbuf_finish(struct grad_strbuf *sb, int steal)
{
	struct strsegm *segm;
	size_t size;
	char *p;

	grad_strbuf_grow_char(sb, 0);

	segm = DLIST_FIRST(&sb->cur);
	if (DLIST_NEXT(segm, link) == NULL) {
		DLIST_REMOVE_HEAD(&sb->cur, link);
		strsegm_tailor(segm);
		DLIST_INSERT_TAIL(&sb->mem, segm, link);
	} else {
		struct strsegm *tp;

		size = 0;
		DLIST_FOREACH(tp, &sb->cur, link)
			size += tp->len;

		segm = strsegm_alloc(&sb->mem, size);
		while (!DLIST_EMPTY(&sb->mem)) {
			tp = DLIST_FIRST(&sb->mem);
			strsegm_append(segm, tp->buf, tp->len);

			DLIST_REMOVE_HEAD(&sb->mem, link);
			strsegm_free(tp);
		}
	}

	p = segm->buf;
	if (steal) {
		DLIST_REMOVE(&sb->mem, segm, link);
		free(segm);
	}
	return p;
}

void
grad_strbuf_free_string(struct grad_strbuf *sb, char *str)
{
	struct strsegm *segm, *tmp;
	DLIST_FOREACH_SAFE(segm, tmp, &sb->mem, link) {
		if (segm->buf == str) {
			DLIST_REMOVE(&sb->mem, segm, link);
			strsegm_free(segm);
			return;
		}
	}
}

void
grad_strbuf_reset(struct grad_strbuf *sb)
{
	strsegm_head_free(&sb->cur);
//	DLIST_INIT(&sb->cur);
	DLIST_INIT(&sb->mem);
}

void
grad_strbuf_clear(struct grad_strbuf *sb)
{
	strsegm_head_free(&sb->cur);
	strsegm_head_free(&sb->mem);
}

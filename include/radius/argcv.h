/* GNU Mailutils -- a suite of utilities for electronic mail
   Copyright (C) 1999-2025 Free Software Foundation, Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GNU Radius.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef _ARGCV_H
#define _ARGCV_H 1

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int grad_argcv_get    (const char *command, const char *delim,
			      const char* cmnt,
			      int *argc, char ***argv);
extern int grad_argcv_string (int argc, char **argv, char **string);
extern int grad_argcv_free   (int argc, char **argv);
extern int grad_argcv_unquote_char (int c);
extern int grad_argcv_quote_char (int c);
extern size_t grad_argcv_quoted_length (const char *str, int *quote);
extern size_t grad_argcv_quoted_length_n (const char *str, size_t size,
					  int *quote);
extern void grad_argcv_unquote_copy (char *dst, const char *src, size_t n);
extern void grad_argcv_quote_copy_n (char *dst, const char *src, size_t size);
extern void grad_argcv_quote_copy (char *dst, const char *src);

#ifdef __cplusplus
}
#endif

#endif /* _ARGCV_H */

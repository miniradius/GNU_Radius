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

#ifndef SYSDEP_H_INCLUDED
#define SYSDEP_H_INCLUDED

#include <strings.h>
#include <string.h>

#include <stdarg.h>
#include <sys/types.h>
#include <stdint.h>
#include <sys/time.h>
#include <time.h>

#ifndef timercmp
#define       timercmp(tvp, uvp, cmp)\
		      ((tvp)->tv_sec cmp (uvp)->tv_sec ||\
		      (tvp)->tv_sec == (uvp)->tv_sec &&\
		      (tvp)->tv_usec cmp (uvp)->tv_usec)
#endif
#ifndef timersub
# define timersub(a, b, result)                             \
  do {                                                      \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;           \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;        \
    if ((result)->tv_usec < 0) {                            \
      --(result)->tv_sec;                                   \
      (result)->tv_usec += 1000000;                         \
    }                                                       \
  } while (0)
#endif

typedef void (*grad_signal_handler_t) (int);
grad_signal_handler_t grad_set_signal(int sig, grad_signal_handler_t sighandler);

#endif /* SYSDEP_H_INCLUDED */

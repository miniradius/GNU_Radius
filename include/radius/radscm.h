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

#ifndef _gnu_radius_radscm_h
#define _gnu_radius_radscm_h

SCM radscm_avl_to_list(grad_avp_t *pair);
grad_avp_t *radscm_list_to_avl(SCM list);
SCM radscm_avp_to_cons(grad_avp_t *pair);
grad_avp_t *radscm_cons_to_avp(SCM scm);
void radscm_init(void);
void grad_scm_init(void);

void grad_mschap(const unsigned char *win_password,
		 const unsigned char *challenge, unsigned char *response);
void grad_lmpwdhash(const unsigned char *password, unsigned char *lmhash);

void rscm_syslog_init(void);
void rscm_utmp_init(void);
void rscm_avl_init(void);
void rscm_dict_init(void);
void rscm_radlog_init(void);
void rscm_rewrite_init(void);
void rscm_sql_init(void);
void rscm_add_load_path(char *path);
void rscm_server_init(void);
void rscm_hash_init(void);

char *rscm_load_path(char *);

#endif /* !_gnu_radius_radscm_h */

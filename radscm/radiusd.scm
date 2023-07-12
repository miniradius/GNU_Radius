;;;; -*- scheme -*-
;;;; This file is part of GNU Radius.
;;;; Copyright (C) 2004, 2007, 2008 Free Software Foundation, Inc.
;;;;
;;;; Written by Sergey Poznyakoff
;;;;
;;;; GNU Radius is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; GNU Radius is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Radius; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;

;;; Export symbols defined in the server module
(export GRAD_LOG_MAIN GRAD_LOG_AUTH GRAD_LOG_ACCT GRAD_LOG_PROXY
        GRAD_LOG_SNMP GRAD_LOG_EMERG GRAD_LOG_ALERT GRAD_LOG_CRIT
        GRAD_LOG_ERR GRAD_LOG_WARN GRAD_LOG_NOTICE GRAD_LOG_INFO GRAD_LOG_DEBUG)
(export SQL_AUTH SQL_ACCT)

(export  rad-log-open rad-log rad-log-close)
(export  rad-rewrite-execute-string rad-rewrite-execute)
(export  radius-sql-query radius-sql-run-query)

;;; Export needed symbols from gnuradius module
;;; We could have added :use-module (gnuradius) to the
;;; module definition, but that would break the testsuite
;;; (gnuradius won't load unless it is installed).
;;;
;;; FIXME: Possibly it will be worth while to give up .inc files
;;; in the favor of .exp and to re-export all gnuradius symbols right here,
;;; so that loadable modules will not have to explicitely load
;;; gnuradius.scm

(export rad-dict-value->name)

;;; Define radiusd module. Make sure we re-export all useful symbols
;;; from the server.
(define-module (radiusd)
  :use-module (guile-user)
  :re-export (GRAD_LOG_MAIN GRAD_LOG_AUTH GRAD_LOG_ACCT GRAD_LOG_PROXY
	      GRAD_LOG_SNMP GRAD_LOG_EMERG GRAD_LOG_ALERT GRAD_LOG_CRIT
	      GRAD_LOG_ERR GRAD_LOG_WARN GRAD_LOG_NOTICE GRAD_LOG_INFO 
	      GRAD_LOG_DEBUG
	      SQL_AUTH SQL_ACCT
               rad-log-open rad-log rad-log-close
               rad-rewrite-execute-string rad-rewrite-execute
               radius-sql-query radius-sql-run-query))

(define auth-mod-list '())

(define-public (radiusd-register-auth-method auth-type handler)
  (set! auth-mod-list (cons (cons auth-type handler) auth-mod-list)))

(define-public (radiusd-try-auth auth-type req check reply)
  (let ((x (assoc (rad-dict-value->name "Auth-Type" auth-type)
		  auth-mod-list)))
    (if x
	((cdr x) req check reply)
	#f)))

;;;; End of radiusd.scm

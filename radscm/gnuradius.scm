;;;; -*- scheme -*-
;;;; This file is part of GNU Radius.
;;;; Copyright (C) 2004, 2007 Free Software Foundation, Inc.
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


(define-module (gnuradius)
  :use-module (ice-9 format))

(let ((lib-path "/usr/local/lib/"))
  (load-extension (string-append
		   lib-path "libguile-gnuradius-v-1.6") "radscm_init"))

;; Exports go here
(export %raddb-path
	grad-package
	grad-version
	grad-package-string)

(export sha1-calc)

;; Includes go here
(export avl-delete)
(export avl-merge)
(export avl-match?)

(export rad-dict-name->attr)
(export rad-dict-value->name)
(export rad-dict-name->value)
(export rad-dict-pec->vendor)

(export rad-openlog)
(export rad-syslog)
(export rad-closelog)

(export rad-utmp-putent)

(export md5-calc)
(export md4-calc)
(export sha1-calc-list)
(export lm-password-hash)
(export mschap-response)
(export string-hex->bin)
(export string-bin->hex)


(export rad-directory)
(export rad-send-internal)
(export rad-client-list-servers)
(export rad-get-server)
(export rad-client-add-server)
(export rad-client-set-server)
(export rad-client-source-ip)
(export rad-client-timeout)
(export rad-client-retry)
(export rad-read-no-echo)


;; Code

(define (sha1-calc . rest)
  (sha1-calc-list rest))

;; Ports

(define-public :port-auth 0)
(define-public :port-acct 1)
(define-public :port-cntl 2)
;; Request types
(define-public :auth-req 1)
(define-public :auth-ack 2)
(define-public :auth-rej 3)
(define-public :acct-req 4)
(define-public :acct-resp 5)
(define-public :acct-stat 6)
(define-public :access-challenge 11)



;; Format a radius reply code.
;;  dest   --  destination (format-like)
;;  code   --  integer reply code
(define-public (rad-format-code dest code)
"Format a radius reply code into a human-readable form.
DEST-BOOL has the same meaning as in format.

(rad-format-code DEST-BOOL CODE-NUMBER)"
  (format dest "~A\n"
          (cond 
            ((= code :auth-req)
             "Auth-Request")
            ((= code :auth-ack)
             "Auth-Acknowledge")
            ((= code :auth-rej)
             "Auth-Reject")
            ((= code :acct-req)
             "Acct-Request")
            ((= code :acct-resp)
             "Acct-Response")
            ((= code :acct-stat)
             "Acct-Stat")
            ((= code :access-challenge)
             "Access-Challenge")
            (else
             (number->string code)))))

;; Format an attribute-value pair
(define-public (rad-format-pair dest pair)
"Format a radius attribute/value pair for output.
DEST-BOOL has the same meaning as in format.
PAIR is eihter 
                (cons NAME-STRING VALUE)
        or
                (cons ATTR-NUMBER VALUE)
Where VALUE may be of any type appropriate for the given attribute.
(rad-format-pair DEST-BOOL PAIR)"
  (format dest "~A: ~A\n" (car pair) (cdr pair)))

;; Pretty-print A/V pairs from plist
(define-public (rad-print-pairs dest plist)
"Output the radius attribute/value pairs from the PAIR-LIST.
DEST-BOOL has the same meaning as in format.
PAIR-LIST is a list of pairs in the form
                (cons NAME-STRING VALUE)
        or
                (cons ATTR-NUMBER VALUE)
Where VALUE may be of any type appropriate for the given attribute.

All \"Reply-Message\" pairs from the list are concatenated and displayed
as one.

(rad-print-pairs DEST-BOOL PAIR-LIST)"
  (let loop ((plist plist)
             (mesg '()))
    (cond
     ((null? plist)
      (cond
       ((not (null? mesg))
        (format dest "Reply Message:\n")
        (for-each (lambda (m)
                    (format dest "~A" m))
                  mesg)))
      (format dest "\n"))
     (else
      (loop (cdr plist)
            (cond
             ((string=? (car (car plist)) "Reply-Message")
              (append mesg (list (cdr (car plist)))))
             (else
              (rad-format-pair dest (car plist))
              mesg)))))))

(define-public (rad-format-reply-msg plist . text)
"Concatenate and print text from all \"Reply-Message\" pairs from the
PAIR-LIST. If TEXT is specified, it is printed before the concatenated
text.

(rad-format-reply-msg PAIR-LIST . TEXT)"
  (let loop ((plist plist)
             (mesg '()))
    (cond
     ((null? plist)
      (cond
       ((not (null? mesg))
        (if (pair? text)
            (format #t "~A\n" (car text)))
        (for-each (lambda (m)
                    (format #t "~A" m))
                  mesg))))
     (else
      (loop (cdr plist)
            (cond
             ((string=? (car (car plist)) "Reply-Message")
              (append mesg (list (cdr (car plist)))))
             (else
              mesg)))))))

;; Send a radius request.
;;  Arguments:
;;   port       Specifies the radius communication port to use
;;   code       Request code
;;   plist      A/V pair list
;;   verbose    (bool) Verbosely print the server reply
;;  Return
;;   (list reply-code reply-pairs)
;;  or
;;   () when if there were no response
(define-public (rad-send port code plist . verbose)
"Sends the request to currently selected server.
PORT-NUMBER     Port to use.
                        0 - Authentication port
                        1 - Accounting port
                        2 - Control port
                The actual port numbers are those configured for
                the given server.
CODE-NUMBER     Request code.
PAIR-LIST       List of Attribute-value pairs. Each pair is:
                        (cons ATTR-NAME-STRING . VALUE)
                or
                        (cons ATTR-NUMBER . VALUE)
VERBOSE         If it is specified, the verbose report about interaction
                with the radius server is printed.

Return:

On success
        (list RETURN-CODE-NUMBER PAIR-LIST)
On failure:
        '()

(rad-send PORT-NUMBER CODE-NUMBER PAIR-LIST . VERBOSE)" 
  (let ((repl (rad-send-internal port code plist)))
    (cond
     ((and (pair? verbose) (car verbose))
      (cond
       ((null? repl)
        (format #t "NO REPLY\n"))
       (else
        (rad-format-code #t (car repl))
        (rad-print-pairs #t (cdr repl))))))
    repl))

(define-public rad-server-list '())

(define-public (rad-select-server id)
"Select the server identified by ID-STRING as a current server. The server
data are looked up in rad-server-list variable.

(rad-select-server ID-STRING)"
  (let ((server (do ((tail rad-server-list (cdr tail))
                     (match #f))
                    ((or (null? tail) match) match)
                  (if (string=? (caar tail) id)
                      (set! match (car tail))))))
    (cond
     (server
      (rad-client-set-server server))
     (else
      (format #t "no such server: ~A\n" id)
      #f))))

(define-public (rad-add-server id)
"Add the server identified by ID-STRING to the list of current servers.
The server data are looked up in rad-server-list variable.

(rad-add-server ID-STRING)"  
  (let ((server (do ((tail rad-server-list (cdr tail))
                     (match #f))
                    ((or (null? tail) match) match)
                  (if (string=? (caar tail) id)
                      (set! match (car tail))))))
    (cond
     (server
      (rad-client-add-server server))
     (else
      (format #t "no such server: ~A\n" id)
      #f))))
        
(define-public (rad-list-servers)
"For each server from rad-server-list print its ID and hostname
or IP address.

(rad-list-servers)"
  (for-each (lambda (s)
              (format #t "~A ~A\n"
                      (car s) (cadr s)))
            rad-server-list))

;;;; End of gnuradius.scm

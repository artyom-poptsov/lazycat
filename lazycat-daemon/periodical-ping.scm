;;; periodical-ping.scm -- Periodical ping of hosts.

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is part of LazyCat.
;;
;; LazyCat is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; LazyCat is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with LazyCat.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; This module contains `periodical-ping' procedure that walks through
;; the host list with specified interval and checks whether they are
;; online or not.


;;; Code:

(define-module (lazycat lazycat-daemon periodical-ping)
  #:use-module (logging logger)
  #:use-module (scheme documentation)
  ;; LazyCat modules:
  #:use-module (lazycat message)
  #:use-module (lazycat proxy)
  #:use-module (lazycat lazycat-daemon host)
  #:use-module (lazycat lazycat-daemon host-list)
  #:use-module (lazycat lazycat-daemon proxy-list)
  #:export (periodical-ping))

(define-with-docs *periodical-ping-thread-prio*
  "Priority of the periodical ping thread."
  15)

(define (periodical-ping host-list options)
  "Ping hosts from HOST-LIST periodically and update their statuses.
Ping interval is set through `ping-interval' option from the OPTIONS
hash table."
  (setpriority PRIO_PROCESS 0 *periodical-ping-thread-prio*)
  (while #t
    (let ((plain-host-list (host-list-get-plain-list host-list))
          (period    (string->number (hash-ref options 'ping-interval))))

      ;; Zero interval means disabled ping.
      (if (zero? period)
          (begin
            (yield)
            (sleep 5)
            (continue)))

      (for-each
       (lambda (host)
         (let* ((address      (host-get-address host))
                (host-proxies (host-get-proxy-list host))
                (proxy        (proxy-list-get-proxy (car host-proxies))))

           (if proxy

               (catch 'proxy-error
                 (lambda ()
                   (let ((msg-rsp (proxy-ping proxy address)))
                     (if (not (message-error? msg-rsp))
                         (if (message-field-ref msg-rsp 'status)
                             (host-set-status! host 'online)
                             (host-set-status! host 'offline)))))
                 (lambda (key . args)
                   (log-msg 'ERROR (object->string args))
                   (host-set-status! host 'offline)))

               (let ((msg (string-append "No such proxy: "
                                         (object->string (car host-proxies)))))
                 (log-msg 'DEBUG msg)))))

       plain-host-list)

      (sleep period))))

;;; periodical-ping.scm ends here

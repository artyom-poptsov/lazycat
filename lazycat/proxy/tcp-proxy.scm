;;; tcp-proxy.scm -- TCP proxy for LazyCat.

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

;; Proxy for communicating with remote hosts through TCP sockets.


;;; Code:

(define-module (lazycat proxy tcp-proxy)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (lazycat proxy)
  #:re-export  (handle-send-message
                handle-set-option!
                handle-get-option
                handle-list-options
                handle-ping)
  #:export (<tcp-proxy>))


(define-class <tcp-proxy> (<proxy>)
  (options
   #:getter get-options
   #:init-value (make-hash-table)))

(define-method (initialize (obj <tcp-proxy>) args)
  (next-method)

  (proxy-set-name! obj "tcp-proxy")
  (proxy-set-description! obj "TCP proxy"))


;;; Helper functions

;; Parse address.  Address is expected to be in the following format:
;;   host:port
;;
;; Return address' components in the following format:
;;   '(hostname port)
;;
(define (parse-address address)
  (let ((s (string-match "^(.*):([0-9]+)" address)))

    (if (regexp-match? s)

        (let ((hostname (match:substring s 1))
              (port     (match:substring s 2)))
          (list hostname port))

        #f)))


;;; Interface implementation

(define-method (handle-send-message (obj     <tcp-proxy>)
                                    (address <string>)
                                    (message <string>))
  (let ((connection-data (parse-address address)))

    (if (not connection-data)
        (proxy-error "Wrong address" address))

    (let* ((hostname  (car connection-data))
           (port      (string->number (cadr connection-data)))
           (s         (socket PF_INET SOCK_STREAM 0))
           (host      (gethost hostname))
           (inet-addr (car (hostent:addr-list host))))

      (connect s AF_INET inet-addr port)

      (display message s)
      (newline s)

      (let ((response (read-line s)))
        (close s)
        response))))


;; Get options list
;; 
;; Return:
;;   <payload> ::= ( ( <option> . <value> ) ... )
(define-method (handle-list-options (obj <tcp-proxy>))
  (hash-map->list cons (get-options obj)))


;; Get option value
;; 
;; Return:
;;   <payload> ::= ( <value> )
(define-method (handle-get-option (obj <tcp-proxy>) (option <symbol>))
  (hash-ref (get-options obj) option))


;; Set option value
;; 
;; Return:
;;   <payload> ::= ( )
(define-method (handle-set-option! (obj <tcp-proxy>) (option <symbol>) value)
  (hash-set! (get-options obj) option value)
  '())


;; Check if remote side with address ADDRESS is accessible.
;;
;; Return:
;;   <payload> ::= ( #t )
(define-method (handle-ping (obj <tcp-proxy>) (address <string>))
  (handle-send-message obj address "uname")
  '(#t))

;;; tcp-proxy.scm ends here

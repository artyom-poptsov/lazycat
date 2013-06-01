;;; protocol.scm -- Description of the protocol.

;; Copyright (C) 2013 Artyom Poptsov <poptsov.artyom@gmail.com>
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

;; Description of the protocol that is used in communication between
;; the server and a UI (i.e. lc and lazy-gtk-cat) and between the
;; server and proxies (i.e. ssh-proxy).
;;
;; Message format is described in Backus-Naur Form (BNF) notation.
;;
;; General request format:
;;   <request> ::= ( <type> <request-payload> )
;;   <type>            ::= <number>
;;   <request-payload> ::= <object> | <list> | ( )
;;
;; General response format:
;;   <response> ::= ( #t <response-payload> ) | ( #f <error-message> )


;;; Code:

(define-module (lazycat protocol))

;; Version of the protocol
(define-public *protocol-version* 2)


;;; client -> server

;; Get protocol version that is used by the server
;;
;; Request format:
;;   <request-payload> ::= ( )
;;
;; Response format:
;;   <response-payload> ::= ( <protocol-version> )
;;
(define-public *cmd-get-protocol-version* 0)


;; Add a new host
;;
;; Request format:
;;   <request-payload> ::= ( <group> <name> <proxy-list> <address> <description> )
;;   <group>       ::= <string>
;;   <name>        ::= <string>
;;   <proxy-list>  ::= ( <proxy-name> ... )
;;   <proxy-name>  ::= <string>
;;   <address>     ::= <string>
;;   <description> ::= <string>
;;
;; Response format:
;;   <response-payload> ::= ( <host-id> )
;;
(define-public *cmd-add-host*             1)


;; Remove a host
;;
;; Request format:
;;   <request-payload> ::= <host-id>
;;   <host-id> ::= <number>
;;
;; Response format:
;;   <response-payload> ::= ( )
;;
(define-public *cmd-rem-host*             2)


;; Update host information
;;
;; Request format:
;;   <request-payload> ::= ( <parameter> <value> )
;;
;; Response format:
;;   <response-payload> ::= ( )
;;
(define-public *cmd-upd-host*             3)


;; List various kinds of information.
;;
;; Request format:
;;   <request-payload> ::= <type>
;;   <type> ::= <symbol>
;;
;; Response format:
;;   <response-payload> ::= ( <object> ... )
;;
(define-public *cmd-list*                 4)


;; Execute a command on a host.
;;
;; Request format:
;;   <request-payload> ::= <command> | ( <host-id> <command> )
;;   <host-id> ::= <number>
;;   <command> ::= <string>
;;
;; Response format:
;;   <response-payload> ::= ( ( <host-id> ( <status> <output> ) ) ... )
;;   <host-id> ::= <number>
;;   <status>  ::= <boolean>
;;   <output>  ::= <string>
;;
(define-public *cmd-exec*                 5)


;; Get a diff.
;;
;; Request format:
;;   <request-payload> ::= ( <action> ) | ( <action> <argument> )
;;
;; Response format (getting pattern):
;;   <response-payload> ::= ( ( <master-host-id> ( <status> <output> ) ) )
;;
;; Response format (comparing):
;;   <response-payload> ::= ( ( <host-id> ( <result> <output> ) ) ... )
;;   <result> ::= 'similar | 'different | 'error
;;   <output> ::= #f | <diff> | <error-message>
;;
(define-public *cmd-diff*                 6)


;; Get value of an option.
;;
;; Request format:
;;   <request-payload> ::= <option>
;;   <option> ::= <string>
;;
;; Response format:
;;   <response-payload> ::= ( <value> )
;;   <value> ::= <string>
;;
(define-public *cmd-get*                  7)


;; Set a new value to an option.
;;
;; Request format:
;;   <request-payload> ::= ( <option> <value> )
;;   <option> ::= <string>
;;   <value>  ::= <string>
;;
;; Response format:
;;   <response-payload> ::= ( )
;;
(define-public *cmd-set*                  8)


;; Stop the lazycat daemon
;;
;; Request format:
;;   <request-payload> ::= ( )
;;
;; Response format:
;;   <response-format> ::= ( )
;;
(define-public *cmd-stop*                 10)


;;; server -> proxy

;; Send a message to a host.
;;
;; Request format:
;;   <request-payload> ::= ( <address> <message> )
;;   <address> ::= <string>
;;   <message> ::= <string>
;;
;; Response format:
;;   <response-payload> ::= ( <output> )
;;   <output> ::= <string>
;;
(define-public *cmd-proxy-send*           100)


;; Set an option.
;;
;; Request format:
;;   <request-payload> ::= ( <option> <value> )
;;   <option> ::= <string>
;;   <value> ::= <string>
;;
;; Response format
;;   <response-payload> ::= ( )
;;
(define-public *cmd-proxy-set*            101)


;; Get value of an option.
;;
;; Request format:
;;   <request-payload> ::= <option>
;;   <option> ::= <string>
;;
;; Response format:
;;   <response-payload> ::= ( <value> )
;;
(define-public *cmd-proxy-get*            102)


;; List all options.
;;
;; Request format:
;;   <request-payload> ::= ( )
;;
;; Response format:
;;   <response-payload> ::= ( ( <option> . <value> ) ... )
;;   <option> ::= <symbol>
;;
(define-public *cmd-proxy-list-options*   103)


;; Ping a host.
;;
;; Request foramt:
;;   <request-payload> ::= <address>
;;
;; Response format:
;;   <response-payload> ::= ( #t ) | ( #f )
;;
(define-public *cmd-proxy-ping*           104)


;; Stop the proxy process
;;
;; Request format:
;;   <request-payload> ::= ( )
;;
;; Response format:
;;   <response-payload> ::= ( )
;;
(define-public *cmd-proxy-stop*           105)

;;; protocol.scm ends here.

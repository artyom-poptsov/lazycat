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
;; Message format is described in the Augmented Backus-Naur Form
;; (ABNF) notation described in RFC5234.
;;
;; Some rules that are represented Scheme objects (such as
;; <scheme-number> and <scheme-list>) are not described here -- refer
;; to the documentation for Scheme programming language.
;;
;; Generally the message is an association list consist of the
;; mandatory fields represent the <header> and optional <payload>.
;;
;; General message format:
;;
;;   <message>         = "(" <header> <payload> ")"
;;                             ; <header> MAY follow the <payload>, although
;;                             ; it is reasonable to put it at the beginning of
;;                             ; the message.
;;
;;   <header>          = "(type  . " <type>    ")"
;;                       ( "(flags . " <flags> ")" / "(flags)" )
;;
;;   <type>            = <scheme-number>
;;                             ; Possible message types are described in
;;                             ; this file.
;;
;;   <flags>           = "(" *( <scheme-symbol> <WSP> ) ")"
;;                             ; Possible flags are described in the message.scm
;;                             ; file.  NOTE that order flags doesn't matter.
;;
;;   <payload>         = *<key-value-pair>
;;                             ; Payload is a Scheme association list
;;                             ; and consists of Scheme pairs.
;;                             ; NOTE that order of pairs doesn't matter.
;;   <key-value-pair>  = "(" <scheme-symbol> " . " <scheme-object> ")"
;;


;;; Code:

(define-module (lazycat protocol))

;; Version of the protocol
(define-public *protocol-version* 2)


;;; client <-> server

;; Get a protocol version that is used by the server
;;
;; Response format:
;;   <payload>      = "( protocol-version . " <protocol-version> ")"
;;
(define-public *cmd-get-protocol-version* 0)


;; Add a new host
;;
;; Request payload:
;;   <payload>      = "( group       . " <scheme-string> ") "
;;                    "( name        . " <scheme-string> ") "
;;                    "( proxy-list  . " <scheme-list>   ") "
;;                    "( address     . " <scheme-string> ") "
;;                    "( description . " <scheme-string> ") "
;;                          ; NOTE that order of the fields doesn't matter.
;;
;; Response payload:
;;   <payload>      = "(host-id . " <scheme-number> ")"
;;
(define-public *cmd-add-host*             1)


;; Remove a host
;;
;; Request payload:
;;   <payload>      = "(host-id . " <scheme-number> ")"
;;
(define-public *cmd-rem-host*             2)


;; Update host information
;;
;; Request payload:
;;   <payload>      = "(" <parameter> " . " <value> ")"
;;   <parameter>    = <scheme-symbol>
;;   <value>        = <scheme-object>
;;
(define-public *cmd-upd-host*             3)


;; List various kinds of information.
;;
;; Request payload:
;;   <payload>      = "(object-type . " <object-type> ")"
;;   <object-type>  = <scheme-symbol>
;;
;; Response payload:
;;   <payload>      = "(object-list  . " <scheme-list> ")"
;;
(define-public *cmd-list*                 4)


;; Execute a command on a host.
;;
;; Request payload:
;;   <payload>      = "(command . " <scheme-list> ")"
;;                    [ "(host-id . " <scheme-number> ")" ]
;;                          ; If host-id is omitted it means that
;;                          ; the command should be executed on all
;;                          ; accessible hosts.
;;                    [ "(translate? . " <scheme-boolean> ")" ]
;;
;; Response payload:
;;   <payload>      = "( output . " <output-list> ")"
;;   <output-list>  = "(" 1*( "(" <host-id> <WSP>
;;                    "(" <status> <WSP> <output> "))" ) ")"
;;   <host-id>      = <scheme-number>
;;   <status>       = <scheme-boolean>
;;   <output>       = <scheme-string>
;;
(define-public *cmd-exec*                 5)


;; Get a diff.
;;
;; Request payload:
;;   <payload>      =  "(action . get-pattern)" [<WSP>]
;;                     "(command . " <command> ")"
;;                          ; NOTE that order of pairs doesn't matter.
;;   <payload>      =/ "(action . continue)"
;;   <payload>      =/ "(action . abort)"
;;
;;   <command>      =  <scheme-string>
;;
;; Response payload (getting pattern):
;;   <payload>      = "(output . (" <master-host-id> <WSP>
;;                    "(" <status> <WSP> <output> ")))"
;;   <output>       = <scheme-string>
;;
;; Response payload (comparing):
;;   <payload>      =  "(" 1*( "(" <host-id> <diff-result> ")" ) ")"
;;   <diff-result>  =  "similar" <WSP>
;;   <diff-result>  =/ "different" <WSP> <diff>
;;   <diff-result>  =/ "error" <WSP> <error-message>
;;
;;   <diff>          = <scheme-string>
;;   <error-message> = <scheme-string>
;;
(define-public *cmd-diff*                 6)


;; Get value of an option.
;;
;; Request payload:
;;   <payload>      = "(option . " <option> ")"
;;   <option>       = <scheme-string>
;;
;; Response payload:
;;   <payload>      = "(option . " <scheme-symbol> ")"
;;                    "(value  . " <scheme-object> ")"
;;
(define-public *cmd-get*                  7)


;; Set a new value to an option.
;;
;; Request payload:
;;   <payload>      = "(option . " <scheme-symbol> ")"
;;                    "(value  . " <scheme-object> ")"
;;
(define-public *cmd-set*                  8)


;; Stop the lazycat daemon
(define-public *cmd-stop*                 10)


;;; server <-> proxy

;; Send a message to a host.
;;
;; Request payload:
;;   <payload>      = "(address . " <scheme-string> ")"
;;                    "(message . " <scheme-string> ")"
;;
;; Response payload:
;;   <payload>      = "(response . " <scheme-string> ")"
;;
(define-public *cmd-proxy-send*           100)


;; Set an option.
;;
;; Request payload:
;;   <payload>      = "(option . " <scheme-symbol> ")"
;;                    "(value  . " <scheme-object> ")"
;;
(define-public *cmd-proxy-set*            101)


;; Get value of an option.
;;
;; Request payload:
;;   <payload>      = "(option . " <scheme-symbol> ")"
;;
;; Response payload:
;;   <payload>      = "(option . " <scheme-symbol> ")"
;;                    "(value  . " <scheme-object> ")"
;;
(define-public *cmd-proxy-get*            102)


;; List all options.
;;
;; Response payload:
;;   <payload>      = "(object-list . " <options-list> ")"
;;   <options-list> = "(" *<scheme-pair> ")"
;;
(define-public *cmd-proxy-list-options*   103)


;; Ping a host.
;;
;; Request payload:
;;   <payload>      = "(address . " <scheme-string> ")"
;;
;; Response payload:
;;   <payload>      = "(status . " <scheme-boolean> ")"
;;
(define-public *cmd-proxy-ping*           104)


;; Stop the proxy process
(define-public *cmd-proxy-stop*           110)

;;; protocol.scm ends here.

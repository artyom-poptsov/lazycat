#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
export GUILE_LOAD_PATH=__DATA_DIR__
main='(module-ref (resolve-module '\''(lazycat ui lc)) '\'main')'
exec ${GUILE-guile} -l $0 -c "(apply $main (command-line))" "$@"
!#


;;; lc -- A CLI for LazyCat.

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

;; A CLI for LazyCat.
;;
;; Usage: lc <command> [ args ]
;;
;; Possible commands:
;;   a, add     -- add a new host
;;   d, diff    -- compare outputs from hosts
;;   e, exec    -- execute a command
;;   l, list    -- list objects
;;   r, rem     -- remove host
;;   s, set     -- set a new value for an option
;;   stop       -- stop LazyCat daemon
;;   version    -- print current version
;;
;; See "<command> --help" for more information.


;;; Code:

(define-module (lazycat ui lc)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (lazycat ui cli pretty-format)
  #:use-module (lazycat protocol)
  #:use-module (lazycat message)
  #:use-module (lazycat utils)
  #:export (<lc> main))


;;; Constants

(define *version* "0.2")

(define *error-connection-lost*
  "connection to the server lost\n")


;;; Main class

(define-class <lc> ()
  (server-socket
   #:setter set-socket
   #:getter get-socket)

  (server-socket-path
   #:getter get-server-socket-path
   #:init-value "/tmp/lazycat/server")

  (server-port
   #:getter get-server-port
   #:setter set-server-port))

(define-method (initialize (obj <lc>) args)
  (next-method)
  (set-server-port obj (socket PF_UNIX SOCK_STREAM 0))
  (connect (get-server-port obj) AF_UNIX (get-server-socket-path obj)))


;;; Methods

;; Quit the application.
(define-method (lc-quit (obj <lc>))
  (close (get-server-port obj)))

;; Send a MESSAGE of TYPE to the server.
(define-method (send-message (obj <lc>) (msg-req <message>))
  (let ((server-port (get-server-port obj)))
    (message-send msg-req server-port)
    (message-recv server-port)))


;; Add a new host.
(define-method (handle-add (obj <lc>) (args <list>))

  (define option-spec
    '((help        (single-char #\h) (value #f))
      (group       (single-char #\g) (value #t))
      (name        (single-char #\n) (value #t))
      (proxy-list  (single-char #\p) (value #t))
      (address     (single-char #\a) (value #t))
      (description (single-char #\d) (value #t))))

  (define (print-help)
    (display
     (string-append
      "Usage: lc add -g <group> -n <name> -p <proxy1>\n"
      "              -a <address> -d <description>\n"
      "Options:\n"
      "  -g <group>, --group=<group>                    Group for the new host\n"
      "  -n <name>, --name=<name>                       Host name\n"
      "  -p <proxy>, --proxy=<proxy>                    Proxy which will be used to reach the host\n"
      "  -a <address>, --address=<address>              Host address\n"
      "  -d <description>, --description=<description>  Description of the host\n"
      "\n"
      "Example:\n"
      "  $ lc add -g home-network -n lazy-pc1 -p ssh-proxy \\\n"
      "           -a \"avp@pc1:22\" -d \"A lazy computer\"\n")))

  (if (null? args)

      (print-help)

      (let* ((args         (cons "handle-add" args))
             (options      (getopt-long args option-spec))
             (help-needed? (option-ref options 'help        #f))
             (group        (option-ref options 'group       #f))
             (name         (option-ref options 'name        #f))
             (proxy-list   (option-ref options 'proxy-list  #f))
             (address      (option-ref options 'address     #f))
             (description  (option-ref options 'description "")))

        (if help-needed?

            (print-help)

            (let ((msg-req (make <message>
                             #:type         *cmd-add-host*
                             #:request-flag #t)))

              (message-field-set! msg-req 'group       group)
              (message-field-set! msg-req 'name        name)

              ;; FIXME: Temporary solution
              (message-field-set! msg-req 'proxy-list  (list proxy-list))

              (message-field-set! msg-req 'address     address)
              (message-field-set! msg-req 'description description)

              (let ((msg-rsp (send-message obj msg-req)))
                (if msg-rsp
                    (if (message-error? msg-rsp)
                        (format-error-message msg-rsp))
                    (format-error *error-connection-lost*))))))))

;; Remove a host.
(define-method (handle-rem (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc rem <host-id>\n"
      "\n"
      "Example:\n"
      "  $ lc rem 5\n")))

  (if (or (null? args)
          (string=? (car args) "--help") (string=? (car args) "-h"))

      (print-help)

      (let ((msg-req (make <message> #:type *cmd-rem-host* #:request-flag #t)))
        (message-field-set! msg-req 'host-id (string->number (car args)))

        (message-format msg-req)    ;DEBUG

        (let ((msg-rsp (send-message obj msg-req)))
          (if msg-rsp
              (if (message-error? msg-rsp)
                  (format-error-message msg-rsp))
              (format-error *error-connection-lost*))))))


;;; exec handler

;; A function that handles command line for 'exec' command.
(define-method (handle-exec (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc exec [ --host-id <host-id> | -n <host-id> ] <command>\n"
      "\n"
      "Examples:\n"
      "  $ lc exec uname -a\n"
      "  $ lc exec --host-id 2 uptime\n")))

  (cond

   ((or (null? args)
        (string=? (car args) "--help")    (string=? (car args) "-h"))
    (print-help))

   ((or (string=? (car args) "--host-id") (string=? (car args) "-n"))
    (let ((host-id (string->number (cadr args)))
          (cmd     ""))
      (for-each (lambda (s)
                  (set! cmd (string-append cmd " " s)))
                (cddr args))
      (exec-cmd-on-host obj host-id cmd)))

   (#t
    (let ((cmd ""))
      (for-each (lambda (s)
                  (set! cmd (string-append cmd " " s)))
                args)
      (exec-cmd obj cmd)))))

;; Execute a command CMD on the every accessible host.
(define-method (exec-cmd (obj <lc>) (command <string>))
  (let ((msg-req (make <message> #:type *cmd-exec* #:request-flag #t)))
    (message-field-set! msg-req 'command command)
    (let ((msg-rsp (send-message obj msg-req)))
      (if msg-rsp
          (if (not (message-error? msg-rsp))
              (format-output-list (message-field-ref msg-rsp 'output))
              (format-error-message msg-rsp))
          (format-error *error-connection-lost*)))))

;; Execute a command CMD on a host with the given HOST-ID.
(define-method (exec-cmd-on-host (obj     <lc>)
                                 (host-id <number>)
                                 (command <string>))
  (let ((msg-req (make <message> #:type *cmd-exec* #:request-flag #t)))
    (message-field-set! msg-req 'host-id host-id)
    (message-field-set! msg-req 'command command)

    (let ((msg-rsp (send-message obj msg-req)))
      (if msg-rsp
          (if (not (message-error? msg-rsp))
              (format-output-list (message-field-ref msg-rsp 'output))
              (format-error-message msg-rsp))
          (format-error *error-connection-lost*)))))

;;;

;; Compare outputs between master host and other hosts.
(define-method (handle-diff (obj <lc>) (args <list>))

  (define option-spec
    '((help        (single-char #\h) (value #f))
      (get-pattern (single-char #\g) (value #t))
      (continue    (single-char #\c) (value #f))
      (abort       (single-char #\a) (value #f))))

  (define (print-help)
    (display
     (string-append
      "Usage: lc diff [ -h ] [ -g | -c | -a ]\n"
      "\n"
      "Parameters:\n"
      "  -h, --help                             Print this message\n"
      "  -g <command>, --get-pattern=<command>  Get pattern from a master host\n"
      "  -c, --continue                         Execute command on the rest of hosts\n"
      "  -a, --abort                            Delete the stored pattern\n"
      "\n"
      "Examples:\n"
      "  $ lc diff --get-pattern uname -a\n"
      "  $ lc diff --continue\n")))

  (if (null? args)

      (print-help)

      (let* ((args            (cons "lazycat-diff" args))
             (options         (getopt-long args option-spec))
             (help-needed?    (option-ref options 'help #f))
             (pattern         (option-ref options 'get-pattern #f))
             (coninue?        (option-ref options 'continue    #f))
             (abort?          (option-ref options 'abort       #f)))
    
        (cond

         (help-needed?
          (print-help))

         (pattern
          (let* ((command (string-join (cdr args) " ")))
            (let ((msg-req (make <message> #:type *cmd-diff* #:request-flag #t)))
              (message-field-set! msg-req 'action  'get-pattern)
              (message-field-set! msg-req 'command command)

              (let ((msg-rsp (send-message obj msg-req)))
                (if msg-rsp
                    (if (not (message-error? msg-rsp))
                        (format-output (message-field-ref msg-rsp 'output))
                        (format-error-message msg-rsp))
                    (format-error *error-connection-lost*))))))

         (continue?
          (let ((msg-req (make <message> #:type *cmd-diff* #:request-flag #t)))
            (message-field-set! msg-req 'action 'continue)

            (let ((msg-rsp (send-message obj msg-req)))
              (if msg-rsp
                  (if (not (message-error? msg-rsp))
                      (format-diff (message-field-ref msg-rsp 'output))
                      (format-error-message msg-rsp))
                  (format-error *error-connection-lost*)))))

         (abort?
          (let ((msg-req (make <message> #:type *cmd-diff* #:request-flag #t)))
            (message-field-set! msg-req 'action 'abort)

            (let ((msg-rsp (send-message obj msg-req)))
              (if msg-rsp
                  (if (message-error? msg-rsp)
                      (format-error-message msg-rsp))
                  (format-error *error-connection-lost*)))))
         (else
          (format-error (string-append
                         "Wrong action: " (symbol->string (car args)))))))))

;; Set a new value to a option.
(define-method (handle-set (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc set <option> <value>\n"
      "\n"
      "Example:\n"
      "  $ lc set master 5\n")))

  (if (or (null? args)
          (string=? (car args) "--help") (string=? (car args) "-h"))

      (print-help)

      (let ((option (string->symbol (car args)))
            (value  (cadr args))
            (msg-req (make <message> #:type *cmd-set* #:request-flag #t)))

        (message-field-set! msg-req 'option option)
        (message-field-set! msg-req 'value  value)
        (let ((msg-rsp (send-message obj msg-req)))
          (if msg-rsp
              (if (message-error? msg-rsp)
                  (format-error-message msg-rsp))
              (format-error *error-connection-lost*))))))

;; Get the value of an option.
(define-method (handle-get (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc get <option>\n"
      "\n"
      "Example:\n"
      "  $ lc get master\n")))

  (if (or (null? args)
          (string=? (car args) "--help") (string=? (car args) "-h"))

      (print-help)
  
      (let ((option (string->symbol (car args)))
            (msg-req (make <message> #:type *cmd-get* #:request-flag #t)))
        (message-field-set! msg-req 'option option)
        (let ((msg-rsp (send-message obj msg-req)))
          (if msg-rsp
              (if (not (message-error? msg-rsp))
                  (begin
                    (display (message-field-ref msg-rsp 'value))
                    (newline))
                  (format-error-message msg-rsp))
              (format-error *error-connection-lost*))))))

;; Show a list of objects of the specific type.
(define-method (handle-list (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc list <object-type>\n"
      "\n"
      "Available object types:\n"
      "  h, hosts        List hosts\n"
      "  o, options      List options\n"
      "\n"
      "Examples:\n"
      "  $ lc list hosts\n"
      "  $ lc list options\n")))

  (let ((cmd (car args)))
    (cond
     ((or (null? args) (string=? cmd "--help") (string=? cmd "-h"))
      (print-help))

     ((or (string=? cmd "hosts") (string=? cmd "h"))
      (let ((msg-req (make <message> #:type *cmd-list* #:request-flag #t)))
        (message-field-set! msg-req 'object-type 'host)

        (let ((msg-rsp (send-message obj msg-req)))
          (if msg-rsp
              (if (not (message-error? msg-rsp))
                  (format-host-list (message-field-ref msg-rsp 'object-list))
                  (format-error-message msg-rsp))
              (format-error *error-connection-lost*)))))

     ((or (string=? cmd "options") (string=? cmd "o"))
      (let ((msg-req (make <message> #:type *cmd-list* #:request-flag #t)))
        (message-field-set! msg-req 'object-type 'option)
        (let ((msg-rsp (send-message obj msg-req)))
          (if msg-rsp
              (if (not (message-error? msg-rsp))
                  (format-options-list (message-field-ref msg-rsp 'object-list))
                  (format-error-message msg-rsp))
              (format-error *error-connection-lost*)))))

     (#t
      (format-error "Unknown command.")))))

;; Stop lazycat daemon
(define-method (handle-stop (obj <lc>))
  (let ((msg-req (make <message> #:type *cmd-stop* #:request-flag #t)))
    (send-message obj msg-req)))

;;; Helper procedures

;; Get information about current version
(define-method (print-version (obj <lc>))
  (display
   (string-append
    "Version:          " *version* "\n"
    "Protocol version: " (number->string *protocol-version*) "\n")))

(define (print-help)
  (display
   (string-append
    "Usage: lc <command>\n"
    "\n"
    "Possible commands:\n"
    "  a, add     add a new host\n"
    "  d, diff    compare outputs from hosts\n"
    "  e, exec    execute a command\n"
    "  g, get     get current value of an option\n"
    "  l, list    list objects\n"
    "  r, rem     remove host\n"
    "  s, set     set a new value for an option\n"
    "  stop       stop LazyCat daemon\n"
    "  version    print information about current version\n"
    "\n"
    "Enter 'lc <command> -h' to get documentation for the command.\n")))


;;; Entry point of the program

(define (main . args)

  (if (null? (cdr args))
      (begin
        (print-help)
        (quit)))

  (let* ((lc        (make <lc>))
         (arguments (cdr args))
         (cmd       (car arguments))
         (cmd-args  (cdr arguments)))

    (case* string=? cmd

     (("version")
      (print-version lc))

     (("add" "a")
      (handle-add lc cmd-args))

     (("rem" "r")
      (handle-rem lc cmd-args))

     (("exec" "e")
      (handle-exec lc cmd-args))

     (("diff" "d")
      (handle-diff lc cmd-args))

     (("set" "s")
      (handle-set lc cmd-args))

     (("get" "g")
      (handle-get lc cmd-args))

     (("list" "l")
      (handle-list lc cmd-args))

     (("stop")
      (handle-stop lc))

     (else
      (print-help)))

    (lc-quit lc)))

;;; lc ends here.

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
  #:use-module (lazycat ui cli pretty-format)
  #:use-module (lazycat protocol)
  #:use-module (lazycat message)
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
(define-method (lazycat-add (obj <lc>) (args <list>))

  (define (print-help)
  (display
   (string-append
    "Usage: lc add <group> <name> <proxy> <address> <description>\n"
    "\n"
    "Example:\n"
    "\t" "$ lc add home-network lazy-pc1 ssh-proxy \"avp@pc1:22\" \"A lazy computer\"\n")))

  (if (or (null? args)
          (string=? (car args) "--help") (string=? (car args) "-h"))

      (print-help)

      (let ((msg-req (make <message> #:type *cmd-add-host* #:request-flag #t)))

        (message-field-set! msg-req 'group       (list-ref args 0))
        (message-field-set! msg-req 'name        (list-ref args 1))

        ;; FIXME: Temporary solution
        (message-field-set! msg-req 'proxy-list  (list (list-ref args 2)))

        (message-field-set! msg-req 'address     (list-ref args 3))
        (message-field-set! msg-req 'description (list-ref args 4))

        (let ((msg-rsp (send-message obj msg-req)))
          (if msg-rsp
              (if (message-error? msg-rsp)
                  (format-error-message msg-rsp))
              (format-error *error-connection-lost*))))))

;; Remove a host.
(define-method (lazycat-rem (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc rem <host-id>\n"
      "\n"
      "Example:\n"
      "\t" "$ lc rem 5\n")))

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

;; A function that handles command line for 'exec' command.
(define-method (lazycat-generic-exec (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc exec [ --host-id <host-id> | -n <host-id> ] <command>\n"
      "\n"
      "Examples:\n"
      "\t" "$ lc exec uname -a\n"
      "\t" "$ lc exec --host-id 2 uptime\n")))

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
      (lazycat-exec obj host-id cmd)))

   (#t
    (let ((cmd ""))
      (for-each (lambda (s)
                  (set! cmd (string-append cmd " " s)))
                args)
      (lazycat-exec obj cmd)))))

(define-generic lazycat-exec)

;; Execute a command CMD on the every accessible host.
(define-method (lazycat-exec (obj <lc>) (command <string>))
  (let ((msg-req (make <message> #:type *cmd-exec* #:request-flag #t)))
    (message-field-set! msg-req 'command command)
    (let ((msg-rsp (send-message obj msg-req)))
      (if msg-rsp
          (if (not (message-error? msg-rsp))
              (format-output-list (message-field-ref msg-rsp 'output))
              (format-error-message msg-rsp))
          (format-error *error-connection-lost*)))))

;; Execute a command CMD on a host with the given HOST-ID.
(define-method (lazycat-exec (obj <lc>) (host-id <number>) (command <string>))
  (let ((msg-req (make <message> #:type *cmd-exec* #:request-flag #t)))
    (message-field-set! msg-req 'host-id host-id)
    (message-field-set! msg-req 'command command)

    (let ((msg-rsp (send-message obj msg-req)))
      (if msg-rsp
          (if (not (message-error? msg-rsp))
              (format-output-list (message-field-ref msg-rsp 'output))
              (format-error-message msg-rsp))
          (format-error *error-connection-lost*)))))

;; Compare outputs between master host and other hosts.
(define-method (lazycat-diff (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc diff [ --get-pattern <command> | -g <command> ]\n"
      "               [ --continue | -c ] [ --abort | -a ]\n"
      "\n"
      "Parameters:\n"
      "\t" "--get-pattern, -g    Get pattern from a master host\n"
      "\t" "--continue, -c       Execute command on the rest of hosts\n"
      "\t" "--abort, -a          Delete the stored pattern\n"
      "\n"
      "Examples:\n"
      "\t" "$ lc diff --get-pattern uname -a\n"
      "\t" "$ lc diff --continue\n")))

  (cond

   ((or (null? args)
        (string=? (car args) "--help") (string=? (car args) "-h"))
    (print-help))
    
   ((or (string=? (car args) "--get-pattern") (string=? (car args) "-g"))
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

   ((or (string=? (car args) "--continue") (string=? (car args) "-c"))
    (let ((msg-req (make <message> #:type *cmd-diff* #:request-flag #t)))
      (message-field-set! msg-req 'action 'continue)

      (let ((msg-rsp (send-message obj msg-req)))
        (if msg-rsp
            (if (not (message-error? msg-rsp))
                (format-diff (message-field-ref msg-rsp 'output))
                (format-error-message msg-rsp))
            (format-error *error-connection-lost*)))))

   ((or (string=? (car args) "--abort") (string=? (car args) "-a"))
    (let ((msg-req (make <message> #:type *cmd-diff* #:request-flag #t)))
      (message-field-set! msg-req 'action 'abort)

      (let ((msg-rsp (send-message obj msg-req)))
        (if msg-rsp
            (if (message-error? msg-rsp)
                (format-error-message msg-rsp))
            (format-error *error-connection-lost*)))))

   (#t
    (format-error (string-append
                   "Wrong action: " (symbol->string (car args)))))))

;; Set a new value to a option.
(define-method (lazycat-set (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc set <option> <value>\n"
      "\n"
      "Example:\n"
      "\t" "$ lc set master 5\n")))

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
(define-method (lazycat-get (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc get <option>\n"
      "\n"
      "Example:\n"
      "\t" "$ lc get master\n")))

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
(define-method (lazycat-list (obj <lc>) (args <list>))

  (define (print-help)
    (display
     (string-append
      "Usage: lc list <object-type>\n"
      "\n"
      "Example:\n"
      "\t" "$ lc list hosts\n"
      "\t" "$ lc list options\n")))

  (cond

   ((or (null? args)
        (string=? (car args) "--help") (string=? (car args) "-h"))
    (print-help))

   ((string=? (car args) "hosts")
    (let ((msg-req (make <message> #:type *cmd-list* #:request-flag #t)))
      (message-field-set! msg-req 'object-type 'host)

      (let ((msg-rsp (send-message obj msg-req)))
        (if msg-rsp
            (if (not (message-error? msg-rsp))
                (format-host-list (message-field-ref msg-rsp 'object-list))
                (format-error-message msg-rsp))
            (format-error *error-connection-lost*)))))

   ((string=? (car args) "options")
    (let ((msg-req (make <message> #:type *cmd-list* #:request-flag #t)))
      (message-field-set! msg-req 'object-type 'option)
      (let ((msg-rsp (send-message obj msg-req)))
        (if msg-rsp
            (if (not (message-error? msg-rsp))
                (format-options-list (message-field-ref msg-rsp 'object-list))
                (format-error-message msg-rsp))
            (format-error *error-connection-lost*)))))

   (#t
    (format-error "Unknown command."))))

;; Stop lazycat daemon
(define-method (lazycat-stop (obj <lc>))
  (let ((msg-req (make <message> #:type *cmd-stop* #:request-flag #t)))
    (send-message obj msg-req)))

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
    "\t" "a, add     add a new host\n"
    "\t" "d, diff    compare outputs from hosts\n"
    "\t" "e, exec    execute a command\n"
    "\t" "g, get     get current value of an option\n"
    "\t" "l, list    list objects\n"
    "\t" "r, rem     remove host\n"
    "\t" "s, set     set a new value for an option\n"
    "\t" "stop       stop LazyCat daemon\n"
    "\t" "version    print information about current version\n"
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
         (command   (car arguments)))

    (cond

     ((string=? command "version")
      (print-version lc))

     ((or (string=? command "add")  (string=? command "a"))
      (lazycat-add lc (cdr arguments)))

     ((or (string=? command "rem")  (string=? command "r"))
      (lazycat-rem lc (cdr arguments)))

     ((or (string=? command "exec") (string=? command "e"))
      (lazycat-generic-exec lc (cdr arguments)))

     ((or (string=? command "diff") (string=? command "d"))
      (lazycat-diff lc (cdr arguments)))

     ((or (string=? command "set")  (string=? command "s"))
      (lazycat-set lc (cdr arguments)))

     ((or (string=? command "get")  (string=? command "g"))
      (lazycat-get lc (cdr arguments)))

     ((or (string=? command "list") (string=? command "l"))
      (lazycat-list lc (cdr arguments)))

     ((string=? command "stop")
      (lazycat-stop lc))

     (#t
      (print-help)))

    (lc-quit lc)))

;;; lc ends here.

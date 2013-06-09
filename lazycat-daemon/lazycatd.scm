;;; lazycatd.scm -- LazyCat daemon.

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;
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

;; A daemon that communicates with hosts and processes the data.
;; 
;; These methods are exported:
;; 
;;   (run obj)


;;; Code:

(define-module (lazycat lazycat-daemon lazycatd)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (lazycat proxy)
  #:use-module (lazycat protocol)
  #:use-module (lazycat logger)
  #:use-module (lazycat lazycat-daemon host)
  #:use-module (lazycat lazycat-daemon host-list)
  #:use-module (lazycat lazycat-daemon proxy-list)
  #:use-module (lazycat lazycat-daemon diff)
  #:export (<lazycatd> run))


;;; Constants

(define *syslog-tag* "lazycatd")

;; Possible object types
(define *object-types* '(hosts options))

;; Accessible options
(define *options* '(master log-verbosity))


;;; Main class
(define-class <lazycatd> ()

  (no-detach-mode
   #:init-keyword #:no-detach-mode
   #:getter       no-detach?
   #:init-value   #t)

  (debug-mode
   #:init-keyword #:debug-mode
   #:getter       debug?
   #:init-value   #f)

  (lazycat-home
   #:setter set-home!
   #:getter get-home)

  (tmp-dir
   #:setter set-tmp-dir!
   #:getter get-tmp-dir
   #:init-value "/tmp/lazycat/")
  
  (socket-path
   #:setter set-socket-path!
   #:getter get-socket-path
   #:init-value "/tmp/lazycat/server")

  (lazycatd-socket
   #:setter set-socket!
   #:getter get-socket)

  (logger
   #:getter get-logger
   #:init-value (make <logger>
                  #:ident    *syslog-tag*
                  #:facility 'user))

  (options
   #:getter get-options
   #:init-value (make-hash-table))

  (host-list
   #:setter set-host-list!
   #:getter get-host-list)

  (proxy-list
   #:setter set-proxy-list!
   #:getter get-proxy-list)

  (pattern-output
   #:setter set-pattern!
   #:getter get-pattern
   #:init-value #f))

(define-method (initialize (obj <lazycatd>) args)
  (next-method)

  (set-home! obj (string-append (getenv "HOME") "/.lazycat"))

  (if (not (file-exists? (get-tmp-dir obj)))
      (mkdir (get-tmp-dir obj)))
  
  (set-host-list! obj (make <host-list> #:lazycat-home (get-home obj)))
  (set-proxy-list! obj (make <proxy-list> #:tmp-dir (get-tmp-dir obj)))

  (host-list-load (get-host-list obj))

  ;; Load options
  (let ((options (get-options obj)))
    (hash-set! options 'master        #f)
    (hash-set! options 'log-verbosity (if (debug? obj) "4" "1")))

  (let ((host-list (get-host-list obj)))
    (if (not (host-list-empty? host-list))
        ;; Set default master host (the 1st host from the list)
        ;; TODO: It'll be good idea to store info about master host between
        ;;       sessions.
        (let* ((list (host-list-get-plain-list host-list))
               (master-host-id (host-get-id (car list))))
          (hash-set! (get-options obj) 'master (number->string master-host-id)))
        (logger-message (get-logger obj) 'info "Host list is empty"))))


;;; Helper procedures

;; Throw an exception
(define-method (lazycat-throw msg . info)
  (apply throw 'lazycat-exception msg info))

;; Log debug message.
(define-method (log-debug (obj <lazycatd>) (message <string>))
  (if (>= (string->number (hash-ref (get-options obj) 'log-verbosity)) 4)
      (logger-message (get-logger obj) 'debug message)))

;;;


;; Open a socket and start listening it.
(define-method (open-socket (obj <lazycatd>))
  (set-socket! obj (socket PF_UNIX SOCK_STREAM 0))
  (let ((path            (get-socket-path obj))
        (lazycatd-socket (get-socket obj)))

    (if (file-exists? path)
        (delete-file path))

    (bind lazycatd-socket AF_UNIX path)
    (listen lazycatd-socket 1)))


;; Create a file contains PID of the daemon.
;;
;; FIXME: Unused.
(define-method (create-pid-file (obj <lazycatd>) (pid <number>))
  (let ((pid-file (open-output-file (get-pid-file obj))))
    (write pid pid-file)))

;; Remove the PID file.
;;
;; FIXME: Unused.
(define-method (remove-pid-file (obj <lazycatd>))
  (delete-file (get-pid-file obj)))

;; Send message to a client.
(define-method (send-message (obj <lazycatd>) message (port <port>))
  (write message port)
  (newline port))


;; Stop the lazycat daemon.
(define-method (lazycat-stop (obj <lazycatd>) (port <port>))
  (host-list-save (get-host-list obj))
  (proxy-list-stop-all (get-proxy-list obj))

  (send-message obj (list #t) port)

  (close (get-socket obj)))

;; Add a new host to the host list.
(define-method (lazycat-add (obj <lazycatd>) args (client <port>))
  (if (null? args)
      (lazycat-throw "Malformed message" args))

  (let* ((args            (car args))
         (group           (car args))
         (host-attributes (cdr args))
         (host-list       (get-host-list obj)))

    (let* ((proxy-list (list-ref host-attributes 1))
           ;; FIXME: Workaround
           (proxy-list (if (list? proxy-list) proxy-list (list proxy-list))))

      (host-list-add-host host-list
                          #:group       group
                          #:name        (list-ref host-attributes 0)
                          #:proxy-list  proxy-list
                          #:address     (list-ref host-attributes 2)
                          #:description (list-ref host-attributes 3))
      (send-message obj (list #t) client))))

;; Remove the host with given HOST-ID from the host list.
(define-method (lazycat-rem (obj <lazycatd>) args (client <port>))
  (if (null? args)
      (lazycat-throw "Malformed message" args))

  (let ((host-list (get-host-list obj))
        (host-id   (car args)))
    (host-list-rem-host host-list host-id)
    (send-message obj (list #t) client)))

;; Get list of objects with the given type TYPE.
;;
;; Throws lazycat-exception on error.
(define-method (lazycat-list (obj <lazycatd>) args (client <port>))
  (if (null? args)
      (lazycat-throw "Malformed message" args))

  (let ((type (car args)))
    (if (eqv? (member type *object-types*) #f)
        (lazycat-throw "Wrong object type" type))

    (log-debug obj (string-append "lazycat-list: type: " (symbol->string type)))

    (cond

     ((eq? type 'hosts)
      (let* ((host-list (get-host-list obj))
             (response  (list #t (host-list-get-unrolled-list host-list))))
        (send-message obj response client)))

     ((eq? type 'options)
      (let* ((options  (get-options obj))
             (response (list #t (hash-map->list cons options))))
        (send-message obj response client))))))

;; Get value of an option OPTION
(define-method (lazycat-get (obj <lazycatd>) args (client <port>))
  (if (null? args)
      (lazycat-throw "Malformed message" args))

  (let ((option (car args)))

    (if (eqv? (member option *options*) #f)
        (lazycat-throw "No such option" option))

    (let ((value (hash-ref (get-options obj) option)))
      (send-message obj (list #t value) client))))

;; Set a VALUE for the OPTION.
;;
;; Throws lazycat-exception on error.
(define-method (lazycat-set (obj <lazycatd>) args (client <port>))
  (if (null? args)
      (lazycat-throw "Malformed message" args))

  (let* ((args   (car args))
         (option (car args))
         (value  (cadr args)))

    (if (eqv? (member option *options*) #f)
        (lazycat-throw "No such option" option))

    (if (hash-set! (get-options obj) option value)
        (send-message obj (list #t) client)
        (send-message obj (list #f) client))))


(define-generic lazycat-exec)

;; Execute a command and send output to the CLIENT.
(define-method (lazycat-exec (obj <lazycatd>) args (client <port>))
  (if (null? args)
      (lazycat-throw "Malformed message" args))

  (if (list? (car args))

      (let* ((host-id (caar args))
             (command (cadar args))
             (result  (lazycat-exec obj host-id command)))
        (send-message obj (list #t result) client))

      (let* ((command (car args))
             (result (lazycat-exec obj command)))
        (send-message obj (list #t result) client))))

;; Execute a command COMMAND on a host with given HOST-ID.
;;
;; Return:
;;   <result> ::= (<host-id> (<status> <output>))
;;   <status> ::= <boolean>
;;   <output> ::= <string>
;;
(define-method (lazycat-exec (obj     <lazycatd>)
                             (host-id <number>)
                             (command <string>))
  (let* ((host-list    (get-host-list obj))
         (proxy-list   (get-proxy-list obj))
         (host         (host-list-get-host-by-id host-list host-id)))

    (if (not host)
        (lazycat-throw "Couldn't find the host with the given ID" host-id))

    (let* ((host-addr    (host-get-address host))
           (host-proxies (host-get-proxy-list host))
           ;; FIXME: We use first proxy from the list for now.
           (proxy        (proxy-list-get proxy-list (car host-proxies))))

      (let ((response (proxy-send-message proxy host-addr command)))
        (list host-id response)))))

;; Execute the command COMMAND on all accessible hosts.
;;
;; Return a list in the following format:
;;
;;   <result> ::= (<status> <response>)
;;   <response>    ::= ((<host-id> <host-result>) ...) | ( <error-message> )
;;   <host-id>     ::= <number>
;;   <host-result> ::= (<status> <output>)
;;   <output>      ::= <string>
;;   <status>      ::= <boolean>
;;
(define-method (lazycat-exec (obj <lazycatd>) (command <string>))

  (log-debug obj (string-append "lazycat-exec: " command))

  (let* ((host-list  (get-host-list obj))
         (plain-list (host-list-get-plain-list host-list))
         (result     '()))

    (for-each

     (lambda (host)
       (let* ((host-id  (host-get-id host))
              (response (lazycat-exec obj (list host-id command))))
         (set! result (cons response result))))

     plain-list)

    result))

;; Compare output from hosts with an output from the master host.
;; Return diffs.  Throw lazycat-diff-error on error.
(define-method (lazycat-diff (obj <lazycatd>) args (client <port>))

  ;; Return:
  ;;   <result> ::= ( <host-id> ( <status> <output> ) )
  ;;   <status> ::= 'similar | 'different | 'error
  (define (fetch-and-analyse host pattern message)
    (let* ((host-id  (host-get-id host))

           ;; <result> ::= ( <host-id> ( <status> <output> ) )
           (result   (lazycat-exec obj (list host-id message)))

           (result   (cadr result))
           (success? (car result))
           (output   (cadr result)))

      (if success?

          (let ((diff (make-string-diff (get-tmp-dir obj) pattern output)))
            (if (diff-empty? diff)
                (list host-id 'similar #f )
                (list host-id 'different (diff-get diff))))

          (list host-id (list 'error (cadr output))))))


  (if (null? args)
      (lazycat-throw "Malformed message" args))

  (let* ((args   (car args))
         (action (car args)))
    (cond

     ((eq? action 'get-pattern)
      (let* ((command (cadr args))
             (master (string->number (hash-ref (get-options obj) 'master)))
             (result (cadr (lazycat-exec obj (list master command)))))

        (set-pattern! obj (list command (cadr result)))

        ;; <message> ::= ( #t ( ( <master-host-id> ( <status> <output> ) ) ) )
        (send-message obj (list #t (list master result)) client)))

     ((eq? action 'continue)
      (if (not (eq? (get-pattern obj) #f))
          (let* ((host-list  (get-host-list obj))
                 (plain-list (host-list-get-plain-list host-list))
                 (pattern    (get-pattern obj))
                 (result     '()))

            (for-each
             (lambda (host)
               (let* ((cmd (car pattern))
                      (ptn (cadr pattern))
                      (output (fetch-and-analyse host ptn cmd)))
                 (set! result (cons output result))))
             plain-list)

            (send-message obj (list #t result) client))

          (lazycat-throw "No pattern found")))

     ((eq? action 'abort)
      (begin
        (set-pattern! obj #f)
        (send-message obj (list #t '()) client)))

     (#t
      (lazycat-throw "Wrong action" action)))))


(define-method (main-loop (obj <lazycatd>))

  ;; Wrapper for accept() that catch errors.
  (define (accept-and-catch socket)
    (catch 'system-error
      (lambda ()
        (accept socket))
      (lambda (key . args)
        (logger-message (get-logger obj) 'err "accept() call was interrupted."))))

  (let ((lazycatd-socket (get-socket obj)))

    (while #t

      (let* ((client-connection (accept-and-catch lazycatd-socket))
             (client            (car client-connection))
             (raw-message       (read-line client 'trim)))

        (if (eof-object? raw-message)
            (continue))

        (let* ((message        (read (open-input-string raw-message)))
               (message-type   (car message)))

          ;; Debug
          (log-debug obj (string-append
                          "main-loop: message type: "
                          (number->string message-type)))

          (catch 'lazycat-exception

            (lambda ()
              (cond

               ;; Get protocol version
               ((= message-type *cmd-get-protocol-version*)
                (send-message obj (list #t *protocol-version*) client))

               ;; List objects
               ((= message-type *cmd-list*)
                (lazycat-list obj (cdr message) client))

               ;; Add a new host
               ((= message-type *cmd-add-host*)
                (lazycat-add obj (cdr message) client))

               ;; Remote a host
               ((= message-type *cmd-rem-host*)
                (lazycat-rem obj (cdr message) client))

               ;; Get an option value
               ((= message-type *cmd-get*)
                (lazycat-get obj (cdr message) client))

               ;; Set an option
               ((= message-type *cmd-set*)
                (lazycat-set obj (cdr message) client))

               ;; Execute a command
               ((= message-type *cmd-exec*)
                (lazycat-exec obj (cdr message) client))

               ;; Get a diff
               ((= message-type *cmd-diff*)
                (lazycat-diff obj (cdr message) client))

               ;; Stop the daemon
               ((eq? message-type *cmd-stop*)
                (begin
                  (lazycat-stop obj client)
                  (break)))))

            (lambda (key . args)
              (let ((error-message (car args)))
                (logger-message (get-logger obj) 'warning error-message)
                (send-message obj (list #f error-message) client)))))

          (close client)))))


(define-method (run (obj <lazycatd>))
  (if (no-detach? obj)

      ;; No-detach mode.  Don't detach from a terminal, and don't
      ;; become a daemon.
      (begin
        (proxy-list-load (get-proxy-list obj))
        (open-socket obj)
        (main-loop   obj))

      ;; Regular mode.
      (let ((pid (primitive-fork)))
        (if (zero? pid)

            (begin
              (close-port (current-input-port))
              (close-port (current-output-port))
              (close-port (current-error-port))

              (setsid)

              ;; This call is here because we want to get a nice
              ;; process hierarhy in process manager such as htop.
              ;; So all proxy processes will be descendants of
              ;; lazycat-daemon.
              (proxy-list-load (get-proxy-list obj))
              
              (open-socket   obj)
              (main-loop     obj))

            (begin
              ;; FIXME: Fix it.
              ;; (create-pid-file obj pid)
              (quit))))))

;;; lazycatd.scm ends here.

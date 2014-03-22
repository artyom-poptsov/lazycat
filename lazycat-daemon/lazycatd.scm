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
  #:use-module (ice-9 threads)
  #:use-module (ice-9 getopt-long)
  ;; Logging
  #:use-module (logging logger)
  #:use-module (logging rotating-log)
  #:use-module (logging port-log)
  ;; LazyCat modules
  #:use-module (lazycat utils)
  #:use-module (lazycat proxy)
  #:use-module (lazycat protocol)
  #:use-module (lazycat message)
  #:use-module (lazycat translator)
  #:use-module (lazycat lazycat-daemon host)
  #:use-module (lazycat lazycat-daemon host-list)
  #:use-module (lazycat lazycat-daemon proxy-list)
  #:use-module (lazycat lazycat-daemon diff)
  #:use-module (lazycat lazycat-daemon curiosity)
  #:use-module (lazycat lazycat-daemon periodical-ping)
  #:use-module (lazycat lazycat-daemon translator-list)
  #:export (<lazycatd> run))


;;; Constants

(define *syslog-tag* "lazycatd")

;; Possible object types
(define *object-types* '(host option))

;; Accessible options
(define *options* '(master log-verbosity ping-interval))

(define *default-ping-interval* 30)     ;Seconds

;; The pid file placed in /tmp only for debugging.
(define *pid-file* "/tmp/lazycat/lazycat.pid")

;;; lazycatd instance
(define lcd #f)


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

  (options
   #:getter get-options
   #:init-value (make-hash-table))

  (pattern-output
   #:setter set-pattern!
   #:getter get-pattern
   #:init-value #f))


(define-method (initialize (obj <lazycatd>) args)
  (next-method)

  (set-home! obj (string-append (getenv "HOME") "/.lazycat"))

  (if (not (file-exists? (get-tmp-dir obj)))
      (mkdir (get-tmp-dir obj)))

  (let ((hlist (make <host-list> #:lazycat-home (get-home obj))))
    (set-default-host-list! hlist))

  (let ((plist (make <proxy-list>
                 #:tmp-dir    (get-tmp-dir obj)
                 #:debug-mode (debug? obj))))
    (set-default-proxy-list! plist))

  (host-list-load)

  ;; Load options
  (let ((options (get-options obj)))
    (hash-set! options 'master        #f)
    (hash-set! options 'log-verbosity (if (debug? obj) "4" "1"))
    (hash-set! options 'ping-interval (number->string *default-ping-interval*)))

  (if (not (host-list-empty?))
      ;; Set default master host (the 1st host from the list)
      ;; TODO: It'll be good idea to store info about master host between
      ;;       sessions.
      (let* ((list (host-list-get-plain-list))
             (master-host-id (host-get-id (car list))))
        (hash-set! (get-options obj) 'master (number->string master-host-id)))
      (log-msg 'INFO "Host list is empty")))


;;; Helper procedures

(define-method (setup-logging)

  (let ((lgr       (make <logger>))
        (rotating  (make <rotating-log>
                     #:num-files 1
                     #:size-limit 10000
                     #:file-name "/tmp/lazycat/lazycat.log")))

    (if (no-detach? lcd)
        (let ((err (make <port-log> #:port (current-error-port))))
          (if (not (debug? lcd))
              (begin
                ;; don't want to see warnings or info on the screen!
                (disable-log-level! err 'WARN)
                (disable-log-level! err 'INFO)
                (disable-log-level! err 'DEBUG)))
          (add-handler! lgr err)))

    ;; add the handlers to our logger
    (add-handler! lgr rotating)
    ;; make this the application's default logger
    (set-default-logger! lgr)
    (open-log! lgr)))

(define (shutdown-logging)
  (flush-log)   ;; since no args, it uses the default
  (close-log!)  ;; since no args, it uses the default
  (set-default-logger! #f))

;; Throw an exception
(define-method (lazycat-throw msg . info)
  (apply throw 'lazycat-exception msg info))

;;;


(define (open-socket)
  "Open a socket and start listening it."
  (set-socket! lcd (socket PF_UNIX SOCK_STREAM 0))
  (let ((path            (get-socket-path lcd))
        (lazycatd-socket (get-socket lcd)))

    (if (file-exists? path)
        (delete-file path))

    (bind lazycatd-socket AF_UNIX path)
    (listen lazycatd-socket 1)))


(define (create-pid-file pid)
  "Create a file that contains the PID of the daemon."
  (let ((pid-file (open-output-file *pid-file*)))
    (write pid pid-file)
    (close-port pid-file)))

(define (remove-pid-file)
  "Remove the PID file."
  (delete-file *pid-file*))

(define (send-message message port)
  "Send message to a client."
  (write message port)
  (newline port))


;;; Request handlers

(define (handle-stop-req port)
  "Stop the lazycat daemon."
  (host-list-save)
  (proxy-list-stop-all)

  (let ((msg-rsp (make <message> #:type *cmd-stop*)))
    (message-send msg-rsp port))

  (close (get-socket lcd))
  (shutdown-logging))

;; 
(define (handle-add-req msg-req client)
  "Add a new host to the host list."
  (let ((group       (message-field-ref msg-req 'group))
        (name        (message-field-ref msg-req 'name))
        (proxy-list  (message-field-ref msg-req 'proxy-list))
        (address     (message-field-ref msg-req 'address))
        (description (message-field-ref msg-req 'description)))

    (host-list-add-host #:group       group
                        #:name        name
                        #:proxy-list  proxy-list
                        #:address     address
                        #:description description)

    (let ((msg-rsp (make <message> #:type *cmd-add-host*)))
      (message-send msg-rsp client))))

(define (handle-rem-req msg-req client)
  "Remove a host from the host list."
  (let ((host-id (message-field-ref msg-req 'host-id)))

    (if (or (not host-id) (null? host-id))
        (lazycat-throw "Malformed message"))

      (catch 'no-such-host
        (lambda ()
          (host-list-rem-host host-id))
        (lambda (key . args)
          (lazycat-throw "No such host" args)))

      (let ((msg-rsp (make <message> #:type *cmd-rem-host*)))
        (message-send msg-rsp client))))

(define (handle-list-req msg-req client)
  "Get list of objects with the given type.
Throw lazycat-exception on error."
  (let ((object-type (message-field-ref msg-req 'object-type)))

    (if (not object-type)
        (lazycat-throw "Malformed message"))

    (if (eqv? (member object-type *object-types*) #f)
        (lazycat-throw "Wrong object type" object-type))

    (log-msg 'DEBUG (string-append "lazycat-list: type: "
                                   (symbol->string object-type)))

    (case object-type

     ((host)
      (let ((host-id   (message-field-ref msg-req 'host-id))
            (msg-rsp   (make <message> #:type *cmd-list*)))
        (if host-id
            (let ((host (host-list-get-host-by-id host-id)))
              (let ((serialized-host (host-serialize/state host)))
                (message-field-set! msg-rsp 'serialized-host serialized-host)
                (message-send msg-rsp client)))

            (let* ((object-list (host-list-get-serialized-list)))
              (message-field-set! msg-rsp 'object-list object-list)
              (message-send msg-rsp client)))))

     ((option)
      (let* ((options     (get-options lcd))
             (object-list (hash-map->list cons options))
             (msg-rsp     (make <message> #:type *cmd-list*)))
        (message-field-set! msg-rsp 'object-list object-list)
        (message-send msg-rsp client))))))

(define (handle-get-req msg-req client)
  "Get value of an option."
  (let ((option (message-field-ref msg-req 'option)))

    (if (or (not option) (null? option))
        (lazycat-throw "Malformed message"))

    (if (eqv? (member option *options*) #f)
        (lazycat-throw "No such option" option))

    (let ((value   (hash-ref (get-options lcd) option))
          (msg-rsp (make <message> #:type *cmd-get*)))
      (message-field-set! msg-rsp 'option option)
      (message-field-set! msg-rsp 'value value)
      (message-send msg-rsp client))))

(define (handle-set-req msg-req client)
  "Set a value for the given option.
Throw lazycat-exception on error."
  (let ((option (message-field-ref msg-req 'option))
        (value  (message-field-ref msg-req 'value)))

    (if (or (not option) (null? option))
        (lazycat-throw "Malformed message"))

    (if (eqv? (member option *options*) #f)
        (lazycat-throw "No such option" option))

    (let ((msg-rsp (make <message> #:type *cmd-set*)))
      (if (hash-set! (get-options lcd) option value)
          (message-send msg-rsp client)
          (begin
            (message-set-error-flag! msg-rsp #t)
            (message-field-set!      msg-rsp 'error "Couldn't set option")
            (message-send msg-rsp client))))))


;;; exec request

(define (translate-command host command)
  (let ((trans (get-translator-for-host host)))
    (if (not trans)
        (lazycat-throw (string-append
                        "Couldn't find an appropriate"
                        "translator for the host.")
                       (host-get-id host)))
    (translate trans command)))

;; Execute a command COMMAND on a host with given HOST-ID.
;;
;; Return:
;;   <result>       = "(" <host-id> "(" <status> <WSP> <output> "))"
;;   <host-id>      = <scheme-number>
;;   <status>       = <scheme-boolean>
;;   <output>       = <scheme-string>
;;
(define (exec-cmd-on-host translate? host-id command)
  "Execute a command on a host with given host ID."
  (let ((host (host-list-get-host-by-id host-id)))

    (if (not host)
        (lazycat-throw "Couldn't find the host with the given ID" host-id))
    (let* ((host-addr    (host-get-address host))
           (host-proxies (host-get-proxy-list host))
           ;; FIXME: We use first proxy from the list for now.
           (proxy        (proxy-list-get-proxy (car host-proxies)))
           (command      (if translate?
                             (translate-command host command)
                             (string-join command " "))))

      (let ((msg-proxy-rsp (proxy-send-message proxy host-addr command)))
        (if (not (message-error? msg-proxy-rsp))
            (let ((response (message-field-ref msg-proxy-rsp 'response)))
              (list host-id (list #t response)))
            (let ((error-message (message-field-ref msg-proxy-rsp 'error)))
              (list host-id (list #f error-message))))))))

;; Execute the command COMMAND on all accessible hosts.
;;
;; Return a list in the following format:
;;
;;   <result> = "(" 1*( "(" <host-id> "(" <status> <WSP> <output> "))" ) ")"
;;   <host-id>      = <scheme-number>
;;   <status>       = <scheme-boolean>
;;   <output>       = <scheme-string>
;;
(define (exec-cmd translate? cmd)
  "Execute the command CMD on all accessible hosts."
  (log-msg 'DEBUG (string-append "exec-cmd: " (object->string cmd)))

  (let* ((plain-list (host-list-get-plain-list))
         (result     '()))

    (for-each

     (lambda (host)
       (let* ((host-id  (host-get-id host))
              (response (exec-cmd-on-host translate? host-id cmd)))
         (set! result (cons response result))))

     plain-list)

    result))

(define (handle-exec-req msg-req client)
  "Execute a command and send output to the CLIENT."
  (let ((host-id (message-field-ref msg-req 'host-id))
        (command (message-field-ref msg-req 'command))
        (translate? (message-field-ref msg-req 'translate?)))
    (cond
     ((or (not command) (null? command))
      (lazycat-throw "Malformed message"))

     ((and host-id (not (null? host-id)))
      (let ((output  (exec-cmd-on-host translate? host-id command))
            (msg-rsp (make <message> #:type *cmd-exec*)))
        (message-field-set! msg-rsp 'output (list output))
        (message-send msg-rsp client)))

     ((or (not host-id) (null? host-id))
      (let ((output  (exec-cmd translate? command))
            (msg-rsp (make <message> #:type *cmd-exec*)))
        (message-field-set! msg-rsp 'output output)
        (message-send msg-rsp client))))))

;;;

(define (handle-diff-req msg-req client)
  "Compare output from hosts with an output from the master host.
Return diffs.  Throw lazycat-diff-error on error."

  ;; Return:
  ;;   <result> ::= ( <host-id> ( <status> <output> ) )
  ;;   <status> ::= 'similar | 'different | 'error
  (define (fetch-and-analyse host pattern command)
    (let* ((host-id  (host-get-id host))

           ;; <result> ::= ( <host-id> ( <status> <output> ) )
           (result   (exec-cmd-on-host host-id command))

           (result   (cadr result))
           (success? (car result))
           (output   (cadr result)))

      (if success?

          (let ((diff (make-string-diff (get-tmp-dir lcd) pattern output)))
            (if (diff-empty? diff)
                (list host-id 'similar)
                (list host-id 'different (diff-get diff))))

          (list host-id 'error output))))


  (let ((action (message-field-ref msg-req 'action)))

    (if (or (not action) (null? action))
        (lazycat-throw "Malformed message" args))

    (case action

     ((get-pattern)
      (let* ((command (message-field-ref msg-req 'command))
             (master (string->number (hash-ref (get-options lcd) 'master)))
             (result (cadr (exec-cmd-on-host master command))))

        (set-pattern! lcd (list command (cadr result)))

        (let ((msg-rsp (make <message> #:type *cmd-diff*)))
          (message-field-set! msg-rsp 'output (list master result))
          (message-send msg-rsp client))))

     ((continue)
      (if (not (eq? (get-pattern lcd) #f))
          (let* ((plain-list (host-list-get-plain-list))
                 (pattern    (get-pattern lcd))
                 (result     '()))

            (for-each
             (lambda (host)
               (let* ((cmd (car pattern))
                      (ptn (cadr pattern))
                      (output (fetch-and-analyse host ptn cmd)))
                 (set! result (cons output result))))
             plain-list)

            (let ((msg-rsp (make <message> #:type *cmd-diff*)))
              (message-field-set! msg-rsp 'output result)
              (message-send msg-rsp client)))

          (lazycat-throw "No pattern found")))

     ((abort)
      (let ((msg-rsp (make <message> #:type *cmd-diff*)))
        (set-pattern! lcd #f)
        (message-send msg-rsp client)))

     (else
      (lazycat-throw "Wrong action" action)))))


;;; Main loop

(define (main-loop)

  ;; Wrapper for accept() that catch errors.
  (define (accept-and-catch socket)
    (catch 'system-error
      (lambda ()
        (accept socket))
      (lambda (key . args)
        (log-msg 'ERROR "accept() call was interrupted."))))

  (let ((lazycatd-socket (get-socket lcd)))

    (while #t

      (let* ((client-connection (accept-and-catch lazycatd-socket))
             (client            (car client-connection))
             (msg-req           (message-recv client)))

        ;; EOF is received
        (if (not msg-req)
            (continue))

        (let ((message-type (message-get-type msg-req)))

          ;; Debug
          (log-msg 'DEBUG (string-append
                           "main-loop: message type: "
                           (message-type->string message-type)
                           " (" (number->string message-type) ")"))

          (catch 'lazycat-exception

            (lambda ()
              (case* = message-type

               ;; Get protocol version
               ((*cmd-get-protocol-version*)
                (let ((msg-rsp (make <message> #:type *protocol-version*)))
                  (message-send msg-rsp client)))

               ;; List objects
               ((*cmd-list*)
                (handle-list-req msg-req client))

               ;; Add a new host
               ((*cmd-add-host*)
                (handle-add-req msg-req client))

               ;; Remote a host
               ((*cmd-rem-host*)
                (handle-rem-req msg-req client))

               ;; Get an option value
               ((*cmd-get*)
                (handle-get-req msg-req client))

               ;; Set an option
               ((*cmd-set*)
                (handle-set-req msg-req client))

               ;; Execute a command
               ((*cmd-exec*)
                (handle-exec-req msg-req client))

               ;; Get a diff
               ((*cmd-diff*)
                (handle-diff-req msg-req client))

               ;; Stop the daemon
               ((*cmd-stop*)
                (begin
                  (handle-stop-req client)
                  (break)))))

            (lambda (key . args)
              (let ((error-message (car args))
                    (msg-rsp       (make <message>
                                     #:type        message-type
                                     #:answer-flag #t
                                     #:error-flag  #t)))
                (log-msg 'WARN error-message)
                (message-field-set! msg-rsp 'error error-message)
                (message-send msg-rsp client)))))

          (close client)))))

;;; Signal handling

(define (handle-signal sig)
  (log-msg 'WARN (string-append "Caught signal: " (number->string sig)))
  (quit 1))


;;; Entry point of the program.

;; Command line options specification
(define *option-spec*
  '((debug     (single-char #\d) (value #f))
    (no-detach (single-char #\D) (value #f))
    (help      (single-char #\h) (value #f))))

(define (print-help-and-exit)
  "Print the help message"
  (display
   (string-append "\
Usage: lazycat-daemon [ OPTIONS ]

Options:
  --debug, -d        Debug mode.
  --no-detach, -D    Don't detach from a terminal and don't become a
                     daemon.  Useful for debugging.
  --help, -h         Print this message and exit.
"))
  (exit 0))

(define (main args)
  "Entry point of the program.  It is called from the C code and
starts LazyCat daemon."
  (c-set-lazycat-signals)

  (let* ((options       (getopt-long args *option-spec*))
         (debug-needed? (option-ref options 'debug     #f))
         (no-detach?    (option-ref options 'no-detach #f))
         (help-needed?  (option-ref options 'help      #f)))

    (if help-needed?
        (print-help-and-exit))

    ;; Create the lazycatd instance.
    (set! lcd (make <lazycatd>
                #:no-detach-mode no-detach?
                #:debug-mode     debug-needed?))

    (setup-logging)

    (if no-detach?

        ;; No-detach mode.  Don't detach from a terminal, and don't
        ;; become a daemon.
        (let ((options (get-options lcd)))

          (create-pid-file (getpid))

          (proxy-list-load)
          (make-thread (periodical-ping options))
          (make-thread (curiosity))
          (open-socket)
          (main-loop))

        ;; Regular mode.
        (let ((pid (primitive-fork)))
          (if (zero? pid)

              (begin
                (close-port (current-input-port))
                (close-port (current-output-port))
                (close-port (current-error-port))

                ;; This is the workaround for problem with
                ;; `with-output-to-string' procedure which tries to
                ;; restore the default port even if it closed.
                (let ((p (open-output-file "/dev/null")))
                  (set-current-output-port p)
                  (set-current-error-port  p))

                (setsid)

                (let ((options (get-options lcd)))
                  (proxy-list-load)
                  (make-thread (periodical-ping options))
                  (make-thread (curiosity))
                  (open-socket)
                  (main-loop)))

              (begin
                (create-pid-file pid)
                (quit)))))))

;;; lazycatd.scm ends here.

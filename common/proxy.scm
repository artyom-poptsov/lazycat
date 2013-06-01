;;; proxy.scm -- LazyCat proxy.

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

;; Description of the LazyCat <proxy> base class.  All proxies must
;; have this class as a parent.
;;
;; Note that descendant should implement handle-* methods, otherwise
;; you'll get 'implementation-missing exception on call.


;;; Code:

(define-module (lazycat proxy)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (lazycat logger)
  #:use-module (lazycat protocol)
  #:export (<proxy> proxy-send-message
                    proxy-set-option!
                    proxy-get-option
                    proxy-list-options
                    proxy-ping
                    proxy-get-name
                    proxy-set-name!
                    proxy-set-description!
                    proxy-get-description
                    proxy-start
                    proxy-stop

                    handle-send-message
                    handle-set-option!
                    handle-get-option
                    handle-list-options
                    handle-ping
                    handle-start
                    handle-stop

                    log-debug log-info log-error

                    proxy-debug? proxy-debug!
                    proxy-error))


;; Basic class for all proxies
(define-class <proxy> ()
  ;; Proxy name
  (name
   #:init-keyword #:name
   #:accessor   name
   #:setter     proxy-set-name!
   #:getter     proxy-get-name
   #:init-value #f)

  ;; Proxy description
  (description
   #:setter proxy-set-description!
   #:getter proxy-get-description)

  ;; Directory that contains the proxy socket
  (socket-dir
   #:init-keyword #:socket-dir
   #:getter get-socket-dir)

  ;; Full path to the socket file
  (socket-file
   #:setter set-socket-file!
   #:getter get-socket-file)

  (logger
   #:accessor logger
   #:getter get-logger
   #:init-value (make <logger>
                  #:ident "lazycatd"
                  #:facility 'user))

  ;; Proxy socket
  (server-socket
   #:getter get-server-socket
   #:setter set-server-socket!)

  (client-socket
   #:getter get-client-socket
   #:setter set-client-socket!)

  (debug-mode?
   #:getter proxy-debug?
   #:setter proxy-debug!
   #:init-value #f)

  ;; Process ID of the proxy
  (pid
   #:getter get-pid
   #:setter set-pid!))


;;; Helper procedures

(define (proxy-error message . args)
  (throw 'proxy-error message args))


;;; Logging
;; Procedures for working with syslog.  They can be overloaded by
;; a child class.

;; Pretty formatting for log messages.
(define-method (format-log-message (obj <proxy>) (message <string>))
  (string-append
   "[" (proxy-get-name obj) " (" (number->string (getpid)) ")]: " message))

(define-generic log-error)
(define-method (log-error (obj <proxy>) (message <string>))
  (let ((logger (get-logger obj)))
    (logger-message logger 'err (format-log-message obj message))))

(define-generic log-info)
(define-method (log-info (obj <proxy>) (message <string>))
  (let ((logger (get-logger obj)))
    (logger-message logger 'info (format-log-message obj message))))

(define-generic log-debug)
(define-method (log-debug (obj <proxy>) (message <string>))
  (if (proxy-debug? obj)
      (let ((logger (get-logger obj)))
        (logger-message logger 'debug (format-log-message obj message)))))


;;; Handlers
;; All these methods should be implemented in a derivative class.

;; Send MESSAGE to a host that is accessible by ADDRESS.
;; Return response from the host.
(define-generic handle-send-message)
(define-method (handle-send-message (obj     <proxy>)
                                    (address <string>)
                                    (message <string>))
  (throw 'implementation-missing "handle-send-message"))

;; Set an option OPTION to value VALUE.
(define-generic handle-set-option!)
(define-method (handle-set-option! (obj    <proxy>)
                                   (option <symbol>)
                                   value)
  (throw 'implementation-missing "handle-set-option"))

;; Get an option OPTION.
(define-generic handle-get-option)
(define-method (handle-get-option (obj    <proxy>)
                                  (option <symbol>))
  (throw 'implementation-missing "handle-get-option"))

;; List all accessible proxy options.
(define-generic handle-list-options)
(define-method (handle-list-options (obj <proxy>))
  (throw 'implementation-missing "handle-list-options"))

;; Returns #t if host is online, or #f if it's not.
(define-generic handle-ping)
(define-method (handle-ping (obj     <proxy>)
                            (address <string>))
  (throw 'implementation-missing "handle-ping"))

(define-generic handle-start)
(define-method (handle-start (obj <proxy>))
  (log-info obj "Started.")
  #t)

(define-generic handle-stop)
(define-method (handle-stop (obj <proxy>))
  (log-info obj "Stopped.")
  #t)


;;; Interface

(define-method (proxy-send-message (obj     <proxy>)
                                   (address <string>)
                                   (message <string>))
  (send-request obj *cmd-proxy-send* (list address message)))

(define-method (proxy-set-option! (obj <proxy>) (option <symbol>) value)
  (send-request obj *cmd-proxy-set* (list option value)))

(define-method (proxy-get-option (obj <proxy>) (option <symbol>))
  (send-request obj *cmd-proxy-get* (list option)))

(define-method (proxy-list-options (obj <proxy>))
  (send-request obj *cmd-proxy-list-options* '()))

(define-method (proxy-ping (obj     <proxy>)
                           (address <string>))
  (send-request obj *cmd-proxy-ping* (list address)))


;; Start the proxy
(define-method (proxy-start (obj <proxy>))
  (set-socket-file! obj (string-append
                         (get-socket-dir obj) "/" (proxy-get-name obj)))

  (let ((pid (primitive-fork)))
    (cond

     ;; Proxy process
     ((zero? pid)
      (begin
        (open-socket obj)
        (main-loop obj)))

     ((> 0)
      (begin
        (set-pid! obj pid)
        (set-client-socket! obj (socket PF_UNIX SOCK_STREAM 0))
        (handle-start obj)))

     ;; Error
     (#t
      (throw 'fork-error)))))


;; Stop the proxy
(define-method (proxy-stop (obj <proxy>))
  (send-request obj *cmd-proxy-stop* '()))


;;; Proxy process implementation

(define-method (open-socket (obj <proxy>))
  (set-server-socket! obj (socket PF_UNIX SOCK_STREAM 0))

  (let ((path         (get-socket-file obj))
        (proxy-socket (get-server-socket obj)))

    (if (file-exists? path)
        (delete-file path))

    (bind proxy-socket AF_UNIX path)
    (listen proxy-socket 1)))


(define-method (send-response (obj <proxy>) response (port <port>))
  (write response port)
  (newline port))

;; Send message to the real proxy
(define-method (send-request (obj <proxy>) (type <number>) message)
  (set-client-socket! obj (socket PF_UNIX SOCK_STREAM 0))
  (connect (get-client-socket obj) AF_UNIX (get-socket-file obj))

  (let ((message (list type message))
        (proxy-port (get-client-socket obj)))

    ;; Send the message
    (write message proxy-port)
    (newline proxy-port)

    ;; Receive a response
    (let ((output (let r ((output "")
                          (line   ""))
                    (if (not (eof-object? line))
                        (r (string-append output line "\n")
                           (read-line proxy-port 'trim))
                        output))))

      (close (get-client-socket obj))

      (read (open-input-string output)))))


;; Main loop of the proxy process.
(define-method (main-loop (obj <proxy>))

  ;; Wrapper for accept() that catch errors.
  (define (accept-and-catch socket)
    (catch 'system-error
      (lambda ()
        (accept socket))
      (lambda (key . args)
        (log-error obj "accept() call was interrupted."))))

  (let* ((proxy-socket (get-server-socket obj)))

    (log-info obj "Server connected.")

    (while #t

      (let* ((client-connection (accept-and-catch proxy-socket))
             (client            (car client-connection))
             (raw-message (read-line client 'trim)))

        (if (eof-object? raw-message)
            (continue))

        (let* ((message        (read (open-input-string raw-message)))
               (message-type   (car message))
               (payload        (cadr message)))

          (log-debug obj (string-append
                          "Message type: " (number->string message-type)))

          (catch 'proxy-error

            (lambda ()
              (cond
               ((= message-type *cmd-proxy-send*)
                (let ((res (handle-send-message obj (car payload) (cadr payload))))
                  (send-response obj (list #t res) client)))

               ((= message-type *cmd-proxy-set*)
                (let ((res (handle-set-option! obj (car payload) (cadr payload))))
                  (send-response obj (list #t res) client)))

               ((= message-type *cmd-proxy-get*)
                (let ((res (handle-get-option obj (car payload))))
                  (send-response obj (list #t res) client)))

               ((= message-type *cmd-proxy-list-options*)
                (let ((res (handle-list-options obj)))
                  (send-response obj (list #t res) client)))

               ((= message-type *cmd-proxy-ping*)
                (let ((res (handle-ping obj (car payload))))
                  (send-response obj (list #t res) client)))

               ((= message-type *cmd-proxy-stop*)
                (begin
                  (send-response obj '(#t ()) client)
                  (close client)
                  (handle-stop obj)
                  (quit)))))

            (lambda (key . parameters)
              (send-response obj (list #f parameters) client))))

        (close client)))))
      

          ;; TODO: Add error handling here.

;;; proxy.scm ends here

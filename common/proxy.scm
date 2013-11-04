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

  ;; Logging
  #:use-module (logging logger)

  #:use-module (lazycat protocol)
  #:use-module (lazycat message)
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

                    proxy-log-msg

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

  (mtx
   #:getter get-mutex
   #:init-value (make-mutex))

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

(define-generic proxy-log-msg)
(define-method (proxy-log-msg (obj <proxy>) (lvl <symbol>) (message <string>))
  (log-msg lvl (format-log-message obj message)))


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
  (proxy-log-msg obj 'INFO "Started.")
  #t)

(define-generic handle-stop)
(define-method (handle-stop (obj <proxy>))
  (proxy-log-msg obj 'INFO "Stopped.")
  #t)


;;; Interface

(define-method (proxy-send-message (obj     <proxy>)
                                   (address <string>)
                                   (message <string>))
 (lock-mutex (get-mutex obj))
 (let ((msg-req (make <message> #:type *cmd-proxy-send* #:request-flag #t)))
   (message-field-set! msg-req 'address address)
   (message-field-set! msg-req 'message message)
   (let ((res (send-request obj msg-req)))
     (unlock-mutex (get-mutex obj))
     res)))

(define-method (proxy-set-option! (obj <proxy>) (option <symbol>) value)
  (lock-mutex (get-mutex obj))
  (let ((msg-req (make <message> #:type *cmd-proxy-set* #:request-flag #t)))
    (message-field-set! msg-req 'option option)
    (message-field-set! msg-req 'value  value)
    (let ((res (send-request obj msg-req)))
      (unlock-mutex (get-mutex obj))
      res)))

(define-method (proxy-get-option (obj <proxy>) (option <symbol>))
  (lock-mutex (get-mutex obj))
  (let ((msg-req (make <message> #:type *cmd-proxy-get* #:request-flag #t)))
    (message-field-set! msg-req 'option option)
    (let ((res (send-request obj msg-req)))
      (unlock-mutex (get-mutex obj))
      res)))

(define-method (proxy-list-options (obj <proxy>))
  (lock-mutex (get-mutex obj))
  (let ((msg-req (make <message>
                   #:type *cmd-proxy-list-options*
                   #:request-flag #t)))
    (let ((res (send-request obj msg-req)))
      (unlock-mutex (get-mutex obj))
      res)))

(define-method (proxy-ping (obj     <proxy>)
                           (address <string>))
  (lock-mutex (get-mutex obj))
  (let ((msg-req (make <message> #:type *cmd-proxy-ping* #:request-flag #t)))
    (message-field-set! msg-req 'address address)
    (let ((res (send-request obj msg-req)))
      (unlock-mutex (get-mutex obj))
      res)))


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
  (lock-mutex (get-mutex obj))
  (let ((msg-req (make <message> #:type *cmd-proxy-stop* #:request-flag #t)))
    (send-request obj msg-req)
    (unlock-mutex (get-mutex obj))
    (waitpid (get-pid obj))))


;;; Proxy process implementation

(define-method (open-socket (obj <proxy>))
  (set-server-socket! obj (socket PF_UNIX SOCK_STREAM 0))

  (let ((path         (get-socket-file obj))
        (proxy-socket (get-server-socket obj)))

    (if (file-exists? path)
        (delete-file path))

    (bind proxy-socket AF_UNIX path)
    (listen proxy-socket 1)))

;; Send message to the real proxy
(define-method (send-request (obj <proxy>) (msg-req <message>))
  (set-client-socket! obj (socket PF_UNIX SOCK_STREAM 0))
  (catch 'system-error
    (lambda ()
      (connect (get-client-socket obj) AF_UNIX (get-socket-file obj))
      (let ((proxy-port (get-client-socket obj)))
        (message-send msg-req proxy-port)
        (message-recv proxy-port)))
    (lambda (key . args)
      (proxy-log-msg obj 'ERROR (string-append
                                 (symbol->string key) ": " (object->string args)))
      #f)))


;; Main loop of the proxy process.
(define-method (main-loop (obj <proxy>))

  ;; Wrapper for accept() that catch errors.
  (define (accept-and-catch socket)
    (catch 'system-error
      (lambda ()
        (accept socket))
      (lambda (key . args)
        (proxy-log-msg obj 'ERROR "accept() call was interrupted."))))

  (let* ((proxy-socket (get-server-socket obj)))

    (proxy-log-msg obj 'INFO "Server connected.")

    (while #t

      (let* ((client-connection (accept-and-catch proxy-socket))
             (client            (car client-connection))
             (msg-req           (message-recv client)))

        ;; EOF is received
        (if (not msg-req)
            (continue))

        (let ((message-type (message-get-type msg-req)))

          (proxy-log-msg obj 'DEBUG (string-append
                                     "Message type: "
                                     (message-type->string message-type)
                                     " (" (number->string message-type) ")"))

          (catch 'proxy-error

            (lambda ()
              (cond
               ((= message-type *cmd-proxy-send*)
                (let* ((address (message-field-ref msg-req 'address))
                       (message (message-field-ref msg-req 'message))
                       (res     (handle-send-message obj address message)))
                  (let ((msg-rsp (make <message> #:type message-type)))
                    (message-field-set! msg-rsp 'response res)
                    (message-send msg-rsp client))))

               ((= message-type *cmd-proxy-set*)
                (let ((option (message-field-ref msg-req 'option))
                      (value  (message-field-ref msg-req 'value)))
                  (handle-set-option! obj option value)
                  (let ((msg-rsp (make <message> #:type message-type)))
                    (message-send msg-rsp client))))

               ((= message-type *cmd-proxy-get*)
                (let* ((option (message-field-ref msg-req 'option))
                       (value  (handle-get-option obj option)))
                  (let ((msg-rsp (make <message> #:type message-type)))
                    (message-field-set! 'option option)
                    (message-field-set! 'value  value)
                    (message-send msg-rsp client))))

               ((= message-type *cmd-proxy-list-options*)
                (let ((object-list (handle-list-options obj))
                      (msg-rsp     (make <message> #:type message-type)))
                  (message-field-set! msg-rsp 'object-list object-list)
                  (message-send msg-rsp client)))

               ((= message-type *cmd-proxy-ping*)
                (let* ((address (message-field-ref msg-req 'address))
                       (status  (handle-ping obj address))
                       (msg-rsp (make <message> #:type message-type)))
                  (message-field-set! msg-rsp 'status status)
                  (message-send msg-rsp client)))

               ((= message-type *cmd-proxy-stop*)
                (let ((msg-rsp (make <message> #:type message-type)))
                  (message-send msg-rsp client)
                  (close client)
                  (handle-stop obj)
                  (break)))))

            (lambda (key . parameters)
              (let ((msg-rsp (make <message>
                               #:type       message-type
                               #:error-flag #t)))
                (message-field-set! msg-rsp 'error parameters)
                (message-send msg-rsp client)

                (if (= message-type *cmd-proxy-stop*)
                    (begin
                      (close client)
                      (break)))))))

        (close client)))

    (close proxy-socket)
    (quit)))
      

          ;; TODO: Add error handling here.

;;; proxy.scm ends here

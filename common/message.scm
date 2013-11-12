;;; message.scm -- LazyCat message definition.

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

;; LazyCat <message> class definition.  The class represents a message
;; format that is used for communication between a client and the
;; server, and between the server and a proxy.


;;; Code:

(define-module (lazycat message)
  #:use-module (lazycat protocol)
  #:use-module (lazycat utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (oop goops)
  #:export (<message>
            message-send
            message-recv
            message-get-type
            message-set-type!
            message-type->string
            message-serialize
            message-deserealize!
            message-set-error-flag!
            message-set-request-flag!
            message-field-set!
            message-field-ref
            message-format
            message-error?
            message-request?))


;; Message flags
(define *flag-error*   'e)
(define *flag-request* 'r)


;; Main class
(define-class <message> ()
  (type
   #:init-keyword #:type
   #:getter message-get-type
   #:setter message-set-type!)

  (request-flag
   #:init-keyword #:request-flag
   #:init-value   #f
   #:getter       get-request-flag)

  (error-flag
   #:init-keyword #:error-flag
   #:init-value   #f
   #:getter       get-error-flag)

  (flags
   #:init-value '()
   #:getter     flags-get
   #:setter     flags-set!)

  (payload
   #:init-value '()
   #:getter     payload-get
   #:setter     payload-set!))

(define-method (initialize (obj <message>) args)
  (next-method)
  (if (get-request-flag obj)
      (message-set-request-flag! obj #t))
  (if (get-error-flag obj)
      (message-set-error-flag! obj #t)))


;; Send message OBJ to the port PORT.
(define-method (message-send (obj <message>) (port <port>))
  ;; DEBUG
  ;; (message-format obj)
  ;; (newline)

  (let ((serialized-msg (message-serialize obj)))
    (write serialized-msg port)
    ;; We use '\0' as the message terminator.  Currently we assume
    ;; that '\0' does not occur in the message.  If it does, we'll
    ;; have a problem.
    (write-char #\nul port)))

;; Receive a message from the port PORT
(define-method (message-recv (port <port>))
  (let ((output (read-delimited "\0" port 'trim))
        (msg (make <message>)))

    (if (not (eof-object? output))
        (begin
          (message-deserealize! msg (read (open-input-string output)))

          ;; DEBUG
          ;; (message-format msg)
          ;; (newline)

          msg)
        #f)))                           ;EOF


;; Serialize the message OBJ to association list.
(define-method (message-serialize (obj <message>))
  (let ((msg (list (cons 'type (message-get-type obj))
                   (cons 'flags (flags-get obj)))))
    (if (not (null? (payload-get obj)))
        (set! msg (append msg (payload-get obj))))
    msg))

;; Deserealize message from association list ALIST
(define-method (message-deserealize! (obj <message>) (alist <list>))
  (message-set-type! obj (assoc-ref alist 'type))
  (flags-set! obj (assoc-ref alist 'flags))
  (payload-set! obj (cddr alist)))


;; Set the error flag 'e for the message OBJ if ENABLE is #t, remove
;; the flag otherwise.
(define-method (message-set-error-flag! (obj <message>) (enable <boolean>))
  (let ((flags (flags-get obj)))
    (if enable
        (if (not (member *flag-error* flags))
            (flags-set! obj (cons *flag-error* flags)))
        (flags-set! obj (delete *flag-error* flags)))))

;; Set the request flag 'r for the message OBJ if ENABLE is #t, remove
;; the flag otherwise.
(define-method (message-set-request-flag! (obj <message>) (enable <boolean>))
  (let ((flags (flags-get obj)))
    (if enable
        (if (not (member *flag-request* flags))
            (flags-set! obj (cons *flag-request* flags)))
        (flags-set! obj (delete *flag-request* flags)))))


;; Assign the value VALUE to the field FIELD and store it in the
;; OBJ message payload.
(define-method (message-field-set! (obj <message>) (field <symbol>) value)
  (payload-set! obj (assoc-set! (payload-get obj) field value)))

;; Get a field FIELD from the OBJ message payload.
;;
;; Return the value, or '() if the FIELD has no value, or #f if the
;; FIELD is absent.
(define-method (message-field-ref (obj <message>) (field <symbol>))
  (assoc-ref (payload-get obj) field))


;;; Predicates

;; Check if the message OBJ is an error message.
(define-method (message-error? (obj <message>))
  (if (member *flag-error* (flags-get obj)) #t #f))

;; Check if the message OBJ is an request.
(define-method (message-request? (obj <message>))
  (if (member *flag-response* (flags-get obj)) #t #f))


;;; Message utils

;; Convert the message type TYPE to a string.
(define (message-type->string type)
  (case* = type
         ((*cmd-get-protocol-version*) "get-protocol-version")
         ((*cmd-add-host*)             "add-host")
         ((*cmd-rem-host*)             "rem-host")
         ((*cmd-upd-host*)             "upd-host")
         ((*cmd-list*)                 "list")
         ((*cmd-exec*)                 "exec")
         ((*cmd-diff*)                 "diff")
         ((*cmd-get*)                  "get")
         ((*cmd-set*)                  "set")
         ((*cmd-stop*)                 "stop")
         ((*cmd-proxy-send*)           "proxy-send")
         ((*cmd-proxy-set*)            "proxy-set")
         ((*cmd-proxy-get*)            "proxy-get")
         ((*cmd-proxy-list-options*)   "proxy-list-options")
         ((*cmd-proxy-ping*)           "proxy-ping")
         ((*cmd-proxy-stop*)           "proxy-stop")))

;; Format message OBJ in human-easy-readable format.
(define-method (message-format (obj <message>))
  (format #t "Message: ~a ~a~%"
          (message-type->string (message-get-type obj))
          (flags-get obj))
  (for-each
   (lambda (field)
     (format #t "  ~10a : ~10a~%" (car field) (cdr field)))
   (payload-get obj)))

;;; message.scm ends here.


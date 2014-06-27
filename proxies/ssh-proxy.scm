;;; ssh-proxy.scm -- SSH proxy for LazyCat

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

;; SSH proxy allows to communicate with hosts through SSH protocol.
;;
;; This module exports:
;;   <ssh-proxy>


;;; Code:

(define-module (lazycat proxies ssh-proxy)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (lazycat proxy)
  #:use-module (oop goops)
  #:use-module (ssh channel)
  #:use-module (ssh session)
  #:use-module (ssh auth)
  #:use-module (ssh key)
  #:use-module (ssh version)
  #:re-export  (handle-send-message
                handle-set-option!
                handle-get-option
                handle-list-options
                handle-ping)
  #:export (<ssh-proxy>))


(define-class <ssh-proxy> (<proxy>)
  (private-key-file
   #:getter get-private-key-file
   #:setter set-private-key-file!)

  (options
   #:getter get-options
   #:init-value (make-hash-table))

  (sessions
   #:getter get-sessions
   #:init-value (make-hash-table)))

(define-method (initialize (obj <ssh-proxy>) args)
  (next-method)

  (proxy-set-name! obj "ssh-proxy")
  (proxy-set-description! obj "SSH proxy")

  ;; SSH options
  (let ((ht (get-options obj)))
    (hash-set! ht 'debug       (proxy-debug? obj))
    (hash-set! ht 'private-key (string-append (getenv "HOME") "/.ssh/lazycat")))) ;DEBUG


;;; Helper procedures

;; Parse address.  Address is expected to be in the following format:
;;   user@host:port
;;
;; Return address' components in the following format:
;;   '(username hostname port)
;;
(define (parse-address address)
  (let ((s (string-match "^(.*)@(.*):([0-9]+)" address)))

    (if (regexp-match? s)

        (let ((username (match:substring s 1))
              (hostname (match:substring s 2))
              (port     (match:substring s 3)))
          (list username hostname port))

        #f)))

(define (read-all port)
  "Read all lines from the PORT."
  (let r ((res "")
          (str (read-line port 'concat)))
    (if (not (eof-object? str))
        (r (string-append res str) (read-line port 'concat))
        res)))

;; Make a SSH session and connect to ADDRESS
(define-method (make-new-session (obj <ssh-proxy>) (address <string>))
  (proxy-log-msg obj 'DEBUG (string-append "Parse the address: " address))

  (let ((connection-data (parse-address address)))

    (or connection-data
        (proxy-error "Wrong address" address))

    (let* ((username (car connection-data))
           (hostname (cadr connection-data))
           (port     (string->number (caddr connection-data)))
           (session  #f))

      (proxy-log-msg obj 'DEBUG "Making a new SSH session...")

      (catch 'guile-ssh-error
        (lambda ()
          (set! session (make-session #:user username
                                          #:host hostname
                                          #:port port
                                          #:log-verbosity (if (proxy-debug? obj) 'protocol 'rare))))
        (lambda (key . args)
          (proxy-error (get-error session) args)))

      (proxy-log-msg obj 'DEBUG "Connect to a host.")

      (catch 'guile-ssh-error
        (lambda ()
          (let ((res (connect! session)))
            (case res
              ((ok)
               (proxy-log-msg obj 'DEBUG "Connected."))
              (else
               (proxy-error (get-error session) res)))))
        (lambda (key . args)
          (proxy-log-msg obj 'WARN (get-error session))
          (proxy-error (get-error session))))

      ;; TODO: Authenticate server
                                        ;    (ssh:authenticate-server session)

      (proxy-log-msg obj 'DEBUG "Get private and public keys.")

      (let* ((pkey-file   (hash-ref (get-options obj) 'private-key))
             (private-key (private-key-from-file session pkey-file)))

        (or private-key
            (begin
              (log-error obj session)
              (proxy-error (get-error session))))

        (let ((public-key (private-key->public-key private-key)))

          (proxy-log-msg obj 'DEBUG "Authenticate user with a public key.")

          (let ((res (userauth-pubkey! session public-key private-key)))
            (and (eqv? res 'error)
                 (begin
                   (log-error obj session)
                   (proxy-error (get-error session) res))))

          session)))))


;;; Logging
;; SSH specific logging procedures

(define-method (log-error (obj <ssh-proxy>) (session <session>))
  (proxy-log-msg obj 'ERROR (get-error session)))


;;; Interface implementation

;; Send message MESSAGE to a remote side with address ADDRESS.
;;
;; Return:
;;   <payload> ::= <output>
;;
(define-method (handle-send-message (obj     <ssh-proxy>)
                                    (address <string>)
                                    (message <string>))
  (let* ((ssh-sessions (get-sessions obj))
         (session      (hash-ref ssh-sessions address)))

    (if (not session)
        (begin
          (hash-set! ssh-sessions address (make-new-session obj address))
          (set! session (hash-ref ssh-sessions address))))

    (proxy-log-msg obj 'DEBUG "Make a new SSH channel")
    (let ((channel (make-channel session)))

      (if (not channel)
          (begin
            (log-error obj session)
            (proxy-error (get-error session))))

      (proxy-log-msg obj 'DEBUG "Open SSH session")

      (catch 'guile-ssh-error
        (lambda ()
          (begin 
            (channel-open-session channel)
            (channel-request-exec channel message)))
        (lambda (key . args)
          (begin
            (log-error obj session)
            (proxy-error (get-error session)))))

      (proxy-log-msg obj 'DEBUG "Poll SSH channel")
      (let ((res (read-all channel)))
        (close channel)
        (proxy-log-msg obj 'DEBUG "Data received")
        res))))


;; Get options list
;; 
;; Return:
;;   <payload> ::= ( ( <option> . <value> ) ... )
(define-method (handle-list-options (obj <ssh-proxy>))
  (hash-map->list cons (get-options obj)))


;; Get option value
;; 
;; Return:
;;   <payload> ::= ( <value> )
(define-method (handle-get-option (obj <ssh-proxy>) (option <symbol>))
  (hash-ref (get-options obj) option))


;; Set option value
(define-method (handle-set-option! (obj <ssh-proxy>) (option <symbol>) value)
  (hash-set! (get-options obj) option value))


;; Check if remote side with address ADDRESS is accessible.
;;
;; Return #t if the address is accessible, #f otherwise.
(define-method (handle-ping (obj <ssh-proxy>) (address <string>))
  (handle-send-message obj address "uname")
  #t)

;;; ssh-proxy.scm ends here

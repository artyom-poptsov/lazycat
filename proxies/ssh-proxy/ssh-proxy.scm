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
  #:use-module (lazycat proxy)
  #:use-module (lazycat logger)
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
  (let* ((s        (string-match "^(.*)@(.*):([0-9]+)" address))
         (username (match:substring s 1))
         (hostname (match:substring s 2))
         (port     (match:substring s 3)))
    (list username hostname port)))

;; Poll a channel CHANNEL for data and read available data.
;; Return obtained data.
(define (poll-channel channel)
  (let poll ((count #f))
    (if (or (not count) (zero? count))
        (poll (ssh:channel-poll channel #f))
        (let ((result (ssh:channel-read channel count #f)))
          (if (not result)
              #f
              result)))))


;;; Logging
;; SSH specific logging procedures

(define-method (log-error (obj <ssh-proxy>) (session <ssh:session>))
  (log-error obj (ssh:get-error session)))


;;; Interface implementation

;; Send message MESSAGE to a remote side with address ADDRESS.
;;
;; Return:
;;   <payload> ::= <output>
;;
(define-method (handle-send-message (obj     <ssh-proxy>)
                                    (address <string>)
                                    (message <string>))
  (let* ((connection-data (parse-address address))
         (username        (car connection-data))
         (hostname        (cadr connection-data))
         (port            (string->number (caddr connection-data)))
         (session (ssh:make-session)))

    (log-debug obj "Set SSH session options.")

    (if (not (ssh:session-set! session 'user username))
        (begin
          (log-error obj session)
          (proxy-error (ssh:get-error session) username)))

    (if (not (ssh:session-set! session 'host hostname))
        (begin
          (log-error obj session)
          (proxy-error (ssh:get-error session) hostname)))

    (if (not (ssh:session-set! session 'port port))
        (begin
          (log-error obj session)
          (proxy-error (ssh:get-error session) port)))
        
    (if (not (ssh:session-set! session 'log-verbosity
                               (if (proxy-debug? obj) 4 0)))
        (begin
          (log-error obj session)
          (proxy-error (ssh:get-error session))))

    (log-debug obj "Connect to a host.")

    (let ((res (ssh:connect! session))) 
      (if (eqv? res 'error)
          (begin
            (log-error obj session)
            (proxy-error (ssh:get-error session) res))))

    ;; TODO: Authenticate server
;    (ssh:authenticate-server session)

    (log-debug obj "Get private and public keys.")

    (let* ((pkey-file   (hash-ref (get-options obj) 'private-key))
           (private-key (ssh:private-key-from-file session pkey-file)))

      (if (not private-key)
          (begin
            (log-error obj session)
            (proxy-error (ssh:get-error session))))

      (let ((public-key (ssh:public-key->string
                         (ssh:private-key->public-key private-key))))

        (log-debug obj "Authenticate user with a public key.")

        (let ((res (ssh:userauth-pubkey! session #f public-key private-key)))
          (if (eqv? res 'error)
              (begin
                (log-error obj session)
                (proxy-error (ssh:get-error session) res))))

        (log-debug obj "Make a new SSH channel")
            
        (let ((channel (ssh:make-channel session)))

          (if (not channel)
              (begin
                (log-error obj session)
                (proxy-error (ssh:get-error session))))

          (if (not (ssh:channel-open-session channel))
              (begin
                (log-error obj session)
                (proxy-error (ssh:get-error session))))

          (ssh:channel-request-exec channel message)

          (poll-channel channel))))))


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
;; 
;; Return:
;;   <payload> ::= ( )
(define-method (handle-set-option! (obj <ssh-proxy>) (option <symbol>) value)
  (hash-set! (get-options obj) option value)
  '())


;; Check if remote side with address ADDRESS is accessible.
;;
;; Return:
;;   <payload> ::= ( #t )
(define-method (handle-ping (obj <ssh-proxy>) (address <string>))
  (handle-send-message obj address "uname")
  '(#t))

;;; ssh-proxy.scm ends here

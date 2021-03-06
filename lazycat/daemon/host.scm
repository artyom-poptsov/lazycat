;;; host.scm -- The host model for LazyCat.

;; Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
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

;; This module describes a <host> class that represents a single host
;; and it is used for communication with the host.
;;
;; This module exports:
;;   <host>
;;   host-get-id
;;   host-get-name
;;   host-get-proxy-list
;;   host-get-address
;;   host-get-description
;;   host-set-status!
;;   host-get-status

;;; Code:


;;; Module definition

(define-module (lazycat daemon host)
  #:use-module (oop goops)
  #:export (<host>
            host-get-id host-get-name host-get-proxy-list host-get-address
            host-get-description host-get-status host-set-status!
            host-get-attr host-set-attr! host-serialize/state))


;;; Main class
(define-class <host> ()
  (id
   #:setter set-id!
   #:getter get-id
   #:init-keyword #:id)

  (name
   #:setter set-name!
   #:getter get-name
   #:init-value #f
   #:init-keyword #:name)

  (proxy-list
   #:setter set-proxy-list!
   #:getter get-proxy-list
   #:init-keyword #:proxy-list
   #:init-value '())

  (address
   #:setter set-address!
   #:getter get-address
   #:init-value #f
   #:init-keyword #:address)

  (description
   #:setter set-description!
   #:getter get-description
   #:init-value #f
   #:init-keyword #:description)

  (status
   #:setter host-set-status!
   #:getter host-get-status
   #:init-value 'offline)

  (attr
   #:getter get-attr
   #:setter set-attr!
   #:init-value #f))

;; Host initialization
(define-method (initialize (obj <host>) args)
  (next-method)
  (set-attr! obj (make-hash-table)))


;;; Public methods

;; Rename the host to NEW-NAME
(define-method (host-set-name! (obj <host>) (new-name <string>))
  (set-name! obj new-name))

(define-method (host-get-name (obj <host>))
  (get-name obj))

;; Assign a new proxy NEW-PROXY for the host
(define-method (host-add-proxy! (obj <host>) (proxy-name <string>))
  ;; TODO: Add check for the proxy-name.
  (set-proxy-list! obj (cons proxy-name (get-proxy-list obj))))

(define-method (host-rem-proxy (obj <host>) (proxy-name <string>))
  #t)                                   ;TODO: implement this

(define-method (host-get-proxy-list (obj <host>))
  (get-proxy-list obj))

;; Assign a new address NEW-ADDRESS for the host
(define-method (host-set-address! (obj <host>) (new-address <string>))
  (set-address! obj new-address))

(define-method (host-get-address (obj <host>))
  (get-address obj))

;; Change the description NEW-DESCRIPTION of the host
(define-method (host-set-description! (obj <host>) (new-description <string>))
  (set-description! obj new-description))

(define-method (host-get-description (obj <host>))
  (get-description obj))

(define-method (host-get-id (obj <host>))
  (get-id obj))

(define-method (host-set-id! (obj <host>) (id <number>))
  (set-id! obj id))

(define-method (host-set-attr! (obj <host>) key val)
  (hash-set! (get-attr obj) key val))

(define-method (host-get-attr (obj <host>) key)
  (hash-ref (get-attr obj) key))

(define-method (host-serialize/state (obj <host>))
  (list
   (cons 'id          (host-get-id obj))
   (cons 'name        (get-name obj))
   (cons 'proxy-list  (get-proxy-list obj))
   (cons 'address     (host-get-address obj))
   (cons 'description (host-get-description obj))
   (cons 'state       (host-get-status obj))
   (cons 'attributes  (hash-map->list cons (get-attr obj)))))

;;; host.scm ends here

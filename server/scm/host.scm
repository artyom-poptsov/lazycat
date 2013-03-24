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
;;
;;   (host-send-message host message)
;;   (host-remove host)
;;
;;   (host-set-name host new-name)
;;   (host-set-proxy host new-proxy)
;;   (host-set-address host new-address)
;;   (host-set-description host new-description)
;;
;;   (host-get-id host)
;;   (host-get-name host)
;;   (host-get-proxy host)
;;   (host-get-address host)
;;   (host-get-description host)

;;; Code:


;;; Module definition

(define-module (lazycat server scm host)
  #:use-module (oop goops)
  #:use-module (lazycat server builtins)
  #:export (<host>
            host-get-id host-get-name host-get-proxy host-get-address
            host-get-description host-send-message host-remove))


;;; Main class
(define-class <host> ()
  (id          #:accessor id          #:init-value #f)
  (name        #:accessor name        #:init-value #f #:init-keyword #:name)
  (proxy       #:accessor proxy       #:init-value #f #:init-keyword #:proxy)
  (address     #:accessor address     #:init-value #f #:init-keyword #:address)
  (description #:accessor description #:init-value #f #:init-keyword #:description))

;; Host initialization
(define-method (initialize (obj <host>) args)
  (next-method)
  (slot-set! obj 'id (lc-add-host (proxy       obj)
                                  (address     obj)
                                  (name        obj)
                                  (description obj))))


;;;
;;; Public methods
;;;

;; Send a messsage MESSAGE to the host
(define-method (host-send-message (obj <host>) (message <string>))
  (lc-send-msg (id obj) message))

;; Close the host
(define-method (host-remove (obj <host>))
  (lc-rem-host (id obj)))


;;; Setters

;; Rename the host to NEW-NAME
(define-method (host-set-name (this <host>) (new-name <string>))
  (slot-set! this 'name new-name))

;; Assign a new proxy NEW-PROXY for the host
(define-method (host-set-proxy (this <host>) (new-proxy <string>))
  (slot-set! this 'proxy new-proxy))

;; Assign a new address NEW-ADDRESS for the host
(define-method (host-set-address (this <host>) (new-address <string>))
  (slot-set! this 'address new-address))

;; Change the description NEW-DESCRIPTION of the host
(define-method (host-set-description (this <host>) (new-description <string>))
  (slot-set! this 'description new-description))


;;; Getters

(define-method (host-get-id          (obj <host>)) (id          obj))
(define-method (host-get-name        (obj <host>)) (name        obj))
(define-method (host-get-proxy       (obj <host>)) (proxy       obj))
(define-method (host-get-address     (obj <host>)) (address     obj))
(define-method (host-get-description (obj <host>)) (description obj))

;;; host.scm ends here

;;;; LazyCat GTK "Add host" dialog.
;;;;
;;;; Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
;;;;
;;;; This file is part of LazyCat.
;;;;
;;;; LazyCat is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LazyCat is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LazyCat.  If not, see <http://www.gnu.org/licenses/>.


;;; Module definition

(define-module (lazycat lc-gtk-add-host-dialog)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (gnome gtk gdk-event)
  #:export (<lc-gtk-add-host-dialog> get-host-name get-proxy-name get-address
                                     get-host-description))


;;; Main class

(define-class <lc-gtk-add-host-dialog> (<gtk-dialog>)
  (cbox-proxy-name        #:accessor cbox-proxy-name)
  (entry-group            #:accessor entry-group)
  (entry-address          #:accessor entry-address)
  (entry-host-name        #:accessor entry-host-name)
  (entry-host-description #:accessor entry-host-description)
  
  (proxy-list #:accessor proxy-list #:init-value '() #:init-keyword #:proxy-list))


;;; Class initialization

(define-method (initialize (obj <lc-gtk-add-host-dialog>) args)

  (define (add-widgets-to-box box . widgets)
    (for-each (lambda (widget) (pack-start box widget #t #t 0)) widgets))

  (define (add-proxies-to-cbox cbox proxies)
    (for-each (lambda (proxy) (gtk-combo-box-append-text cbox proxy)) proxies))

  (next-method)

  (slot-set! obj 'title                  "Add host")
  (slot-set! obj 'cbox-proxy-name        (gtk-combo-box-new-text))
  (slot-set! obj 'entry-group            (make <gtk-entry>))
  (slot-set! obj 'entry-address          (make <gtk-entry>))
  (slot-set! obj 'entry-host-name        (make <gtk-entry>))
  (slot-set! obj 'entry-host-description (make <gtk-entry>))

  (let ((label-proxy-name       (make <gtk-label> #:label "Proxy"))
        (label-group-name       (make <gtk-label> #:label "Group"))
        (label-address          (make <gtk-label> #:label "Address"))
        (label-host-name        (make <gtk-label> #:label "Host name"))
        (label-host-description (make <gtk-label> #:label "Host description"))
        (button-add             (gtk-dialog-add-button obj "Add"    1))
        (button-cancel          (gtk-dialog-add-button obj "Cancel" 0)))

    (add-widgets-to-box (get-vbox obj)
                        label-proxy-name
                        (cbox-proxy-name obj)
                        label-group-name
                        (entry-group obj)
                        label-address
                        (entry-address obj)
                        label-host-name
                        (entry-host-name obj)
                        label-host-description
                        (entry-host-description obj))

    (add-proxies-to-cbox (cbox-proxy-name obj) (proxy-list obj))
    (gtk-combo-box-set-active (cbox-proxy-name obj) 0)

    ;; Set default values
    (gtk-entry-set-text (entry-address          obj) "127.0.0.1:50001")
    (gtk-entry-set-text (entry-host-name        obj) "localhost")
    (gtk-entry-set-text (entry-host-description obj) "Lazy server")

    ;; Connect handlers to signals
    (connect button-add    'clicked (lambda (w) (hide-all obj)))
    (connect button-cancel 'clicked (lambda (w) (hide-all obj)))))


;;;
;;; Public methods
;;;

;;; Getters

(define-method (get-group (obj <lc-gtk-add-host-dialog>))
  (gtk-entry-get-text (entry-group obj)))

(define-method (get-host-name (obj <lc-gtk-add-host-dialog>))
  (gtk-entry-get-text (entry-host-name obj)))

(define-method (get-proxy-name (obj <lc-gtk-add-host-dialog>))
  (gtk-combo-box-get-active-text (cbox-proxy-name obj)))

(define-method (get-address (obj <lc-gtk-add-host-dialog>))
  (gtk-entry-get-text (entry-address obj)))

(define-method (get-host-description (obj <lc-gtk-add-host-dialog>))
  (gtk-entry-get-text (entry-host-description obj)))

;;;; EOF

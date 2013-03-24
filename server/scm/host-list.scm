;;; host-list.scm -- The host list for LazyCat.

;; Copyright (C) 2012-2013 Artyom Poptsov <poptsov.artyom@gmail.com>
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

;; This module describes a <host-list> class which stores <host>
;; instances. The class provides some useful functions for working
;; with this list.
;;
;; These methods are exported:
;;
;;   (host-list-empty? obj)
;;   (host-list-load obj)
;;   (host-list-save obj)
;;
;;   (host-list-add-group obj group-name)
;;   (host-list-rem-group obj group-name)
;;
;;   (host-list-add-host obj group-name host-attributes)
;;   (host-list-rem-host obj host-id)
;;
;;   (host-list-get-host-by-id obj id)
;;   (host-list-get-group-name-by-host-id obj host-id)
;;   (host-list-get-plain-list obj)
;;   (host-list-get-unrolled-list obj)


;;; Code:

(load "config.scm")
(load "host.scm")

(define-module (lazycat server scm host-list)
  #:use-module (oop goops)
  #:use-module (ice-9 common-list)
  ;; LazyCat modules:
  #:use-module (lazycat server scm config)
  #:use-module (lazycat server scm host)
  #:export (<host-list> host-list-load
                        host-list-save
                        host-list-empty?
                        host-list-add-host
                        host-list-add-group
                        host-list-rem-host
                        host-list-rem-group
                        host-list-get-plain-list
                        host-list-get-unrolled-list
                        host-list-get-host-by-id
                        host-list-get-group-name-by-host-id))


;;; Constants

(define *default-lazycat-home* "~/.lazycat")
(define *default-file-name*    "hosts")
(define *default-list-name*    "hosts")

(define *no-group*  #f)


;;; Main class

(define-class <host-list> ()
  (lazycat-home     #:accessor     lazycat-home
                    #:init-value   *default-lazycat-home*
                    #:init-keyword #:lazycat-home)

  (config-file-name #:accessor     config-file-name
                    #:init-value   *default-file-name*
                    #:init-keyword #:config-file-name)

  (host-list        #:accessor host-list
                    #:init-value '())

  (config           #:accessor config
                    #:init-value #f))

;; Host list initialization
(define-method (initialize (obj <host-list>) args)
  (next-method)
  (let ((file (string-append (lazycat-home obj) "/" (config-file-name obj))))
    (slot-set! obj 'config (make <config> #:file file))))


;;;
;;; Public methods
;;;

(define-method (host-list-empty? (obj <host-list>))
  (null? (host-list obj)))

;; Load the host list
(define-method (host-list-load (obj <host-list>))

      (define (load-group group)
        (for-each (lambda (host-attributes)
                    (host-list-add-host obj (car group) host-attributes))
                  (cdr group)))

      (let ((list (config-load-list (config obj) *default-list-name*)))
        (for-each (lambda (h)
                    ;; Check for an empty group
                    ;;
                    ;; TODO: Empty groups shouldn't be missed.
                    (if (not (null? (cdr h)))
                        (if (list? (cadr h))

                            ;; Host is a member of a group
                            (load-group h)

                            ;; Host is not a member of a group
                            (host-list-add-host obj #f h))))
                  list)))

;; Make a list from host attributes
(define (unroll-host host)
  (list (host-get-name        host)
        (host-get-proxy       host)
        (host-get-address     host)
        (host-get-description host)))

;; FIXME: Temporary solution.
(define (unroll-host-with-id host)
  (list (host-get-id          host)
        (host-get-name        host)
        (host-get-proxy       host)
        (host-get-address     host)
        (host-get-description host)))

;; Get members of a group GROUP as a list.
(define (unroll-group group)
  (append (list (car group)) (map unroll-host (cdr group))))

;; FIXME: Temporary solution.
(define (unroll-group-with-id group)
  (append (list (car group)) (map unroll-host-with-id (cdr group))))

;; Save host list to a file
(define-method (host-list-save (obj <host-list>))
  (let ((unrolled-list (map unroll-group (host-list obj))))
    (config-save-list (config obj) *default-list-name* unrolled-list)))

;; This method is used for creating a new empty group in the list.
(define-method (host-list-add-group (obj <host-list>) group-name)
  (let* ((host-list (host-list obj))
         (group     (assoc group-name host-list)))
    (if (eq? group #f)
        (begin
          (set! host-list (append host-list (list group-name)))
          #t)
        #f)))

;;
;; This method is used for removing a group from the list.
;;
;; TODO: It'll be good idea to move members of the group to #f group before
;;       removing the group. There should be another function
;;       host-list-rem-group-with-members or something like this.
;;
(define-method (host-list-rem-group (obj <host-list>) group-name)
    (set! (host-list obj) (remove-if (lambda (element)
                                       (eq? (car element) group-name)) (host-list obj))))

;; Add a new host with given HOST-ATTRIBUTES to the host list, and
;; place it to a group GROUP-NAME. The group will be created if it
;; doesn't exist.
(define-method (host-list-add-host (obj <host-list>) group-name (host-attributes <list>))
  (let* ((name        (list-ref host-attributes 0))
         (proxy       (list-ref host-attributes 1))
         (address     (list-ref host-attributes 2))
         (description (list-ref host-attributes 3))
  
         (host (make <host>
                 #:name        name
                 #:proxy       proxy
                 #:address     address
                 #:description description))

         (group (assoc group-name (host-list obj))))

    (if (not (eq? group #f))
        (set-cdr! group (append (cdr group) (list host)))
        (set! (host-list obj) (append (host-list obj) (list (cons group-name (list host))))))

    (host-get-id host)))

;; Remove a host from the list
;;
;; TODO: Add check that the host exists.
(define-method (host-list-rem-host (obj <host-list>) (host-id <number>))
  (let ((host    (host-list-get-host-by-id obj host-id))
        (group   (get-group-by-host-id obj host-id)))

    ;; Remove the host from the list
    (set-cdr! group (remove-if (lambda (element) (eq? element host)) (cdr group)))

    ;; Remove the host
    (host-remove host)))

;; This function returns host by its ID.
(define-method (host-list-get-host-by-id (obj <host-list>) (id <number>))
  (let ((plain-list (host-list-get-plain-list obj)))
    (let f ((list plain-list))
      (if (eq? (host-get-id (car list)) id)
          (car list)
          (if (null? (cdr list))
              #f
              (f (cdr list)))))))

;; This function returns group name by host ID.
(define-method (host-list-get-group-name-by-host-id (obj <host-list>) (host-id <number>))
  (let ((group (get-group-by-host-id obj host-id)))
    (car group)))

;; This function returns group by host ID.
(define-method (get-group-by-host-id (obj <host-list>) (host-id <number>))

  (define (contains-host? host group)
    (let f ((group-members (cdr group)))
      (if (eq? host (car group-members))
          #t ; Host found.
          (if (null? (cdr group-members))
              #f ; Host is not found.
              (f (cdr group-members))))))

  (let ((host (host-list-get-host-by-id obj host-id)))
    (let f ((group-list (host-list obj)))
      (let ((group (car group-list)))
        (if (eq? (contains-host? host group) #t)
            group
            (if (null? (cdr group-list))
                #f
                (f (cdr group-list))))))))

;; This function returns list of hosts without groups
(define-method (host-list-get-plain-list (obj <host-list>))
  (let ((plain-list '()))
    (for-each (lambda (l) (set! plain-list (append plain-list l)))
              (map (lambda (l) (cdr l)) (host-list obj)))
    plain-list))

(define-method (host-list-get-unrolled-list (obj <host-list>))
  (map unroll-group-with-id (host-list obj)))
  
;;; host-list.scm ends here

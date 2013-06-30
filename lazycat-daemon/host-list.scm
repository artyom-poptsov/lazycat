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

(define-module (lazycat lazycat-daemon host-list)
  #:use-module (oop goops)
  #:use-module (ice-9 common-list)
  #:use-module (ice-9 optargs)
  ;; LazyCat modules:
  #:use-module (lazycat utils)
  #:use-module (lazycat lazycat-daemon config)
  #:use-module (lazycat lazycat-daemon host)
  #:export (<host-list> host-list-load
                        host-list-save
                        host-list-empty?
                        host-list-add-host
                        host-list-add-group
                        host-list-rem-host
                        host-list-rem-group
                        host-list-get-plain-list
                        host-list-get-serialized-list
                        host-list-get-host-by-id
                        host-list-get-group-name-by-host-id))


;;; Constants

(define *default-lazycat-home* "~/.lazycat")
(define *default-file-name*    "hosts")
(define *default-list-name*    "hosts")

(define *no-group*  #f)


;;; Main class

(define-class <host-list> ()
  (lazycat-home
   #:accessor     lazycat-home
   #:init-value   *default-lazycat-home*
   #:init-keyword #:lazycat-home)

  (config-file-name
   #:accessor     config-file-name
   #:init-value   *default-file-name*
   #:init-keyword #:config-file-name)

  (host-list
   #:accessor host-list
   #:init-value '())

  (last-host-id
   #:setter set-last-host-id!
   #:getter get-last-host-id
   #:init-value 0)

  (config
   #:accessor config
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
        (let ((group-name (car group)))
          (for-each
           (lambda (host-attr)
             (let ((id         (assoc-ref host-attr 'id))
                   (name       (assoc-ref host-attr 'name))
                   (proxy-list (assoc-ref host-attr 'proxy-list))
                   (addr       (assoc-ref host-attr 'address))
                   (desc       (assoc-ref host-attr 'description)))

               (add-host obj
                         #:group       group-name
                         #:id          id
                         #:name        name
                         #:proxy-list  proxy-list
                         #:address     addr
                         #:description desc)

               (if (< (get-last-host-id obj) id)
                   (set-last-host-id! obj id))))

           (cdr group))))

      (let ((list (config-load-list (config obj) *default-list-name*)))
        (for-each load-group list)))

;; Make a list from host HOST
(define (serialize-host host)
  (list (cons 'id          (host-get-id          host))
        (cons 'name        (host-get-name        host))
        (cons 'proxy-list  (host-get-proxy-list  host))
        (cons 'address     (host-get-address     host))
        (cons 'description (host-get-description host))))

;; Make an alist from the host HOST including the current internal
;; state of the host.
(define (serialize-host/state host)
  (list (cons 'id          (host-get-id          host))
        (cons 'name        (host-get-name        host))
        (cons 'proxy-list  (host-get-proxy-list  host))
        (cons 'address     (host-get-address     host))
        (cons 'description (host-get-description host))
        (cons 'status      (host-get-status      host))))

;; Get members of a group GROUP as a list.
(define (serialize-group group)
  (cons (car group) (map serialize-host (cdr group))))

(define (serialize-group/state group)
  (cons (car group) (map serialize-host/state (cdr group))))

;; Save host list to a file
(define-method (host-list-save (obj <host-list>))
  (let ((serialized-list (map serialize-group (host-list obj))))
    (config-save-list (config obj) *default-list-name* serialized-list)))

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

;; Add a new host with host id.
(define-method* (add-host (obj <host-list>)
                          (id          #f)
                          (group       #f)
                          (name        #f)
                          (proxy-list  #f)
                          (address     #f)
                          (description #f))
  (let ((host (make <host>
                #:id          id
                #:name        name
                #:proxy-list  proxy-list
                #:address     address
                #:description description))
        (group-list (assoc group (host-list obj))))

    (if (not (eq? group-list #f))
        (set-cdr! group-list (append (cdr group-list) (list host)))
        (set! (host-list obj) (append (host-list obj) (list (cons group (list host))))))

    id))

;; Add a new host to the host list, and place it to a group.  The
;; group will be created if it doesn't exist.
;;
;;   <syntax> ::= (host-list-add-host <host-list-instance>
;;                  #:<keyword> <value> ...)
;;   <keyword> ::= group | name | proxy | address | description
;;
(define-method* (host-list-add-host (obj <host-list>)
                                    (group       #f)
                                    (name        #f)
                                    (proxy-list  #f)
                                    (address     #f)
                                    (description #f))
  (set-last-host-id! obj (1+ (get-last-host-id obj)))
  (add-host obj
            #:group       group
            #:id          (get-last-host-id obj)
            #:name        name
            #:proxy-list  proxy-list
            #:address     address
            #:description description))

;; Remove a host from the list
;;
;; TODO: Add check that the host exists.
(define-method (host-list-rem-host (obj <host-list>) (host-id <number>))
  (let ((host    (host-list-get-host-by-id obj host-id))
        (group   (get-group-by-host-id obj host-id)))

    ;; Remove the host from the list
    (set-cdr! group (remove-if (lambda (element) (eq? element host)) (cdr group)))))

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

(define-method (host-list-get-serialized-list (obj <host-list>))
  (map serialize-group/state (host-list obj)))
  
;;; host-list.scm ends here

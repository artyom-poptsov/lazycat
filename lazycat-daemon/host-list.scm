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
;; These procedures are exported:
;;
;;   set-default-host-list!
;;
;;   host-list-empty?
;;   host-list-load
;;   host-list-save
;;
;;   host-list-add-group
;;   host-list-rem-group
;;
;;   host-list-add-host
;;   host-list-rem-host
;;
;;   host-list-get-host-by-id
;;   host-list-get-group-name-by-host-id
;;   host-list-get-plain-list
;;   host-list-get-unrolled-list


;;; Code:

(define-module (lazycat lazycat-daemon host-list)
  #:use-module (oop goops)
  #:use-module (ice-9 common-list)
  #:use-module (ice-9 optargs)
  #:use-module (scheme documentation)
  ;; LazyCat modules:
  #:use-module (lazycat utils)
  #:use-module (lazycat lazycat-daemon config)
  #:use-module (lazycat lazycat-daemon host)
  #:export (<host-list>
            set-default-host-list!
            host-list-load
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

;;;

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

(define-with-docs hlist
  "Default host list"
  #f)


;;; Public methods

(define (set-default-host-list! hl)
  "Set default host list which will be used for storing hosts."
  (set! hlist hl))

(define (host-list-empty?)
  (null? (host-list hlist)))

;; 
(define (host-list-load)
  "Load the host list"

  (define (load-group group)
    (let ((group-name (car group)))
      (for-each
       (lambda (host-attr)
         (let ((id         (assoc-ref host-attr 'id))
               (name       (assoc-ref host-attr 'name))
               (proxy-list (assoc-ref host-attr 'proxy-list))
               (addr       (assoc-ref host-attr 'address))
               (desc       (assoc-ref host-attr 'description)))

           (add-host #:group       group-name
                     #:id          id
                     #:name        name
                     #:proxy-list  proxy-list
                     #:address     addr
                     #:description desc)

           (if (< (get-last-host-id hlist) id)
               (set-last-host-id! hlist id))))

       (cdr group))))

  (let ((list (config-load-list (config hlist) *default-list-name*)))
    (for-each load-group list)))

(define (serialize-host host)
  "Make a list from host HOST"
  (list (cons 'id          (host-get-id          host))
        (cons 'name        (host-get-name        host))
        (cons 'proxy-list  (host-get-proxy-list  host))
        (cons 'address     (host-get-address     host))
        (cons 'description (host-get-description host))))

(define (serialize-host/state host)
  "Make an alist from the host HOST including the current internal
state of the host."
  (list (cons 'id          (host-get-id          host))
        (cons 'name        (host-get-name        host))
        (cons 'proxy-list  (host-get-proxy-list  host))
        (cons 'address     (host-get-address     host))
        (cons 'description (host-get-description host))
        (cons 'status      (host-get-status      host))))

(define (serialize-group group)
  "Get members of a group GROUP as a list."
  (cons (car group) (map serialize-host (cdr group))))

(define (serialize-group/state group)
  (cons (car group) (map serialize-host/state (cdr group))))

(define (host-list-save)
  "Save host list to a file"
  (let ((serialized-list (map serialize-group (host-list hlist))))
    (config-save-list (config hlist) *default-list-name* serialized-list)))

(define (host-list-add-group group-name)
  "Create a new empty group GROUP-NAME in the list."
  (let* ((host-list (host-list hlist))
         (group     (assoc group-name host-list)))
    (if (eq? group #f)
        (begin
          (set! host-list (append host-list (list group-name)))
          #t)
        #f)))

;; TODO: It'll be good idea to move members of the group to #f group before
;;       removing the group. There should be another function
;;       host-list-rem-group-with-members or something like this.
;;
(define (host-list-rem-group group-name)
  "Remove a group GROUP-NAME from the list."
  (set! (host-list hlist) (remove-if (lambda (element)
                                     (eq? (car element) group-name)) (host-list hlist))))

(define* (add-host #:key (id          #f)
                         (group       #f)
                         (name        #f)
                         (proxy-list  #f)
                         (address     #f)
                         (description #f))
  "Add a new host with host id."
  (let ((host (make <host>
                #:id          id
                #:name        name
                #:proxy-list  proxy-list
                #:address     address
                #:description description))
        (group-list (assoc group (host-list hlist))))

    (if (not (eq? group-list #f))
        (set-cdr! group-list (append (cdr group-list) (list host)))
        (set! (host-list hlist) (append (host-list hlist)
                                        (list (cons group (list host))))))

    id))

;; Add a new host to the host list, and place it to a group.  The
;; group will be created if it doesn't exist.
;;
;;   <syntax> ::= (host-list-add-host
;;                  #:<keyword> <value> ...)
;;   <keyword> ::= group | name | proxy | address | description
;;
(define* (host-list-add-host #:key (group       #f)
                                   (name        #f)
                                   (proxy-list  #f)
                                   (address     #f)
                                   (description #f))
  "Add a new host to the host list, and place it to a group.  The
group will be created if it doesn't exist."
  (set-last-host-id! hlist (1+ (get-last-host-id hlist)))
  (add-host hlist
            #:group       group
            #:id          (get-last-host-id hlist)
            #:name        name
            #:proxy-list  proxy-list
            #:address     address
            #:description description))

(define (host-list-rem-host id)
  "Remove a host with given ID from the list"
  (let ((host (host-list-get-host-by-id id)))

    (or host
        (throw 'no-such-host id))

    (let ((group (get-group-by-host-id id)))
      ;; Remove the host from the list
      (set-cdr! group (remove-if
                       (lambda (element) (eq? element host))
                       (cdr group))))))

(define (host-list-get-host-by-id id)
  "Get host by ID"
  (let ((plain-list (host-list-get-plain-list)))
    (let f ((list plain-list))
      (if (eq? (host-get-id (car list)) id)
          (car list)
          (if (null? (cdr list))
              #f
              (f (cdr list)))))))

(define (host-list-get-group-name-by-host-id id)
  "Get group by the host ID"
  (let ((group (get-group-by-host-id id)))
    (car group)))

(define (get-group-by-host-id id)
  "Get a group by host ID"

  (define (contains-host? host group)
    (let f ((group-members (cdr group)))
      (if (eq? host (car group-members))
          #t ; Host found.
          (if (null? (cdr group-members))
              #f ; Host is not found.
              (f (cdr group-members))))))

  (let ((host (host-list-get-host-by-id id)))
    (let f ((group-list (host-list hlist)))
      (let ((group (car group-list)))
        (if (eq? (contains-host? host group) #t)
            group
            (if (null? (cdr group-list))
                #f
                (f (cdr group-list))))))))

(define (host-list-get-plain-list)
  "Get list of hosts without groups."
  (let ((plain-list '()))
    (for-each (lambda (l) (set! plain-list (append plain-list l)))
              (map (lambda (l) (cdr l)) (host-list hlist)))
    plain-list))

(define (host-list-get-serialized-list)
  (map serialize-group/state (host-list hlist)))
  
;;; host-list.scm ends here

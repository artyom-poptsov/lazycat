;;; proxy-list.scm -- List of proxies.

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

;; List of available proxies.
;;
;; This module exports:
;;   <proxy-list>
;;   proxy-list-get
;;   proxy-list-get-list
;;   proxy-list-load
;;   proxy-list-stop-all

;;; Code:

(define-module (lazycat lazycat-daemon proxy-list)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (oop goops)
  #:use-module (lazycat proxy)
  ;; Proxies
  #:use-module (lazycat proxies ssh-proxy)
  #:use-module (lazycat proxies tcp-proxy)
  #:export (<proxy-list> proxy-list-get
                         proxy-list-get-list
                         proxy-list-load
                         proxy-list-stop-all))

(define-class <proxy-list> ()
  (tmp-dir
   #:init-keyword #:tmp-dir
   #:getter get-tmp-dir)

  (htable
   #:init-value (make-hash-table)
   #:getter get-htable)

  (debug-mode?
   #:init-keyword #:debug-mode
   #:getter       debug-mode?
   #:init-value #f))

;; Load all proxies
(define-method (proxy-list-load (obj <proxy-list>))
  (let ((ssh-proxy (make <ssh-proxy> #:socket-dir (get-tmp-dir obj)))
        (tcp-proxy (make <tcp-proxy> #:socket-dir (get-tmp-dir obj))))

    (if (debug-mode? obj)
        (begin
          (proxy-debug! ssh-proxy #t)
          (proxy-debug! tcp-proxy #t)))

    (proxy-start ssh-proxy)
    (proxy-start tcp-proxy)
    (hash-set! (get-htable obj) (proxy-get-name ssh-proxy) ssh-proxy)
    (hash-set! (get-htable obj) (proxy-get-name tcp-proxy) tcp-proxy)))

;; Get a proxy with name NAME.
(define-method (proxy-list-get (obj <proxy-list>) (name <string>))
  (let ((ht (get-htable obj)))
    (hash-ref ht name #f)))

;; Get list of proxies
(define-method (proxy-list-get-list (obj <proxy-list>))
  (hash-map->list cons (get-htable obj)))

;; Stop all proxy processes
(define-method (proxy-list-stop-all (obj <proxy-list>))
  (for-each (lambda (p) (proxy-stop (cdr p)))
            (proxy-list-get-list obj)))

;;; proxy-list.scm ends here










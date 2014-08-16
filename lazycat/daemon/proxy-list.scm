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
;;   proxy-list-get-proxy
;;   proxy-list-get-list
;;   proxy-list-load
;;   proxy-list-stop-all

;;; Code:

(define-module (lazycat daemon proxy-list)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (oop goops)
  #:use-module (scheme documentation)
  #:use-module (lazycat proxy)
  ;; Proxies
  #:use-module (lazycat proxy ssh-proxy)
  #:use-module (lazycat proxy tcp-proxy)
  #:export (<proxy-list>
            set-default-proxy-list!
            proxy-list-get-proxy
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

(define-with-docs plist
  "Default proxy list"
  #f)

(define (set-default-proxy-list! pl)
  "Set default proxy list which will be used for storing of all the
proxies."
  (set! plist pl))

(define (proxy-list-load)
  "Load all proxies"
  (let ((ssh-proxy (make <ssh-proxy> #:socket-dir (get-tmp-dir plist)))
        (tcp-proxy (make <tcp-proxy> #:socket-dir (get-tmp-dir plist))))

    (if (debug-mode? plist)
        (begin
          (proxy-debug! ssh-proxy #t)
          (proxy-debug! tcp-proxy #t)))

    (proxy-start ssh-proxy)
    (proxy-start tcp-proxy)
    (hash-set! (get-htable plist) (proxy-get-name ssh-proxy) ssh-proxy)
    (hash-set! (get-htable plist) (proxy-get-name tcp-proxy) tcp-proxy)))

;; 
(define (proxy-list-get-proxy name)
  "Get a proxy with name NAME."
  (let ((ht (get-htable plist)))
    (hash-ref ht name #f)))

(define (proxy-list-get-list)
  "Get list of proxies"
  (hash-map->list cons (get-htable plist)))

(define (proxy-list-stop-all)
  "Stop all proxy processes"
  (for-each (lambda (p) (proxy-stop (cdr p)))
            (proxy-list-get-list)))

;;; proxy-list.scm ends here










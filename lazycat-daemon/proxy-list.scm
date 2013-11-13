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
  #:export (proxy-list-set-sock-dir!
            proxy-list-get
            proxy-list-get-list
            proxy-list-load
            proxy-list-stop-all))


;;; Global variables

(define htable      (make-hash-table))
(define debug-mode? #f)
(define sock-dir    #f)


;;; Procedures

(define (proxy-list-set-sock-dir! dir)
  (set! tmp-dir dir))

(define (proxy-list-debug! s)
  (set! debug-mode? s))

(define (proxy-list-load!)
  "Load all proxies"
  (let ((ssh-proxy (make <ssh-proxy> #:socket-dir sock-dir))
        (tcp-proxy (make <tcp-proxy> #:socket-dir sock-dir)))

    (if debug-mode?
        (begin
          (proxy-debug! ssh-proxy #t)
          (proxy-debug! tcp-proxy #t)))

    (proxy-start ssh-proxy)
    (proxy-start tcp-proxy)
    (hash-set! htable (proxy-get-name ssh-proxy) ssh-proxy)
    (hash-set! htable (proxy-get-name tcp-proxy) tcp-proxy)))

(define (proxy-list-get name)
  "Get a proxy with name NAME."
  (hash-ref htable name #f))

(define (proxy-list-get-list)
  "Get list of proxies"
  (hash-map->list cons htable))

(define (proxy-list-stop-all)
  "Stop all proxy processes"
  (for-each (lambda (p) (proxy-stop (cdr p)))
            (proxy-list-get-list)))

;;; proxy-list.scm ends here










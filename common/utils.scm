;;; utils.scm -- Useful procedures and macros.

;; Copyright (C) 2013 Artyom Poptsov <poptsov.artyom@gmail.com>
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

;; Useful procedures and macros.
;;
;; This module exports:
;;
;;   (define-method* (obj <class>) . args)


;;; Code:

(define-module (lazycat utils)
  #:use-module (oop goops)
  #:use-module (ice-9 syncase)
  #:use-module (ice-9 optargs)
  #:export-syntax (define-method* case*))

;; define*-like macro that allows to use keywords with GOOPS'
;; define-method.
;;
;;   <syntax> ::= (define-method* (<method-name> (obj <class>) <args>)
;;                  <body>)
;;   <args>   ::= (<variable> <default-value>) ...
;;
(define-syntax define-method*
  (syntax-rules ()
    ((_ (m (o <class>) (var defval) ...) body ...)
     (define-method (m (o <class>) . args)
       (let-keywords args #t ((var defval) ...)
                     body ...)))))

;; Extended form of `case' that compares the key K with objects using
;; the given predicate P.
(define-syntax case*
  (syntax-rules (else)
    ;; Sanity check for the syntax
    ((_)
     (syntax-error "Missing predicate and clauses"))
    ((_ p)
     (syntax-error "Missing clauses"))
    ;; Syntax rules
    ((_ p k ((obj) expr) . clauses)
     (case* p k clauses ((p k obj) expr)))
    ((_ p k (((obj) expr) . clauses) . forms)
     (case* p k clauses ((p k obj) expr) . forms))
    ((_ p k ((obj) expr))
     (case* p k () ((p k obj) expr) . ignore))
    ((_ p k () (c e) ...)
     (cond (c e) ...))
    ((_ p0 k ((else expr ...)) ((p1 a b) e) ...)
     (cond ((p1 a b) e) ... (else expr ...)))
    ((_ p k (else expr))
     (cond (else expr)))))
         
;;; utils.scm ends here

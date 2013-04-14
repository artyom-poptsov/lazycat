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

(define-module (lazycat server utils)
  #:use-module (oop goops)
  #:use-module (ice-9 syncase)
  #:use-module (ice-9 optargs)
  #:export-syntax (define-method*))

;; define*-like macro that allows to use keywords with GOOPS'
;; define-method.
;;
;;   <syntax> ::= (define-method* (<method-name> (obj <class>) <args>)
;;                  <body>)
;;   <args>   ::= (<variable> <default-value>) ...
;;
;; FIXME: Doesn't work correctly if a defined method called with the empty
;;        args list.
;;
(define-syntax define-method*
  (syntax-rules ()
    ((_ (m (o <class>) (var defval) ...) body ...)
     (define-method (m (o <class>) . args)
       (let-keywords args #t ((var defval) ...)
                     body ...)))))

;;; utils.scm ends here

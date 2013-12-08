;;; translator-list.scm -- List of translators.

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

;; List of available translators.


;;; Code:

(define-module (lazycat lazycat-daemon translator-list)
  #:use-module (oop goops)
  #:use-module (scheme documentation)
  #:use-module (lazycat translator)
  #:use-module (lazycat trans portage)
  #:use-module (lazycat trans apt)
  #:export (get-translator-for-host))

(define-with-docs tlist
  "List of translators."
  `(,(make <portage-translator>)
    ,(make <apt-translator>)))

(define (get-translator-for-host host)
  "Search applicable translator for HOST.  Return a translator if it
is found, #f otherwise."
  (let check ((lst tlist))
    (if (not (null? lst))
        (let ((trans (car lst)))
          (if (understood? trans host)
              trans
              (check (cdr lst))))
        #f)))

;;; translator-list.scm ends here



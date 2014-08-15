;;; translator.scm -- Command translator.

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



;;; Code:

(define-module (lazycat translator)
  #:use-module (oop goops)
  #:use-module (lazycat daemon host)
  #:use-module (scheme documentation)
  #:export (<translator> translate understood?))


(define-class-with-docs <translator> ()
  "The class knows the 'native language' of a specific package
manager.")


(define-generic-with-docs translate
  "Translate a meta-command to the command understood by a specific
package manager.")
(define-method (translate (self <translator>) (command <list>))
  (throw 'implementation-missing "translate"))


(define-generic-with-docs understood?
  "Check whether the given host understand commands produced by the
translator or not.")
(define-method (understood? (self <translator>) (host <host>))
  (throw 'implementation-missing "understood?"))

;;; translator.scm ends here

;;; main.scm -- Entry point of the program.

;; Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
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

;; Entry point of the program.  It is called from the C code and
;; starts the LazyCat daemon.
;;
;; This module exports:
;;
;;   (main)

;;; Code:

(define-module (lazycat server main)
  #:use-module (oop goops)
  #:use-module (lazycat server lazycatd)
  #:export (main))

(define (main args)
  (let ((lazycatd (make <lazycatd>)))
    (run lazycatd)))

;;; main.scm ends here.

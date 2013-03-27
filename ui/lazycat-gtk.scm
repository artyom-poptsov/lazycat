#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
export GUILE_LOAD_PATH=${GUILE_LOAD_PATH}:__UI_GTK_DATA_DIR__:__SERVER_DATA_DIR__
main='(module-ref (resolve-module '\''(lazycat ui lazy-gtk-cat)) '\'main')'
exec ${GUILE-guile} -l $0 -c "(apply $main (command-line))" "$@"
!#

;;; lazycat-gtk.scm -- Run the Lazy GTK Cat application.

;; The script uses lazycat as a shell to eval the Scheme code.
;;
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

;; Run the LazyCat server with GUI based on GTK.
;;
;; Usage:
;;
;;   ./lazycat-gtk.sh


;;; Code:

(load-from-path "lazy-gtk-cat.scm")

(define-module (lazycat ui lazy-gtk-cat)
  #:use-module (oop goops)
  #:use-module (lazycat ui gtk lazy-gtk-cat)
  #:export (main))

(define (main args)
  (let ((lazy-gtk-cat (make <lazy-gtk-cat>)))
    (run lazy-gtk-cat args)))

;;; lazycat-gtk.scm ends here

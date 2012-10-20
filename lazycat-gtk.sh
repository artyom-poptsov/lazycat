#!./lazycat \ ;; -*- scheme -*-
-e main --debug -s
!# 

;;;; This script is used to run Lazy GTK Cat application.
;;;;
;;;; The script uses lazycat as a shell to eval the Scheme code.
;;;;
;;;; Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
;;;;
;;;; This file is part of LazyCat.
;;;;
;;;; LazyCat is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LazyCat is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LazyCat.  If not, see <http://www.gnu.org/licenses/>.

(load "scheme/lazy-gtk-cat.scm")

(use-modules
 (oop goops)
 (lazycat lazy-gtk-cat))

(define (main args)
  (let ((lazy-gtk-cat (make <lazy-gtk-cat>)))
    (run lazy-gtk-cat args)))

;;;; EOF

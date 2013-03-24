;;; protocol.scm -- Description of the protocol.

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

;; Description of the protocol that is used in communication between
;; the server and a UI (i.e. lc and lazy-gtk-cat);


;;; Code:

(define-module (lazycat server scm protocol))

(define-public *protocol-version* 1)

(define-public *cmd-get-protocol-version* 0)
(define-public *cmd-add-host*             1)
(define-public *cmd-rem-host*             2)
(define-public *cmd-upd-host*             3)
(define-public *cmd-list*                 4)
(define-public *cmd-exec*                 5)
(define-public *cmd-diff*                 6)
(define-public *cmd-get*                  7)
(define-public *cmd-set*                  8)
(define-public *cmd-stop*                 10)

;;; protocol.scm ends here.

;;; portage.scm -- Translator for Portage package manager.

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

;; Translator for Portage package manager used in Gentoo GNU/Linux.


;;; Code:

(define-module (lazycat trans portage)
  #:use-module (oop goops)
  #:use-module (lazycat utils)
  #:use-module (lazycat translator)
  #:use-module (lazycat lazycat-daemon host)
  #:use-module (scheme documentation)
  #:re-export  (translate understood?)
  #:export     (<portage-translator>))


(define-class-with-docs <portage-translator> (<translator>)
  "Translator for Portage package management system used in Gentoo
GNU/Linux")


(define-method (translate (self <portage-translator>) (command <list>))
  (let ((cmd      (car command))
        (args-str (string-join (cdr command) " ")))
    (case* string=? cmd
     (("emerge")
      (string-append "emerge " args-str))
     (("unmerge")
      (string-append "emerge -q --unmerge " args-str))
     (("search")
      (string-append "emerge -q --search "  args-str)))))


(define-method (understood? (self <portage-translator>) (host <host>))
  (let ((dist-id (host-get-attr host "lsb/distributor-id")))
    (if dist-id
        (string=? dist-id "Gentoo")
        #f)))

;;; portage.scm ends here

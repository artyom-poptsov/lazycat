;;; apt.scm -- Translator for Aptitude package manager.

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

;; Translator for Aptitude package manager used in Debian based
;; distributions such as Ubuntu GNU/Linux.


;;; Code:

(define-module (lazycat trans apt)
  #:use-module (oop goops)
  #:use-module (scheme documentation)
  #:use-module (lazycat utils)
  #:use-module (lazycat translator)
  #:use-module (lazycat lazycat-daemon host)
  #:re-export  (translate understood?)
  #:export     (<apt-translator>))


(define-class-with-docs <apt-translator> (<translator>)
  "Translator for Aptitude package management system used in
Debian based distributions such as Ubuntu GNU/Linux.")


(define-method (translate (self <apt-translator>) (command <list>))
  (let ((cmd      (car command))
        (args-str (string-join command " ")))
    (case* string=? cmd
     (("emerge")
      (string-append "apt-get install " args-str))
     (("unmerge")
      (string-append "apt-get remove " args-str))
     (("search")
      (string-append "apt-cache search " args-str)))))


(define-method (understood? (self <apt-translator>) (host <host>))
  (let ((dist-id (host-get-attr host "lsb/distributor-id")))
    (if dist-id
        (or (string=? dist-id "Debian")
            (string=? dist-id "Ubuntu"))
        #f)))

;;; apt.scm ends here

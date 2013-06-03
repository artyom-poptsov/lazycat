;;; main.scm -- Entry point of the program.

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

;; Entry point of the program.  It is called from the C code and
;; starts the LazyCat daemon.
;;
;; This module exports:
;;
;;   (main)

;;; Code:

(define-module (lazycat lazycat-daemon main)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:use-module (lazycat lazycat-daemon lazycatd)
  #:export (main))

;; Command line options specification
(define *option-spec*
  '((debug     (single-char #\d) (value #f))
    (no-detach (single-char #\D) (value #f))
    (help      (single-char #\h) (value #f))))

;; Print the help message
(define (print-help)
  (display
   (string-append
    "Usage: lazycat-daemon [ -dDh ]\n"
    "\n"
    "Options:\n"
    "\t" "-d, --debug        Debug mode.\n"
    "\t" "-D, --no-detach    Don't detach from a terminal and don't become a\n"
    "\t" "                   daemon.  Useful for debugging.\n"
    "\t" "-h, --help         Print this message and exit.\n")))


;; Entry point of the program
(define (main args)
  (let* ((options       (getopt-long args *option-spec*))
         (debug-needed? (option-ref options 'debug     #f))
         (no-detach?    (option-ref options 'no-detach #f))
         (help-needed?  (option-ref options 'help      #f)))

    (if help-needed?
        (begin
          (print-help)
          (quit)))

    (let ((lazycatd (make <lazycatd>
                      #:debug-mode      debug-needed?
                      #:no-detach-mode  no-detach?)))
      (run lazycatd))))

;;; main.scm ends here.

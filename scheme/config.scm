;;;; Functions for working with configuration files for LazyCat.
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

(define-module (lazycat config)
  :export (lazycat-home load-config save-config))

(define lazycat-home (string-append (getenv "HOME") "/.lazycat"))

(define (load-config file-name)
  "This function is used for loading a config file."
  (let ((config (string-append lazycat-home "/" file-name)))
    (if (not (file-exists? lazycat-home))
        (system (string-append "mkdir " lazycat-home)))
    (if (file-exists? config)
        (load config))))

(define (save-config file-name list-name list)
  "This function is used for saving a config file."
  (let ((config (string-append lazycat-home "/" file-name)))

    (if (and (string? list-name) (list? list))
        (begin
          (if (not (file-exists? lazycat-home))
              (system (string-append "mkdir " lazycat-home)))

          ;; Create file if it doesn't exist
          (if (not (file-exists? config))
                (system (string-append "touch " config)))

          ;; Store a given list in the file
          (let ((p (open-output-file config))
                (l (cons 'define
                         (cons (string->symbol list-name)
                               (cons list '())))))

            ;; Tell to Emacs that this file contains scheme code
            (display ";; -*- mode: scheme -*-" p)
            (newline p)
            (newline p)

            ;; Write the list to the config file
            (display (string-append "(set! " list-name) p)
            (newline p)

            (display "  '(" p)

            (for-each (lambda (l)
                        (begin
                          (if (not (eq? l (car (reverse list))))

                              ;; l is not the last element of the list
                              (begin
                                (write l p)
                                (newline p)
                                (display "    " p))
                              
                              ;; l is the last element of the list
                              (begin
                                (write l p)
                                (display "))" p)))))
                      list)

            (close-output-port p))))))


;;;; EOF

;;; config.scm -- Stores configuration in a file.

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

;; This module describes a <config> class that is capable of storing a
;; Scheme list in a file.  Currently it supports only one list per
;; file, but this might be changed in the future.
;;
;; These methods are exported:
;;
;;   (config-load-list config list-name)
;;   (config-save-list config list-name list)
;;
;; Example of usage:
;;
;;   ((let* ((config (make <config> #:file file-name))
;;           (list   (config-load-list config list-name)))
;;
;;     ...
;;
;;     (config-save-list config list-name list)


;;; Code:

(define-module (lazycat server config)
  #:use-module (oop goops)
  #:export     (<config> config-load-list config-save-list))


;;; Main class

(define-class <config> ()
  (file #:accessor file #:init-value #f #:init-keyword #:file))

;; Class initialization
(define-method (initialize (obj <config>) args)
  (next-method))


;;; Public methods

;; Load the list LIST-NAME from the config file. Method returns an
;; empty list if file doesn't exist.
(define-method (config-load-list (obj <config>) (list-name <string>))
  (if (file-exists? (file obj))
      (begin
        (load (file obj))
        (let ((list (eval-string list-name)))
          (if (list? list) list #f)))
      '()))

;; Save the list LIST-NAME to the config file.
(define-method (config-save-list (obj <config>) (list-name <string>) (list <list>))

  (define *basic-indent* 4)

  (define (make-indent value)
    "Make indentation"
    (if (> value 0)
        (begin
          (display " ")
          (make-indent (- value 1)))))

  (define (save-list-with-indent indent list)
    (for-each (lambda (l)
                (begin
                  ;; Check for an inherited list
                  (if (and (not (null? (cdr l))) ; Check for an empty list
                           (list? (cadr l)))
                      (begin
                        (display "(")
                        (write (car l))
                        (newline)

                        (make-indent (+ indent 1))

                        (save-list-with-indent (+ indent 1) (cdr l))
                        (display ")"))

                      (write l))

                  ;; Print the new line if this element is not the last
                  ;; element in the list.
                  (if (not (eq? l (car (reverse list))))
                      (begin
                        (newline)
                        (make-indent indent)))))
              list))

  (with-output-to-file (file obj)
    (lambda ()
      (display ";; -*- mode: scheme -*-\n\n")
      (display (string-append "(define " list-name "\n"))
      (display "  '(")
      (save-list-with-indent *basic-indent* list)
      (display "))\n"))))

;;; config.scm ends here

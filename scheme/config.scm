;;;; A configuration file for LazyCat.
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


;;; Module definition

(define-module (lazycat config)
  #:use-module (oop goops)
  #:export     (<config> config-load-list config-save-list))


;;; Main class

(define-class <config> ()
  (file #:accessor file #:init-value #f #:init-keyword #:file))

;; Class initialization
(define-method (initialize (obj <config>) args)
  (next-method))


;;;
;;; Public methods
;;;

;; Load the list LIST-NAME from the config file.
(define-method (config-load-list (obj <config>) (list-name <string>))
  (begin
    (load (file obj))
    (let ((list (eval-string list-name)))
      (if (list? list) list #f))))

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
      ;; Tell to Emacs that this file contains scheme code
      (display ";; -*- mode: scheme -*-\n\n")
      ;; Write the list to the config file
      (display (string-append "(define " list-name "\n"))
      (display "  '(")

      (save-list-with-indent *basic-indent* list)

      (display "))\n"))))

;;;; EOF

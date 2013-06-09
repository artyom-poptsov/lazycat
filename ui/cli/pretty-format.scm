;;; pretty-format.scm -- Format various kind of information.

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



;;; Code:

(define-module (lazycat ui cli pretty-format)
  #:use-module (ice-9 format)
  #:export (format-host-list
            format-options-list
            format-error
            format-output
            format-output-list
            format-diff))


;; Display the host list LIST as a table
(define (format-host-list list)

  (define *header-fmt* "~15a ~5@a ~15a ~10a ~20a ~a\n")
  (define *table-fmt*  "~15a ~5d ~15a ~10a ~20a ~a\n")

  ;; Print header
  (format #t *header-fmt* "Group" "ID" "Name" "Proxy" "Address" "Description")
  (format #t *header-fmt* "-----" "--" "----" "-----" "-------" "-----------")

  (for-each

   (lambda (group)
     (let ((group-name (car group)))
       (for-each
        (lambda (host)
          (format #t *table-fmt*
                  (if (not (eq? group-name #f))
                      group-name
                      " ")
                  (list-ref host 0)
                  (list-ref host 1)
                  (list-ref host 2)
                  (list-ref host 3)
                  (list-ref host 4)))
        (cdr group))))

   list)

  (newline))


;; Display an error message
(define (format-error error)
  (define *fmt* "~a: ~a\n")
  (format #t *fmt* (car error) (cdr error)))


;; Display options list LIST as a table.
(define (format-options-list list)

  (define *fmt* "~15a ~15a\n")

  (format #t *fmt* "Option" "Value")
  (format #t *fmt* "------" "-----")

  (for-each

   (lambda (option)
     (let ((option-name  (car  option))
           (option-value (cdr option)))
       (format #t *fmt* option-name option-value)))

   list)

  (newline))

;; Format output from a host.
(define (format-output output)

  (define *header-fmt* "<<< ~5d ~5a\n")
  (define *output-fmt* "~a\n")

  (let ((host-id  (car output))
        (status   (car (cadr output)))
        (response (cadr (cadr output))))

    (if status

        (begin
          (format #t *header-fmt* host-id "OK")
          (format #t *output-fmt* response))

        (begin
          (format #t *header-fmt* host-id "ERROR")
          (format-error response)))))

(define (format-output-list list)
  (let ((status (car list)))
    (if status
        (for-each format-output (cadr list))
        (format #t "ERROR: ~a\n" (cadr list)))))

(define (format-diff diff)

  (define *header-fmt* "<<< ~5d ~5a\n")
  (define *output-fmt* "~a\n")

  (let ((status (car diff)))
    (if status
        (for-each
         (lambda (diff-result)
           (let ((host-id (car diff-result))
                 (result  (cadr diff-result)))

             (format #t *header-fmt* host-id result)

             (if (or (eq? result 'different) (eq? result 'error))
                 (format #t *output-fmt* (caddr diff-result)))))
         (cadr diff))

        (format #t "ERROR: ~a\n" (cadr diff)))))

;;; pretty-format.scm ends here.

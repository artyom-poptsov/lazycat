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
  #:export (format-host-list format-options-list))


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

;;; pretty-format.scm ends here.

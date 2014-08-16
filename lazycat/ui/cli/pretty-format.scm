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
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (lazycat ui cli format-table)
  #:use-module (lazycat message)
  #:export (format-host
            format-host-list
            format-options-list
            format-error
            format-error-message
            format-output
            format-output-list
            format-diff))

(define *header-fmt* "~15a  ~5a  ~8a  ~15a  ~15a  ~20a  ~80a\n")
(define *table-fmt*  "~15a  ~5a  ~8a  ~15a  ~15a  ~20a  ~80a\n")


;; Display the host list LIST as a table
(define (format-host-list lst)

  (define (proxy-list->string lst)
    (fold
     (lambda (proxy res)
       (let ((p (car (string-split proxy #\-))))
         (string-append p " " res)))
     ""
     lst))

  ;; Print the header
  (format #t *header-fmt* "Group" "ID" "Status" "Name" "Proxy" "Address" "Description")
  (format #t *header-fmt* "-----" "--" "------" "----" "-----" "-------" "-----------")

  (for-each

   (lambda (group)
     (let ((group-name (car group)))
       (for-each
        (lambda (host)
          (let ((group (if (not (eq? group-name #f))
                           group-name
                           ""))
                (id          (number->string (assoc-ref host 'id)))
                (status      (symbol->string (assoc-ref host 'status)))
                (name        (assoc-ref host 'name))
                (proxy-list  (proxy-list->string (assoc-ref host 'proxy-list)))
                (address     (assoc-ref host 'address))
                (description (assoc-ref host 'description)))

            (format-table #t *table-fmt* (list (list group
                                                     id
                                                     status
                                                     name
                                                     proxy-list
                                                     address
                                                     description)))))
        (cdr group))))
   lst)

  (newline))

(define (format-host host)
  (for-each
   (lambda (field)
     (let ((field-name (car field))
           (field-val  (cdr field)))
       (if (list? field-val)
           (begin
             (format #t "~15a:~%" field-name)
             (for-each
              (lambda (v)
                (if (pair? v)
                    (format #t "  ~20a: ~a~%" (car v) (cdr v))
                    (format #t "  ~20a~%" v)))
                
              field-val))
           (format #t "~15a: ~a~%" field-name field-val))))
   host))


;; Display an error message
(define (format-error error)
  (format #t "ERROR: ~a\n" error))


;; Display content of an error message
(define-method (format-error-message (msg <message>))
  (format-error (message-field-ref msg 'error)))


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
  (define *error-fmt*  "~a: ~a\n")  

  (let ((host-id  (car output))
        (status   (car (cadr output)))
        (response (cadr (cadr output))))

    (if status

        (begin
          (format #t *header-fmt* host-id "OK")
          (format #t *output-fmt* response))

        (begin
          (format #t *header-fmt* host-id "ERROR")
          (format #t *error-fmt* (car response) (cdr response))))))

(define (format-output-list list)
  (for-each format-output list))

(define (format-diff diff)

  (define *header-fmt* "<<< ~5d ~5a\n")
  (define *output-fmt* "~a\n")

  (for-each
   (lambda (diff-result)
     (let ((host-id (car diff-result))
           (result  (cadr diff-result)))

       (format #t *header-fmt* host-id result)

       (if (or (eq? result 'different) (eq? result 'error))
           (format #t *output-fmt* (caddr diff-result)))))
   diff))

;;; pretty-format.scm ends here.

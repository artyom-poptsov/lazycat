;;; LazyCat GTK output view.

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

;; This module describes a <output-view> class -- a simply text
;; view that is capable of showing messages with their headers.
;; 
;; These methods are exported:
;; 
;;   (output-view-append obj header message)
;;   (output-view-append-error obj header message)
;;   (output-view-format obj output)
;;   (output-view-format-list obj output-list)
;;   (output-view-format-diff obj diff)
;;


;;; Code:

;; Module definition
(define-module (lazycat ui gtk output-view)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (gnome gtk gdk-event)
  #:export (<output-view>
            output-view-append output-view-append-error
            output-view-format output-view-format-list
            output-view-format-diff))


;;; Main class

(define-class <output-view> (<gtk-text-view>))

;; Class initialization
(define-method (initialize (obj <output-view>) args)
  (next-method)

  (slot-set! obj 'editable #f)

  (let* ((tag-table   (make <gtk-text-tag-table>))
         (text-buffer (make <gtk-text-buffer>
                        #:tag-table tag-table)))

    (set-buffer obj text-buffer)

    ;; Fill in table of tags for buffer

    (gtk-text-tag-table-add tag-table
                            (make <gtk-text-tag>
                              #:name       "normal"
                              #:background "grey"
                              #:foreground "black"))

    (gtk-text-tag-table-add tag-table
                            (make <gtk-text-tag>
                              #:name       "error"
                              #:background "grey"
                              #:foreground "red"))))


;;; Public methods

;; Format an OUTPUT from a host.
(define-method (output-view-format (obj <output-view>) (output <list>))

  (define *header-fmt* "<<< ~5d ~5a\n")

  (let ((host-id  (car output))
        (status   (car (cadr output)))
        (response (cadr (cadr output))))
    (if status
        (let ((header (with-output-to-string
                       (lambda ()
                         (format #t *header-fmt* host-id "OK")))))
          (output-view-append obj header response))
        (let ((header (with-output-to-string
                       (lambda ()
                         (format #t *header-fmt* host-id "ERROR")))))
          (output-view-append-error obj header response)))))

;; Format the list of outputs LIST from hosts.
(define-method (output-view-format-list (obj <output-view>) (list <list>))
  (let ((status (car list)))
    (if status
        (for-each
         (lambda (output)
           (output-view-format obj output))
         (cdr list))

        (let ((header "*** ERROR\n"))
          (output-view-append-error obj header (cadr list))))))

;; Format a diff.
(define-method (output-view-format-diff (obj <output-view>) (diff <list>))

  (define *header-fmt* "<<< ~5d ~5a\n")
  (define *output-fmt* "~a\n")

  (let ((status (car diff)))
    (if status
        (for-each
         (lambda (diff-result)
           (let* ((host-id (car diff-result))
                  (result  (cadr diff-result))
                  (header  (with-output-to-string
                            (lambda ()
                              (format #t *header-fmt* host-id result)))))

             (if (or (eq? result 'different) (eq? result 'error))
                 (output-view-append-error obj header (caddr diff-result))
                 (output-view-append obj header ""))))
         (cadr diff))

        (let ((header "*** ERROR"))
          (output-view-append-error obj header (cadr diff))))))

;; Append a message to the output view.
(define-method (output-view-append (obj     <output-view>)
                                          (header  <string>)
                                          (message <string>))
  (insert obj "normal" header message))

;; Append an error message to the output view.
(define-method (output-view-append-error (obj     <output-view>)
                                                (header  <string>)
                                                (message <string>))
  (insert obj "error" header message))


;;; Private methods

;; Insert generic message to the output view.
(define-method (insert (obj      <output-view>)
                       (tag-name <string>)
                       (header   <string>)
                       (message  <string>))
  (let* ((buffer       (get-buffer obj))
         (text-iter    (gtk-text-buffer-get-end-iter buffer))
         (mark         (gtk-text-buffer-create-mark  buffer #f text-iter #t))
         (iter-at-mark (gtk-text-buffer-get-iter-at-mark buffer mark)))

    (gtk-text-buffer-insert buffer text-iter header -1)
    (let ((iter-at-mark (gtk-text-buffer-get-iter-at-mark buffer mark)))
      (gtk-text-buffer-apply-tag-by-name buffer tag-name iter-at-mark text-iter))
    (gtk-text-buffer-insert buffer text-iter message -1)
    (gtk-text-buffer-insert buffer text-iter "\n" -1)
    ;; Scroll the window to the last line.
    (let ((mark (gtk-text-buffer-create-mark buffer #f text-iter #t)))
      (gtk-text-view-scroll-mark-onscreen obj mark))))

;;; output-view.scm ends here

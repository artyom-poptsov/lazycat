;;; LazyCat GTK output view.

;; Copyright (C) 2012 Artyom Poptsov <poptsov.artyom@gmail.com>
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

;; This module describes a <lc-gtk-output-view> class -- a simply text
;; view that is capable of showing messages with their headers.
;; 
;; These methods are exported:
;; 
;;   (lc-gtk-output-view-append obj header message)
;;   (lc-gtk-output-view-append-error obj header message)
;;


;;; Code:

;; Module definition
(define-module (lazycat lc-gtk-output-view)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (gnome gtk gdk-event)
  #:export (<lc-gtk-output-view>
            lc-gtk-output-view-append lc-gtk-output-view-append-error))


;;; Main class

(define-class <lc-gtk-output-view> (<gtk-text-view>))

;; Class initialization
(define-method (initialize (obj <lc-gtk-output-view>) args)
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

;; Append a message to the output view.
(define-method (lc-gtk-output-view-append (obj     <lc-gtk-output-view>)
                                          (header  <string>)
                                          (message <string>))
  (insert obj "normal" header message))

;; Append an error message to the output view.
(define-method (lc-gtk-output-view-append-error (obj     <lc-gtk-output-view>)
                                                (header  <string>)
                                                (message <string>))
  (insert obj "error" header message))


;;; Private methods

;; Insert generic message to the output view.
(define-method (insert (obj      <lc-gtk-output-view>)
                       (tag-name <string>)
                       (header   <string>)
                       (message  <string>))
  (let* ((buffer       (get-buffer obj))
         (text-iter    (gtk-text-buffer-get-end-iter buffer))
         (mark         (gtk-text-buffer-create-mark  buffer #f text-iter #t))
         (iter-at-mark (gtk-text-buffer-get-iter-at-mark buffer mark)))

    (gtk-text-buffer-insert buffer text-iter
                            (string-append "<<< " header "\n") -1)
    (let ((iter-at-mark (gtk-text-buffer-get-iter-at-mark buffer mark)))
      (gtk-text-buffer-apply-tag-by-name buffer tag-name iter-at-mark text-iter))
    (gtk-text-buffer-insert buffer text-iter message -1)
    (gtk-text-buffer-insert buffer text-iter "\n" -1)
    ;; Scroll the window to the last line.
    (let ((mark (gtk-text-buffer-create-mark buffer #f text-iter #t)))
      (gtk-text-view-scroll-mark-onscreen obj mark))))

;;; lc-gtk-output-view.scm ends here

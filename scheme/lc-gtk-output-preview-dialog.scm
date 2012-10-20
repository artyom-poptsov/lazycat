;;;; LazyCat GTK output preview dialog.
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

(define-module (lazycat lc-gtk-output-preview-dialog)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (gnome gtk gdk-event)
  #:export (<lc-gtk-output-preview-dialog> show-output))

(define-class <lc-gtk-output-preview-dialog> (<gtk-dialog>)
  (output-preview-buffer #:accessor output-preview-buffer))

(define-method (initialize (obj <lc-gtk-output-preview-dialog>) args)
  (next-method)

  (slot-set! obj 'title "Output preview")
  (slot-set! obj 'output-preview-buffer (make <gtk-text-buffer>))
  
  (set-default-size obj 800 500)

  (let* ((label          (make <gtk-label> #:label "Is this output correct?"))
         (output-preview (make <gtk-text-view>
                           #:buffer (output-preview-buffer obj)
                           #:editable #f))
         (yes-button     (gtk-dialog-add-button obj "No"  0))
         (no-button      (gtk-dialog-add-button obj "Yes" 1)))
    
    ;; Connect handlers to signals
    (connect yes-button 'clicked (lambda (w) (hide-all obj)))
    (connect no-button  'clicked (lambda (w) (hide-all obj)))

    ;; Pack GUI
    (gtk-box-pack-start (get-vbox obj) label #f #f 0)
    (gtk-box-pack-start (get-vbox obj) output-preview #t #t 0)))

(define-method (show-output (obj <lc-gtk-output-preview-dialog>) (output <string>))
  (let ((text-iter (gtk-text-buffer-get-end-iter (output-preview-buffer obj))))
    (gtk-text-buffer-insert (output-preview-buffer obj) text-iter output -1)
    (show-all obj)))

;;;; EOF

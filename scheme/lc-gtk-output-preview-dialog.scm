;;; lc-gtk-output-preview-dialog.scm -- LazyCat GTK output preview dialog.

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

;; This module describes a <lc-gtk-output-preview-dialog> class.
;; 
;; These methods are exported:
;; 
;;   (show-output obj output)


;;; Code:

;;; Module definition

(define-module (lazycat lc-gtk-output-preview-dialog)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (gnome gtk gdk-event)
  #:export (<lc-gtk-output-preview-dialog> show-output))


;;; Main class

(define-class <lc-gtk-output-preview-dialog> (<gtk-dialog>)
  (output-preview-buffer #:accessor output-preview-buffer))

;; Class inititalization
(define-method (initialize (obj <lc-gtk-output-preview-dialog>) args)
  (next-method)

  (slot-set! obj 'title "Output preview")
  (slot-set! obj 'output-preview-buffer (make <gtk-text-buffer>))
  
  (set-default-size obj 800 500)

  (let* ((label           (make <gtk-label> #:label "Is this output correct?"))
         (output-preview  (make <gtk-text-view>
                           #:buffer (output-preview-buffer obj)
                           #:editable #f))
         (scrolled-window (make <gtk-scrolled-window>
                            #:hscrollbar-policy 'automatic
                            #:vscrollbar-policy 'automatic))
         (yes-button      (gtk-dialog-add-button obj "No"  0))
         (no-button       (gtk-dialog-add-button obj "Yes" 1)))
    
    ;; Connect handlers to signals
    (connect yes-button 'clicked (lambda (w) (hide-all obj)))
    (connect no-button  'clicked (lambda (w) (hide-all obj)))

    ;; Pack GUI

    (gtk-container-add scrolled-window output-preview)

    (let ((vbox (get-vbox obj)))
      (gtk-box-pack-start vbox label #f #f 0)
      (gtk-box-pack-start vbox scrolled-window #t #t 0))))

(define-method (show-output (obj <lc-gtk-output-preview-dialog>) (output <string>))
  (gtk-text-buffer-set-text (output-preview-buffer obj) output)
  (show-all obj))

;;; lc-gtk-output-preview-dialog.scm ends here

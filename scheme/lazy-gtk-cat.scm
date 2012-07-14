;;;; GTK interface for LazyCat.
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

(read-set! keywords 'prefix)

(use-modules (gnome-2)
             (oop goops)
             (gnome gobject)
             (gnome gtk)
             (gnome gtk gdk-event)
             (ice-9 receive))

(load "./tools.scm")

(define (load-config name)
  "This function is used for loading config file."
  (let* ((lazycat-home (string-append (getenv "HOME") "/.lazycat"))
         (config (string-append lazycat-home "/" name)))
    (if (not (file-exists? lazycat-home))
        (system (string-append "mkdir " lazycat-home)))
    (if (file-exists? config)
        (load file))))

;;; Global variables

(define tmp-dir   "/tmp/lazycat")

(define mode-raw  "raw-mode")
(define mode-diff "diff-mode")
(define mode      mode-raw)

;;;   Create LazyCat GUI
;;; =============================================================================

;;
;;   ----------------------------
;;  | Main window
;;  |  --------------------------
;;  | | Main vbox
;;  | |  ------------------------
;;  | | | Menu bar
;;  | | |------------------------
;;  | | | Main paned
;;  | | |
;;

;;
;; Main window
;;

(define main-window (make <gtk-window> #:type  'toplevel #:title "LazyCat"))

;;
;; Main vbox
;;

(define main-vbox (make <gtk-vbox> #:homogeneous #f #:spacing 0))
(gtk-container-add main-window main-vbox)

;;
;; Menu bar
;;

(define menubar        (make <gtk-menu-bar>))

(define menu-item-file (make <gtk-menu-item> #:label "File"))
(define menu-file      (make <gtk-menu>))
(define menu-file-quit (make <gtk-menu-item> #:label "Quit"))

(define menu-item-mode (make <gtk-menu-item> #:label "Mode"))
(define menu-mode      (make <gtk-menu>))
(define menu-mode-raw  (gtk-radio-menu-item-new-with-label #f "Raw"))
(define menu-mode-diff (gtk-radio-menu-item-new-with-label
                        (gtk-radio-menu-item-get-group menu-mode-raw)
                        "Diff"))

(append menubar menu-item-file)
      
(set-submenu menu-item-file menu-file)
(append menu-file menu-file-quit)

(append menubar menu-item-mode)

(set-submenu menu-item-mode menu-mode)
(append menu-mode menu-mode-raw)
(gtk-menu-item-activate menu-mode-raw)
(append menu-mode menu-mode-diff)

;; Pack the Menu bar
(pack-start main-vbox menubar #f #f 0)

;;
;; Main paned
;;

(define main-paned (make <gtk-hpaned> #:position 300))
(pack-end main-vbox main-paned #t #t 0)

;;
;;   -----------------------------------------
;;  | Main paned                              |
;;  |  ----------------  :  ----------------  |
;;  | | Left vbox      | : | Right vbox     | |
;;  | |                | : |                | |
;;

;;
;; Left vbox
;;

(define left-vbox (make <gtk-vbox> #:homogeneous #f #:spacing 0))
(gtk-paned-add1 main-paned left-vbox)

;;
;; Right vbox
;;

(define right-vbox (make <gtk-vbox> #:homogeneous #f #:spacing 0))
(gtk-paned-add2 main-paned right-vbox)

;;
;;   ---------------------
;;  | Left vbox
;;  |  -------------------
;;  | | Tree view
;;  | |
;;  | |-------------------
;;  | | Button hbox
;;  | |
;;  |  -------------------
;;   ---------------------
;;

;;
;; Tree view
;;

(define host-list      (gtk-tree-store-new (list <gchararray> <gchararray> <gchararray>)))
(define tree-view      (make <gtk-tree-view> #:model host-list #:reorderable #t))
(define host-list-menu (make <gtk-menu>))

(define id-column      (make <gtk-tree-view-column> #:title "ID"))
(define name-column    (make <gtk-tree-view-column> #:resizable #f #:title "Name"))
(define desc-column    (make <gtk-tree-view-column> #:resizable #f #:title "Description"))
(define text-renderer  (gtk-cell-renderer-text-new))

;; Pack the Tree view
(pack-start left-vbox tree-view #t #t 0)

;;
;; Button hbox
;;

(define button-hbox     (make <gtk-hbox> #:homogeneous #f #:spacing 0))
(define add-host-button (make <gtk-button> #:label "Add host"))
(define rem-host-button (make <gtk-button> #:label "Remove host"))

;; Pack buttons
(pack-start button-hbox add-host-button #t #t 0)
(pack-end   button-hbox rem-host-button #t #t 0)

;; Pack the Button hbox
(pack-end left-vbox button-hbox #f #f 0)

;;
;;   ---------------------  
;;  | Right vbox
;;  |  -------------------
;;  | | Scrolled window
;;  | |
;;  | |-------------------
;;  | | Entry
;;  | |
;;  |  -------------------
;;   ---------------------
;;

;;
;; Scrolled window
;;

(define scrolled-window (make <gtk-scrolled-window>
                          #:hscrollbar-policy 'automatic
                          #:vscrollbar-policy 'automatic))

(pack-start right-vbox scrolled-window #t #t 0)

;;
;; Entry
;;

(define entry           (make <gtk-entry>))

(pack-end right-vbox entry #f #f 0)

;;
;;   ---------------------
;;  | Scrolled window
;;  |  -------------------
;;  | | Text view
;;  | |
;;

(define tag-table   (make <gtk-text-tag-table>))
(define text-buffer (make <gtk-text-buffer> #:tag-table tag-table))
(define text-view   (make <gtk-text-view> #:buffer text-buffer #:editable #f))

(gtk-container-add scrolled-window text-view)

;;
;; Popup menu:
;;
;;   ---------------------
;;  | Create group
;;  |---------------------
;;  | Remove group
;;   ---------------------
;;

(define popup-menu (make <gtk-menu>))
(define popup-menu-item-create-group (make <gtk-menu-item> #:label "Create group"))
(define popup-menu-item-remove-group (make <gtk-menu-item> #:label "Remove group"))

(append popup-menu popup-menu-item-create-group)
(append popup-menu popup-menu-item-remove-group)

(show-all popup-menu)

;;;   Dialog windows
;;; =============================================================================

(define (output-preview-dialog output)
  (let* ((dialog         (make <gtk-dialog>))
         (label          (make <gtk-label> #:label "Is this output correct?"))

         (output-buffer  (make <gtk-text-buffer>))
         (output-preview (make <gtk-text-view>
                           #:buffer output-buffer
                           #:editable #f))
        
         (yes-button     (gtk-dialog-add-button dialog "No"  0))
         (no-button      (gtk-dialog-add-button dialog "Yes" 1))

         (text-iter      (gtk-text-buffer-get-end-iter output-buffer)))

    (set-default-size dialog 800 500)

    ;; Show output
    (gtk-text-buffer-insert output-buffer text-iter output -1)

    ;; Connect handlers to signals
    (connect yes-button 'clicked (lambda (w) (destroy dialog)))
    (connect no-button  'clicked (lambda (w) (destroy dialog)))

    ;; Pack GUI
    (gtk-box-pack-start (get-vbox dialog) label #f #f 0)
    (gtk-box-pack-start (get-vbox dialog) output-preview #t #t 0)
  dialog))


(define (show-add-host-dialog . opt-dup)
  "'Add host' dialog"
  (let ((dialog                (make <gtk-dialog> #:title "Add host"))

        (label-proxy-name       (make <gtk-label> #:label "Proxy"))
        (cbox-proxy-name        (gtk-combo-box-new-text))

        (label-address          (make <gtk-label> #:label "Address"))
        (entry-address          (make <gtk-entry>))

        (label-host-name        (make <gtk-label> #:label "Host name"))
        (entry-host-name        (make <gtk-entry>))

        (label-host-description (make <gtk-label> #:label "Host descripion"))
        (entry-host-description (make <gtk-entry>))

        (main-vbox              (make <gtk-vbox> #:homogeneous #f #:spacing 0))
        (button-hbox            (make <gtk-hbox> #:homogeneous #f #:spacing 0))

        (button-add             (make <gtk-button> #:label "Add"))
        (button-cancel          (make <gtk-button> #:label "Cancel")))

    
    (define (add-widgets-to-box box . widgets)
      "This function adds widgets to given box"
      (define (add-widget widget) (pack-start box widget #t #t 0))
      (for-each add-widget widgets))

    (define (add-host-to-list)
      "Add host to the host list and to DB"
      (let ((host-id (lc-add-host
                      (gtk-combo-box-get-active-text cbox-proxy-name)
                      (gtk-entry-get-text entry-address)
                      (gtk-entry-get-text entry-host-name)
                      (gtk-entry-get-text entry-host-description)))
            (top-level (gtk-tree-store-append host-list #f)))

        (gtk-tree-store-set-value host-list top-level 0 (number->string host-id))
        (gtk-tree-store-set-value host-list top-level 1 (gtk-entry-get-text entry-host-name))
        (gtk-tree-store-set-value host-list top-level 2 (gtk-entry-get-text entry-host-description))

        (destroy dialog)))


    (gtk-combo-box-append-text cbox-proxy-name "tcp-proxy")
    (gtk-combo-box-set-active  cbox-proxy-name 0)

    ;;
    ;; Set default values
    ;;
    
    (gtk-entry-set-text entry-address          "127.0.0.1:50001")
    (gtk-entry-set-text entry-host-name        "localhost")
    (gtk-entry-set-text entry-host-description "Lazy server")

    ;;
    ;; Set up handlers for signals
    ;;
    
    (connect button-add 'clicked
             (lambda (w) (add-host-to-list)))
    
    (connect button-cancel 'clicked
             (lambda (w) (destroy dialog)))

    ;;
    ;; Assemble dialog
    ;;
    
    (add-widgets-to-box main-vbox
                        label-proxy-name
                        cbox-proxy-name
                        label-address
                        entry-address
                        label-host-name
                        entry-host-name
                        label-host-description
                        entry-host-description)

    (pack-start button-hbox button-add    #t #t 0)
    (pack-end   button-hbox button-cancel #t #t 0)

    (pack-start main-vbox button-hbox  #t #t 0)
    (pack-start (get-vbox dialog) main-vbox  #t #t 0)

    (show-all dialog)))

;;;   Functions related to sending and receiving messages
;;; =============================================================================

(define (send-message w)
  (let ((host-vector (lc-get-host-list))
        (message     (gtk-entry-get-text entry))
        (text-iter   (gtk-text-buffer-get-end-iter text-buffer)))

    (define (just-send-and-print host-id)
      "Send message to the host."
      (let ((mark (gtk-text-buffer-create-mark text-buffer #f text-iter #t)))
        
        ;; Insert label
        (gtk-text-buffer-insert text-buffer text-iter
                                (string-append "<<<" (number->string host-id) "\n") -1)

        (gtk-text-buffer-apply-tag-by-name text-buffer "normal"
                                           (gtk-text-buffer-get-iter-at-mark text-buffer mark)
                                           text-iter)

        ;; Send message, get response from host and print it.
        (gtk-text-buffer-insert text-buffer text-iter
                                (lc-send-msg host-id message) -1)))
    
    (define (fetch-and-analyse host-list pattern)
      
      (define (cut-timestamp output)
        "Skip the first line of output. Dirty hack."
        (let ((list (string->list output)))
          (list->string
           (let f ((list2 list))
             (if (char=? (car list2) #\newline)
                 list2
                 (f (cdr list2)))))))

      (define (compare host-id)
        (let ((diff (sdiff tmp-dir
                           (cut-timestamp pattern)
                           (cut-timestamp (lc-send-msg host-id message))))
              (mark (gtk-text-buffer-create-mark text-buffer #f text-iter #t)))

          ;; Insert label
          (gtk-text-buffer-insert text-buffer text-iter
                                  (string-append "Host #" (number->string host-id) "... ") -1)

          (gtk-text-buffer-apply-tag-by-name text-buffer "normal"
                                             (gtk-text-buffer-get-iter-at-mark text-buffer mark)
                                             text-iter)

          (set! mark (gtk-text-buffer-create-mark text-buffer #f text-iter #t))

          (if (string=? diff "")
              ;; Response match to pattern. 
              (begin
                (gtk-text-buffer-insert text-buffer text-iter "OK\n" -1)
                (gtk-text-buffer-apply-tag-by-name text-buffer "normal"
                                                   (gtk-text-buffer-get-iter-at-mark text-buffer mark)
                                                   text-iter))
              ;; Response doesn't match to pattern.
              ;; Print diff to the text buffer.
              (begin
                (gtk-text-buffer-insert text-buffer text-iter "NOK\n" -1)
                (gtk-text-buffer-apply-tag-by-name text-buffer "error"
                                                   (gtk-text-buffer-get-iter-at-mark text-buffer mark)
                                                   text-iter)
                (gtk-text-buffer-insert text-buffer text-iter diff -1)))))
      
      ;; Send message to every host from the host-list and compare response
      ;; with pattern.
      (for-each compare host-list))

    (if (string=? mode mode-raw)
        ;; Raw mode. Just send message to all hosts and print responses.
        (for-each just-send-and-print (vector->list host-vector))
        
        ;; Diff mode.
        ;; First host from list will be taken.
        (let* ((host-id   (vector-ref host-vector 0))
               (pattern   (lc-send-msg host-id message))
               (host-list '()))

          (if (> (vector-length host-vector) 1)
              ;; If there is more than one host in the list -
              ;; skip the host which is taken as pattern.
              (for-each (lambda (host-id)
                          (if (not (= host-id 1))
                              (set! host-list (append host-list (list host-id)))))
                        (vector->list host-vector))
              (append host-list (list host-id)))

          ;; Show preview of output from the model host
          (let ((dialog (output-preview-dialog pattern)))
            (connect dialog 'response
                     (lambda (w rsp)
                       (if (= rsp 1) (fetch-and-analyse host-list pattern))))
            (show-all dialog)))))

  (gtk-entry-set-text entry ""))

;;;   Basic initalization
;;; =============================================================================

;;
;; Fill in table of tags for buffer
;;

(gtk-text-tag-table-add tag-table (make <gtk-text-tag>
                                    #:name       "normal"
                                    #:background "grey"
                                    #:foreground "black"))

(gtk-text-tag-table-add tag-table (make <gtk-text-tag>
                                    #:name       "error"
                                    #:background "grey"
                                    #:foreground "red"))

;;
;; Setup the tree-view
;; 

(gtk-tree-view-append-column tree-view id-column   0)
(gtk-tree-view-append-column tree-view name-column 1)
(gtk-tree-view-append-column tree-view desc-column 2)
    
(gtk-tree-view-column-pack-start id-column   text-renderer #t)
(gtk-tree-view-column-pack-start name-column text-renderer #t)
(gtk-tree-view-column-pack-end   desc-column text-renderer #t)

(gtk-tree-view-column-add-attribute id-column   text-renderer "text" 0)
(gtk-tree-view-column-add-attribute name-column text-renderer "text" 1)
(gtk-tree-view-column-add-attribute desc-column text-renderer "text" 2)

;;;   Handlers
;;; =============================================================================

(define (remove-host-from-list w)
  "This function removes host from DB and host list"
  (let* ((selection   (gtk-tree-view-get-selection tree-view))
         (host-id     0))

    (receive (model iter) (gtk-tree-selection-get-selected selection)
      (lc-rem-host (gtk-tree-model-get-value model iter 0))
      (gtk-tree-store-remove model iter))))

(define (host-list-add-group . w)
  "Add group to the host list"
  (let* ((dialog        (make <gtk-dialog> #:title "New group"))
         (entry         (make <gtk-entry>))
         (cancel-button (gtk-dialog-add-button dialog "Cancel" 0))
         (ok-button     (gtk-dialog-add-button dialog "OK"     1))
         (top-level     (gtk-tree-store-append host-list #f)))

    (gtk-box-pack-start (get-vbox dialog) entry #f #f 0)
    
    (connect ok-button 'clicked
             (lambda (w) (begin
                           (gtk-tree-store-set-value host-list top-level 1
                                                     (gtk-entry-get-text entry))
                           (destroy dialog))))

    (connect cancel-button 'clicked (lambda (w) (destroy dialog)))
    
    (show-all dialog)))

(define (host-list-rem-group . w)
  "Remove group from the  host list"
  (let ((selection (gtk-tree-view-get-selection tree-view)))
    (receive (model iter) (gtk-tree-selection-get-selected selection)
      (gtk-tree-store-remove model iter))))

;;;   Connect handlers to signals
;;; =============================================================================

(connect entry           'activate   send-message)
(connect add-host-button 'clicked    show-add-host-dialog)
(connect rem-host-button 'clicked    remove-host-from-list)
(connect main-window     'delete-event (lambda (w e) (gtk-main-quit) #f))

(connect menu-file-quit 'activate (lambda (w) (gtk-main-quit)))
(connect menu-mode-raw  'activate (lambda (w) (set! mode mode-raw)))
(connect menu-mode-diff 'activate (lambda (w) (set! mode mode-diff)))

(connect tree-view 'button-press-event
         (lambda (w e)
           (cond ((= (gdk-event-button:button e) 3)
                  (gtk-menu-popup popup-menu ; menu
                                  #f         ; parent-menu-shell
                                  #f         ; parent-menu-item
                                  #f         ; func
                                  3          ; button
                                  0)         ; activate-time
                  #t)
                 (else #f))))

(connect popup-menu-item-create-group 'activate (lambda (w) (host-list-add-group)))
(connect popup-menu-item-remove-group 'activate (lambda (w) (host-list-rem-group)))

;;;   Show main window
;;; =============================================================================

(show-all main-window)

(gtk-main)
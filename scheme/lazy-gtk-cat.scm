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

(use-modules (gnome-2))
(use-modules (oop goops))
(use-modules (gnome gobject))
(use-modules (gnome gtk))
(use-modules (ice-9 receive))

(define (load-config name)
  "This function is used for loading config file."
  (let* ((lazycat-home (string-append (getenv "HOME") "/.lazycat"))
         (config (string-append lazycat-home "/" name)))
    (if (not (file-exists? lazycat-home))
        (system (string-append "mkdir " lazycat-home)))
    (if (file-exists? config)
        (load file))))

;; This function was taken from file test-gtk.scm, which is one of
;; examples shipped with guile-gtk
(define (hack-gtk-radio-menu-item-new-with-label-from-widget group str)
  (if group
      (gtk-radio-menu-item-new-with-label-from-widget group str)
      (let ((item  (make <gtk-radio-menu-item>))
            (label (make <gtk-label> #:label str #:xalign 0 #:yalign 0.5)))
        (gtk-widget-show label)
        (gtk-container-add item label)
        item)))


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

(define (get-diff pattern output)
  "Function returns diff between pattern and output"
  (let* ((tmp-file-dir "/tmp/lazycat")
         (tmp-file1     (string-append tmp-file-dir "/tmp1"))
         (tmp-file2     (string-append tmp-file-dir "/tmp2")))

    (define (diff file1 file2)
      "Wrapper for diff tool."
      (let ((diff-file (string-append tmp-file-dir "/diff"))
            (diff-in   #f)
            (ret       ""))

        ;; Create file which will be used for store diff
        (system (string-append "touch " diff-file))

        (set! diff-in (open-input-file diff-file))
      
        (system (string-append "diff " file1 " " file2 " > " diff-file))

        (let f ((ret (read-char diff-in)))
          (if (eof-object? ret)
              ""
              (string-append (string ret) (f (read-char diff-in)))))))

    (define (cut-timestamp output)
      "Skip the first line of output. Dirty hack."
      (let ((list (string->list output)))
        (list->string
         (let f ((list2 list))
         (if (char=? (car list2) #\newline)
             list2
             (f (cdr list2)))))))

    (system (string-append "touch " tmp-file1 " " tmp-file2))

    (with-output-to-file tmp-file1
      (lambda ()
        (write (cut-timestamp pattern))))
    
    (with-output-to-file tmp-file2
      (lambda ()
        (write (cut-timestamp output))))
    
    (diff tmp-file1 tmp-file2)))

(define (lazy-gtk-cat)
  "Main function of lazy-gtk-cat."
  (read-set! keywords 'prefix)

  (let* ((main-window     (make <gtk-window>
                            #:type  'toplevel
                            #:title "LazyCat"))
         (menu            (make <gtk-menu>))
         (menubar         (make <gtk-menu-bar>))

         (main-vbox       (make <gtk-vbox> #:homogeneous #f #:spacing 0))
         (hbox            (make <gtk-hbox> #:homogeneous #f #:spacing 0))
         
         (scrolled-win    (make <gtk-scrolled-window>
                            #:hscrollbar-policy 'automatic
                            #:vscrollbar-policy 'automatic))

         (main-paned      (make <gtk-hpaned> #:position 300))
         (left-vbox       (make <gtk-vbox> #:homogeneous #f #:spacing 0))
         (right-vbox      (make <gtk-vbox> #:homogeneous #f #:spacing 0))

         (host-list       (gtk-tree-store-new (list <guint> <gchararray> <gchararray>)))
         (tree-view       (make <gtk-tree-view> #:model host-list))
         
         (id-column       (make <gtk-tree-view-column> #:title "ID"))
         (name-column     (make <gtk-tree-view-column> #:resizable #f #:title "Name"))
         (desc-column     (make <gtk-tree-view-column> #:resizable #f #:title "Description"))
         (text-renderer   (gtk-cell-renderer-text-new))
         
         (button-hbox     (make <gtk-hbox> #:homogeneous #f #:spacing 0))
         (add-host-button (make <gtk-button> #:label "Add host"))
         (rem-host-button (make <gtk-button> #:label "Remove host"))
         
         (entry           (make <gtk-entry>))

         (tag-table       (make <gtk-text-tag-table>))
         (text-buffer     (make <gtk-text-buffer> #:tag-table tag-table))
         (text            (make <gtk-text-view>
                            #:buffer text-buffer
                            #:editable #f))

         ;; Variables
         (selected-row    0)
         (mode-raw        "raw-mode")
         (mode-diff       "diff-mode")
         (mode            ""))

    ;;   Define needed functions
    ;; ==========================================================================

    (define (activate-entry . opt-dup)
      (let ((host-vector (lc-get-host-list))
            (message     (gtk-entry-get-text entry))
            (text-iter   (gtk-text-buffer-get-end-iter text-buffer)))

        (define (send-message host-id)
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
          (define (compare host-id)
            (let ((diff (get-diff pattern (lc-send-msg host-id message)))
                  (mark (gtk-text-buffer-create-mark text-buffer #f text-iter #t)))

              (gtk-text-buffer-insert text-buffer text-iter
                                      (string-append "Host #" (number->string host-id) "... ") -1)

              (gtk-text-buffer-apply-tag-by-name text-buffer "normal"
                                                 (gtk-text-buffer-get-iter-at-mark text-buffer mark)
                                                 text-iter)

              (set! mark (gtk-text-buffer-create-mark text-buffer #f text-iter #t))

              (if (string=? diff "")
                  (begin
                    (gtk-text-buffer-insert text-buffer text-iter "OK\n" -1)
                    (gtk-text-buffer-apply-tag-by-name text-buffer "normal"
                                                       (gtk-text-buffer-get-iter-at-mark text-buffer mark)
                                                       text-iter))
                  (begin
                    (gtk-text-buffer-insert text-buffer text-iter "NOK\n" -1)
                    (gtk-text-buffer-apply-tag-by-name text-buffer "error"
                                                       (gtk-text-buffer-get-iter-at-mark text-buffer mark)
                                                       text-iter)
                    (gtk-text-buffer-insert text-buffer text-iter diff -1)))))
          
          (for-each compare host-list))

        
        (if (string=? mode mode-raw)
            ;; Raw mode. Just send message to all hosts and print responses.
            (for-each send-message (vector->list host-vector))
            
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

              (let ((dialog (output-preview-dialog pattern)))
                (connect dialog 'response
                         (lambda (w rsp)
                           (if (= rsp 1) (fetch-and-analyse host-list pattern))))
                (show-all dialog)))))

      (gtk-entry-set-text entry ""))

    
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

            (gtk-tree-store-set-value host-list top-level 0 host-id)
            (gtk-tree-store-set-value host-list top-level 1 (gtk-entry-get-text entry-host-name))
            (gtk-tree-store-set-value host-list top-level 2 (gtk-entry-get-text entry-host-description))

            (destroy dialog)))


        (gtk-combo-box-append-text cbox-proxy-name "tcp-proxy")
        (gtk-combo-box-set-active  cbox-proxy-name 0)
        
        ;; Set default values
        
        (gtk-entry-set-text entry-address          "127.0.0.1:50001")
        (gtk-entry-set-text entry-host-name        "localhost")
        (gtk-entry-set-text entry-host-description "Lazy server")
        
        ;; Set up handlers for signals
        
        (connect button-add 'clicked
                 (lambda (w) (add-host-to-list)))
        
        (connect button-cancel 'clicked
                 (lambda (w) (destroy dialog)))

        ;; Assemble dialog
        
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

    
    (define (remove-host-from-list w)
      "This function removes host from DB and host list"
      (let* ((selection   (gtk-tree-view-get-selection tree-view))
             (host-id     0))

        (receive (model iter) (gtk-tree-selection-get-selected selection)
          (lc-rem-host (gtk-tree-model-get-value model iter 0))
          (gtk-tree-store-remove model iter))))


    (gtk-tree-view-append-column tree-view id-column   0)
    (gtk-tree-view-append-column tree-view name-column 1)
    (gtk-tree-view-append-column tree-view desc-column 2)
    
    (gtk-tree-view-column-pack-start id-column   text-renderer #t)
    (gtk-tree-view-column-pack-start name-column text-renderer #t)
    (gtk-tree-view-column-pack-end   desc-column text-renderer #t)

    (gtk-tree-view-column-add-attribute id-column   text-renderer "text" 0)
    (gtk-tree-view-column-add-attribute name-column text-renderer "text" 1)
    (gtk-tree-view-column-add-attribute desc-column text-renderer "text" 2)


    ;; Fill in table of tags for buffer
    
    (gtk-text-tag-table-add tag-table (make <gtk-text-tag>
                                        #:name       "normal"
                                        #:background "grey"
                                        #:foreground "black"))
    
    (gtk-text-tag-table-add tag-table (make <gtk-text-tag>
                                        #:name       "error"
                                        #:background "grey"
                                        #:foreground "red"))
        
    ;; Set default mode
    (set! mode mode-raw)
    
    ;;   Connect handlers to signals
    ;; ==========================================================================

    (connect entry           'activate   activate-entry)
    (connect add-host-button 'clicked    show-add-host-dialog)
    (connect rem-host-button 'clicked    remove-host-from-list)
    (connect main-window     'delete-event (lambda (w e) (gtk-main-quit) #f))

    ;;   Setup the GUI
    ;; ==========================================================================

    ;; Pack left panel
    ;; ---------------

    ;; Pack buttons
    (pack-start button-hbox add-host-button #t #t 0)
    (pack-end   button-hbox rem-host-button #t #t 0)

    (pack-start left-vbox tree-view   #t #t 0)
    (pack-end   left-vbox button-hbox #f #f 0)

    ;; Pack vbox
    (gtk-paned-add1 main-paned left-vbox)

    ;; Pack right panel
    ;; ----------------

    (gtk-container-add scrolled-win text)
    
    (pack-start right-vbox scrolled-win #t #t 0)
    (pack-end   right-vbox entry        #f #f 0)
    
    (gtk-paned-add2 main-paned right-vbox)

    ;; Pack menu
    ;; ---------

    (let ((menu-item-file (make <gtk-menu-item> #:label "File"))
          (menu-file      (make <gtk-menu>))
          (menu-file-quit (make <gtk-menu-item> #:label "Quit"))
          (menu-item-mode (make <gtk-menu-item> #:label "Mode"))
          (menu-mode      (make <gtk-menu>))
          (group          #f)
          (menu-mode-raw  #f)
          (menu-mode-diff #f))

      (set! menu-mode-raw  (hack-gtk-radio-menu-item-new-with-label-from-widget group "Raw" ))
      (set! group menu-mode-raw)
      (set! menu-mode-diff (hack-gtk-radio-menu-item-new-with-label-from-widget group "Diff"))
      
      (append menubar menu-item-file)
      
      (set-submenu menu-item-file menu-file)
      (append menu-file menu-file-quit)

      (append menubar menu-item-mode)

      (set-submenu menu-item-mode menu-mode)
      (append menu-mode menu-mode-raw)
      (gtk-menu-item-activate menu-mode-raw)
      (append menu-mode menu-mode-diff)

      (connect menu-file-quit 'activate (lambda (w) (gtk-main-quit)))
      (connect menu-mode-raw  'activate (lambda (w) (set! mode mode-raw)))
      (connect menu-mode-diff 'activate (lambda (w) (set! mode mode-diff))))

    ;; Pack main window
    ;; ----------------

    (pack-start main-vbox menubar    #f #f 0)
    (pack-end   main-vbox main-paned #t #t 0)

    (gtk-container-add main-window main-vbox)

    ;; Show all widgets
    ;; ----------------

    (show-all main-window))
  (gtk-main))

(load-config "hosts")

;; Run main function
(lazy-gtk-cat)
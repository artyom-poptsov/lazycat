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

(use-modules (gtk gtk))

(define (load-config name)
  "This function is used for loading config file."
  (let* ((lazycat-home (string-append (getenv "HOME") "/.lazycat"))
         ((config (string-append lazycat-home "/" name))))
    (if (not (file-exists? lazycat-home))
        (system (string-append "mkdir " lazycat-home)))
    (if (file-exists? config)
        (load file))))

(define (gtk-widget-show-multi . widgets)
  "Showing list of widgets"
  (for-each gtk-widget-show widgets))

;; This function was taken from file test-gtk.scm, which is one of
;; examples shipped with guile-gtk
(define (hack-gtk-radio-menu-item-new-with-label-from-widget group str)
  (if group
      (gtk-radio-menu-item-new-with-label-from-widget group str)
      (let ((item  (gtk-widget-new 'GtkRadioMenuItem))
            (label (gtk-widget-new 'GtkLabel
                                   #:label str #:xalign 0 #:yalign 0.5)))
        (gtk-widget-show label)
        (gtk-container-add item label)
        item)))


(define (output-preview-dialog output)
  (let ((dialog         (gtk-dialog-new))
        (label          (gtk-label-new "Is this output correct?"))
        (output-preview (gtk-text-new #f #f))
        (main-vbox      (gtk-vbox-new #f 0))
        (button-hbox    (gtk-hbox-new #f 0))
        (yes-button     (gtk-button-new-with-label "Yes"))
        (no-button      (gtk-button-new-with-label "No")))

    (gtk-widget-set-usize output-preview 800 500)

    ;; Show output
    (gtk-text-insert output-preview #f "black" "white" output -1)

    ;; Connect handlers to signals
    
    (gtk-signal-connect yes-button "clicked"
                        (lambda ()
                          (gtk-dialog-response dialog 1)
                          (gtk-widget-destroy dialog)))

    (gtk-signal-connect no-button "clicked"
                        (lambda ()
                          (gtk-widget-destroy dialog)
                          (gtk-dialog-response dialog 0)))

    ;; Pack GUI
    (gtk-box-pack-start button-hbox yes-button)
    (gtk-box-pack-end   button-hbox no-button)
    
    (gtk-box-pack-start main-vbox label)
    (gtk-box-pack-start main-vbox output-preview)
    (gtk-box-pack-end   main-vbox button-hbox)
    (gtk-box-pack-start (gtk-dialog-action-area dialog) main-vbox  #t #t 0)

    (gtk-window-set-modal dialog #t)
  dialog))

(define (get-diff pattern output)
  "Function returns diff between pattern and output"
  (let* ((tmp-file-dir "/tmp/lazycat")
         (tmp-file1     (string-append tmp-file-dir "/tmp1"))
         (tmp-file2     (string-append tmp-file-dir "/tmp2")))

    (define (diff file1 file2)
      "Wrapper for diff tool."
      (let* ((diff-file (string-append tmp-file-dir "/diff"))
             (diff-in   (open-input-file diff-file))
             (ret        ""))
        
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

  (let* ((main-window     (gtk-window-new 'toplevel))
         (menu            (gtk-menu-new))
         (menubar         (gtk-menu-bar-new))

         (main-vbox       (gtk-vbox-new #f 0))
         (top-hbox        (gtk-hbox-new #f 0))
         (hbox            (gtk-hbox-new #f 0))
         
         (scrolled-win    (gtk-scrolled-window-new))
         
         (left-vbox       (gtk-vbox-new #f 0))
         (right-vbox      (gtk-vbox-new #f 0))

         (titles          #("ID" "Name"))
         (host-list       (gtk-clist-new-with-titles titles))
         
         (button-hbox     (gtk-hbox-new #f 0))
         (add-host-button (gtk-button-new-with-label "Add host"))
         (rem-host-button (gtk-button-new-with-label "Remove host"))
         
         (entry           (gtk-entry-new))
         (text            (gtk-text-new #f #f))
         ;; Variables
         (selected-row    0)
         
         (mode-raw        "raw-mode")
         (mode-diff       "diff-mode")
         (mode            ""))

    ;;   Define needed functions
    ;; ==========================================================================

    (define (activate-entry . opt-dup)
      (let ((host-vector (lc-get-host-list))
            (message     (gtk-entry-get-text entry)))

        (define (send-message host-id)
          "Send message to the host."
          ;; Insert separator
          (gtk-text-insert text #f "black" "yellow"
                           (string-append "<<<" (number->string host-id) "\n") -1)
          ;; Send message, get response from host and print it.
          (gtk-text-insert text #f "black" "white"
                           (lc-send-msg host-id message) -1))

        (define (fetch-and-analyse host-list pattern)
          (define (compare host-id)
            (let ((diff (get-diff pattern (lc-send-msg host-id message))))
              
              (gtk-text-insert text #f "black" "grey"
                               (string-append "Host #" (number->string host-id) "... ") -1)
              
              (if (string=? diff "")
                  (begin
                    (gtk-text-insert text #f "green" "grey" "OK\n" -1)
                    (gtk-text-insert text #f "black" "white" pattern -1))
                  (begin
                    (gtk-text-insert text #f "red"   "grey" "NOK\n" -1)
                    (gtk-text-insert text #f "black" "white" diff -1)))))

          (for-each compare host-list))

        
        (if (string=? mode mode-raw)
            ;; Raw mode. Just send message to all hosts and print responses.
            (for-each send-message (vector->list host-vector))
            
            ;; Diff mode.
            ;; First host from list will be taken.
            (let* ((host-id   (vector-ref host-vector 0))
                   (pattern   (lc-send-msg host-id message))
                   (host-list '()))

              (for-each (lambda (host-id)
                          (if (not (= host-id 1))
                              (set! host-list (append host-list (list host-id)))))
               (vector->list host-vector))

              (let ((dialog (output-preview-dialog pattern)))
                
                (gtk-signal-connect dialog "response"
                                    (lambda (rsp)
                                      (if (= rsp 1)
                                          (fetch-and-analyse host-list pattern))))
                                    
                (gtk-widget-show-all dialog)))))

        (gtk-text-thaw text)
        (gtk-entry-set-text entry ""))

    
    (define (show-add-host-dialog . opt-dup)
      "'Add host' dialog"
      (let ((dialog                (gtk-dialog-new))

            (proxies                #("tcp-proxy"))
            (label-proxy-name       (gtk-label-new "Proxy"))
            (cbox-proxy-name        (gtk-combo-new))

            (label-address          (gtk-label-new "Address"))
            (entry-address          (gtk-entry-new))

            (label-host-name        (gtk-label-new "Host name"))
            (entry-host-name        (gtk-entry-new))

            (label-host-description (gtk-label-new "Host descripion"))
            (entry-host-description (gtk-entry-new))

            (main-vbox              (gtk-vbox-new #f 0))
            (button-hbox            (gtk-hbox-new #f 0))

            (button-add             (gtk-button-new-with-label "Add"))
            (button-cancel          (gtk-button-new-with-label "Cancel")))

        (define (add-widgets-to-box box . widgets)
          "This function adds widgets to given box"
          (define (add-widget widget) (gtk-box-pack-start box widget #t #t 0))
          (for-each add-widget widgets))

        (define (add-host-to-list)
          "Add host to the host list and to DB"
          (let ((host-id (lc-add-host
                          (gtk-entry-get-text (gtk-combo-entry cbox-proxy-name))
                          (gtk-entry-get-text entry-address)
                          (gtk-entry-get-text entry-host-name)
                          (gtk-entry-get-text entry-host-description))))

            (let ((row (vector (number->string host-id)
                               (gtk-entry-get-text entry-host-name))))
              (gtk-clist-append host-list row))
            (gtk-clist-thaw host-list)
            (gtk-widget-destroy dialog)))

        (gtk-combo-set-popdown-strings cbox-proxy-name proxies)
        
        ;; Set default values
        
        (gtk-entry-set-text entry-address          "127.0.0.1:50001")
        (gtk-entry-set-text entry-host-name        "localhost")
        (gtk-entry-set-text entry-host-description "Lazy server")
        
        ;; Set up handlers for signals
        
        (gtk-signal-connect button-add "clicked"
                            (lambda () (add-host-to-list)))
        
        (gtk-signal-connect button-cancel "clicked"
                            (lambda () (gtk-widget-destroy dialog)))

        ;; Assemble dialog
        
        (gtk-window-set-title dialog "Add host")
        
        (add-widgets-to-box main-vbox
                            label-proxy-name
                            cbox-proxy-name
                            label-address
                            entry-address
                            label-host-name
                            entry-host-name
                            label-host-description
                            entry-host-description)

        (gtk-box-pack-start button-hbox button-add    #t #t 0)
        (gtk-box-pack-end   button-hbox button-cancel #t #t 0)

        (gtk-box-pack-start main-vbox button-hbox  #t #t 0)
        (gtk-box-pack-start (gtk-dialog-action-area dialog) main-vbox  #t #t 0)

        (gtk-widget-show-all dialog)))

    (define (remove-host-from-list)
      "This function removes host from DB and host list"
      (let ((text-vector (make-vector 1)))
        
        (vector-set! text-vector 0 "")
        
        (gtk-clist-get-text host-list selected-row 0 text-vector)

        (let ((host-id (vector-ref text-vector 0)))
          (gtk-clist-remove host-list selected-row)
          (lc-rem-host (string->number host-id)))))

    ;; Set default mode
    (set! mode mode-raw)
    
    ;;   Connect handlers to signals
    ;; ==========================================================================

    (gtk-signal-connect entry           "activate"   activate-entry)
    (gtk-signal-connect add-host-button "clicked"    show-add-host-dialog)
    (gtk-signal-connect rem-host-button "clicked"    remove-host-from-list)
    (gtk-signal-connect host-list       "select-row"
                        (lambda (row col event)
                          (set! selected-row row)))

    ;;   Setup the GUI
    ;; ==========================================================================

    (gtk-window-set-title main-window "LazyCat")
    (gtk-scrolled-window-set-policy scrolled-win 'automatic 'automatic)
    (gtk-widget-set-usize scrolled-win 800 120)

    ;; Pack left panel
    ;; ---------------

    ;; Pack buttons
    (gtk-box-pack-start button-hbox add-host-button #t #t 0)
    (gtk-box-pack-end   button-hbox rem-host-button #t #t 0)
    ;; Pack vbox
    (gtk-box-pack-start left-vbox host-list    #t #t 0)
    (gtk-box-pack-end   left-vbox button-hbox  #f #t 0)    

    ;; Pack right panel
    ;; ----------------

    (gtk-container-add scrolled-win text)

    (gtk-box-pack-start right-vbox scrolled-win #t #t 0)
    (gtk-box-pack-end   right-vbox entry        #f #f 0)

    ;; Pack menu
    ;; ---------

    (let ((menu-item-file (gtk-menu-item-new-with-label "File"))
          (menu-file      (gtk-menu-new))
          (menu-file-quit (gtk-menu-item-new-with-label "Quit"))
          (menu-item-mode (gtk-menu-item-new-with-label "Mode"))
          (menu-mode      (gtk-menu-new))
          (group          #f)
          (menu-mode-raw  #f)
          (menu-mode-diff #f))

      (set! menu-mode-raw  (hack-gtk-radio-menu-item-new-with-label-from-widget group "Raw" ))
      (set! group menu-mode-raw)
      (set! menu-mode-diff (hack-gtk-radio-menu-item-new-with-label-from-widget group "Diff"))
      
      (gtk-menu-bar-append menubar menu-item-file)
      
      (gtk-menu-item-set-submenu menu-item-file menu-file)
      (gtk-menu-append menu-file menu-file-quit)

      (gtk-menu-bar-append menubar menu-item-mode)

      (gtk-menu-item-set-submenu menu-item-mode menu-mode)
      (gtk-menu-append menu-mode menu-mode-raw)
      (gtk-menu-item-activate menu-mode-raw)
      (gtk-menu-append menu-mode menu-mode-diff)

      (gtk-signal-connect menu-file-quit "activate"
                          (lambda () (if (gtk-standalone?)
                                         (gtk-exit)
                                         (gtk-widget-destroy window))))

      (gtk-signal-connect menu-mode-raw "activate"
                          (lambda () (set! mode mode-raw)))
      (gtk-signal-connect menu-mode-diff "activate"
                          (lambda () (set! mode mode-diff)))
      
      (gtk-widget-show-multi menu-item-file
                             menu-file
                             menu-file-quit
                             menu-item-mode
                             menu-mode
                             menu-mode-raw
                             menu-mode-diff))

    ;; Pack main window
    ;; ----------------

    (gtk-box-pack-start top-hbox left-vbox  #t #t 0)
    (gtk-box-pack-end   top-hbox right-vbox #t #t 0)

    (gtk-box-pack-start main-vbox menubar   #f #f 0)
    (gtk-box-pack-end   main-vbox top-hbox  #t #t 0)

    (gtk-container-add main-window main-vbox)

    ;; Show all widgets
    ;; ----------------

    (if (gtk-standalone?) (gtk-signal-connect main-window "destroy" gtk-exit))
    
    (gtk-widget-show-all main-window)

    (gtk-standalone-main main-window)))

(load-config "hosts")

;; Run main function
(lazy-gtk-cat)

;;; lazy-gtk-cat.scm -- A GTK interface for LazyCat.

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

;; This module describes a <lazy-gtk-cat> class -- a GUI based on GTK
;; for the LazyCat.
;;
;; The application can be run by calling the run method on
;; <lazy-gtk-cat> instance.
;;
;; These methods are exported:
;;
;;   (run lazy-gtk-cat)


;;; Code:

(define-module (lazycat ui gtk lazy-gtk-cat)
  #:use-module (ice-9 rdelim)
  #:use-module (oop goops)
  #:use-module (gnome gtk)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk gdk-event)
  ;; LazyCat modules
  #:use-module (lazycat ui gtk output-preview-dialog)
  #:use-module (lazycat ui gtk add-host-dialog)
  #:use-module (lazycat ui gtk output-view)
  #:use-module (lazycat ui gtk host-tree)
  #:use-module (lazycat protocol)
  #:use-module (lazycat logger)
  #:export (<lazy-gtk-cat> run))


;;; Constants

(define *mode-raw*         "raw-mode")
(define *mode-diff*        "diff-mode")
(define *application-name* "Lazy GTK Cat")


;;; Main class

(define-class <lazy-gtk-cat> ()

  (server-socket-path
   #:getter get-server-socket-path
   #:init-value "/tmp/lazycat/server")

  (server-socket
   #:getter get-server-socket
   #:setter set-server-socket)

  ;; Directories
  (mode
   #:getter get-mode
   #:setter set-mode
   #:init-value *mode-raw*)

  (master-host-id
   #:getter get-master-host-id
   #:setter set-master-host-id
   #:init-value 1)

  ;; Dialog windows
  (output-preview-dialog #:accessor output-preview-dialog)
  (add-host-dialog       #:accessor add-host-dialog)

  ;; GUI objects

  (gtk-main-window #:accessor gtk-main-window)
  (gtk-main-vbox   #:accessor gtk-main-vbox)

  (gtk-menu-bar       #:accessor gtk-menu-bar)
  (gtk-menu-item-file #:accessor gtk-menu-item-file)
  (gtk-menu-file      #:accessor gtk-menu-file)
  (gtk-menu-file-quit #:accessor gtk-menu-file-quit)
  (gtk-menu-item-mode #:accessor gtk-menu-item-mode)
  (gtk-menu-mode      #:accessor gtk-menu-mode)
  (gtk-menu-mode-raw  #:accessor gtk-menu-mode-raw)
  (gtk-menu-mode-diff #:accessor gtk-menu-mode-diff)

  (gtk-main-paned #:accessor gtk-main-paned)
  (gtk-left-vbox  #:accessor gtk-left-vbox)
  (gtk-right-vbox #:accessor gtk-right-vbox)

  (gtk-button-hbox     #:accessor gtk-button-hbox)
  (gtk-add-host-button #:accessor gtk-add-host-button)
  (gtk-rem-host-button #:accessor gtk-rem-host-button)

  (gtk-scrolled-window #:accessor gtk-scrolled-window)
  (gtk-entry           #:accessor gtk-entry)

  (gtk-host-tree-menu              #:accessor gtk-host-tree-menu)
  (gtk-host-tree-menu-set-master   #:accessor gtk-host-tree-menu-set-master)
  (gtk-host-tree-menu-create-group #:accessor gtk-host-tree-menu-create-group)
  (gtk-host-tree-menu-remove-group #:accessor gtk-host-tree-menu-remove-group)

  (logger
   #:accessor logger
   #:init-value (make <logger> #:ident "lazy-gkt-cat" #:facility 'user))

  (output-view
   #:getter get-output-view
   #:init-value (make <output-view>))

  (host-tree
   #:getter get-host-tree
   #:init-value (make <host-tree>)))


;;; Class initialization

(define-method (initialize (obj <lazy-gtk-cat>) args)
  (next-method)

  (set-server-socket obj (socket PF_UNIX SOCK_STREAM 0))

  (gdk-threads-init)

  ;; Dialog windows
  (slot-set! obj 'output-preview-dialog (make <output-preview-dialog>))
  (slot-set! obj 'add-host-dialog (make <add-host-dialog>
                                    #:proxy-list '("tcp-proxy" "ssh-proxy")))


  ;; Create LazyCat GUI

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
  (slot-set! obj 'gtk-main-window (make <gtk-window> #:type 'toplevel #:title *application-name*))
  (slot-set! obj 'gtk-main-vbox   (make <gtk-vbox> #:homogeneous #f #:spacing 0))
  (gtk-container-add (gtk-main-window obj) (gtk-main-vbox obj))

  ;;
  ;; Menu bar
  ;;
  (slot-set! obj 'gtk-menu-bar       (make <gtk-menu-bar>))
  (slot-set! obj 'gtk-menu-item-file (make <gtk-menu-item> #:label "File"))
  (slot-set! obj 'gtk-menu-file      (make <gtk-menu>))
  (slot-set! obj 'gtk-menu-file-quit (make <gtk-menu-item> #:label "Quit"))
  (slot-set! obj 'gtk-menu-item-mode (make <gtk-menu-item> #:label "Mode"))
  (slot-set! obj 'gtk-menu-mode      (make <gtk-menu>))
  (slot-set! obj 'gtk-menu-mode-raw  (gtk-radio-menu-item-new-with-label #f "Raw"))
  (slot-set! obj 'gtk-menu-mode-diff (gtk-radio-menu-item-new-with-label
                                      (gtk-radio-menu-item-get-group
                                       (gtk-menu-mode-raw obj))
                                      "Diff"))

  (append (gtk-menu-bar obj) (gtk-menu-item-file obj))
  (set-submenu (gtk-menu-item-file obj) (gtk-menu-file obj))
  (append (gtk-menu-file obj) (gtk-menu-file-quit obj))
  (append (gtk-menu-bar obj) (gtk-menu-item-mode obj))

  (set-submenu (gtk-menu-item-mode obj) (gtk-menu-mode obj))
  (append      (gtk-menu-mode      obj) (gtk-menu-mode-raw obj))

  (gtk-menu-item-activate (gtk-menu-mode-raw obj))
  (append (gtk-menu-mode obj) (gtk-menu-mode-diff obj))

  ;; Pack the Menu bar
  (pack-start (gtk-main-vbox obj) (gtk-menu-bar obj) #f #f 0)

  ;;
  ;; Main paned
  ;;
  (slot-set! obj 'gtk-main-paned (make <gtk-hpaned> #:position 300))
  (pack-end (gtk-main-vbox obj) (gtk-main-paned obj) #t #t 0)


  ;;
  ;;   -----------------------------------------
  ;;  | Main paned                              |
  ;;  |  ----------------  :  ----------------  |
  ;;  | | Left vbox      | : | Right vbox     | |
  ;;  | |                | : |                | |
  ;;
  ;; Left vbox
  (slot-set! obj 'gtk-left-vbox (make <gtk-vbox> #:homogeneous #f #:spacing 0))
  (gtk-paned-add1 (gtk-main-paned obj) (gtk-left-vbox obj))
  ;; Right vbox
  (slot-set! obj 'gtk-right-vbox (make <gtk-vbox> #:homogeneous #f #:spacing 0))
  (gtk-paned-add2 (gtk-main-paned obj) (gtk-right-vbox obj))


  ;;
  ;;   ---------------------
  ;;  | Left vbox
  ;;  |  -------------------
  ;;  | | Host tree
  ;;  | |
  ;;  | |-------------------
  ;;  | | Button hbox
  ;;  | |
  ;;  |  -------------------
  ;;   ---------------------
  ;;
  (pack-start (gtk-left-vbox obj) (get-host-tree obj) #t #t 0)

  ;;
  ;; Button hbox
  ;;

  (slot-set! obj 'gtk-button-hbox
             (make <gtk-hbox>
               #:homogeneous #f
               #:spacing 0))

  (slot-set! obj 'gtk-add-host-button
             (make <gtk-button>
               #:label "Add host"))

  (slot-set! obj 'gtk-rem-host-button
             (make <gtk-button>
               #:label "Remove host"))

  ;; Pack buttons
  (pack-start (gtk-button-hbox obj) (gtk-add-host-button obj) #t #t 0)
  (pack-end   (gtk-button-hbox obj) (gtk-rem-host-button obj) #t #t 0)

  ;; Pack the Button hbox
  (pack-end (gtk-left-vbox obj) (gtk-button-hbox obj) #f #f 0)


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
  (slot-set! obj 'gtk-scrolled-window
             (make <gtk-scrolled-window>
               #:hscrollbar-policy 'automatic
               #:vscrollbar-policy 'automatic))
  (slot-set! obj 'gtk-entry
             (make <gtk-entry>))

  (pack-start (gtk-right-vbox obj) (gtk-scrolled-window obj) #t #t 0)
  (pack-end   (gtk-right-vbox obj) (gtk-entry obj) #f #f 0)


  ;;
  ;;   ---------------------
  ;;  | Scrolled window
  ;;  |  -------------------
  ;;  | | Output view
  ;;  | |
  ;;
  (gtk-container-add (gtk-scrolled-window obj) (get-output-view obj))


  ;;
  ;; Popup menu:
  ;;
  ;;   ---------------------
  ;;  | Set as a master host
  ;;  |=====================
  ;;  | Create group
  ;;  |---------------------
  ;;  | Remove group
  ;;   ---------------------
  ;;
  (slot-set! obj 'gtk-host-tree-menu
             (make <gtk-menu>))
  (slot-set! obj 'gtk-host-tree-menu-set-master
             (make <gtk-menu-item>
               #:label "Set as a master host"))
  (slot-set! obj 'gtk-host-tree-menu-create-group
             (make <gtk-menu-item>
               #:label "Create group"))
  (slot-set! obj 'gtk-host-tree-menu-remove-group
             (make <gtk-menu-item>
               #:label "Remove group"))

  (let ((menu (gtk-host-tree-menu obj)))
    (append menu (gtk-host-tree-menu-set-master obj))
    (append menu (make <gtk-separator-menu-item>))
    (append menu (gtk-host-tree-menu-create-group obj))
    (append menu (gtk-host-tree-menu-remove-group obj)))

  (show-all (gtk-host-tree-menu obj))


  ;;
  ;; Set up handlers for signals
  ;;

  (gtype-instance-signal-connect       (gtk-main-window obj) 'delete-event
                                       (lambda (w e) (handle-lazycat-quit obj) #f))
  (gtype-instance-signal-connect-after (gtk-menu-file-quit obj) 'activate
                                       (lambda (w) (handle-lazycat-quit obj)))
  (gtype-instance-signal-connect-after (gtk-menu-mode-raw  obj) 'activate
                                       (lambda (w) (set-mode obj *mode-raw*)))
  (gtype-instance-signal-connect-after (gtk-menu-mode-diff obj) 'activate
                                       (lambda (w) (set-mode obj *mode-diff*)))

  (gtype-instance-signal-connect-after (gtk-entry obj) 'activate
                                       (lambda (e) (handle-send-message obj)))

  (gtype-instance-signal-connect-after (gtk-host-tree-menu-set-master obj) 'activate
                                       (lambda (e) (handle-set-master obj)))
  (gtype-instance-signal-connect-after (gtk-add-host-button obj) 'clicked
                                       (lambda (e) (handle-add-host obj)))
  (gtype-instance-signal-connect-after (gtk-rem-host-button obj) 'clicked
                                       (lambda (e) (handle-rem-host obj)))

  (connect (get-host-tree obj) 'button-press-event
           (lambda (w e)
             (handle-host-tree-button-press-event obj (gdk-event-button:button e))))

  (connect (gtk-host-tree-menu-set-master obj)   'activate
           (lambda (w) (handle-set-master obj)))
  (connect (gtk-host-tree-menu-create-group obj) 'activate
           (lambda (w) (handle-add-group obj)))
  (connect (gtk-host-tree-menu-remove-group obj) 'activate
           (lambda (w) (handle-rem-group obj))))


;;; Handlers for various events.
;; These methods take just one parameter and this parameter is an instance
;; of <lazy-gtk-cat>. Most of usefull work is done by other methods that
;; handlers are calling.

;; Handler for "Set as a master" popup menu item.
(define-method (handle-set-master (obj <lazy-gtk-cat>))
  (let* ((host-tree (get-host-tree obj))
         (host-id   (host-tree-get-selected host-tree)))
    ;; Check that the selected row is a host.
    (if (not (eq? host-id #f))
        (lazycat-set-master-host obj host-id))))

;; This method makes a dialog window to ask the user a group name and
;; add a new group to host-list and host-tree.
;;
;; TODO: Should we move this dialog to its own class?
(define-method (handle-add-group (obj <lazy-gtk-cat>))
  (let* ((dialog        (make <gtk-dialog> #:title "New group"))
         (entry         (make <gtk-entry>))
         (cancel-button (gtk-dialog-add-button dialog "Cancel" 0))
         (ok-button     (gtk-dialog-add-button dialog "OK"     1)))

    (gtk-box-pack-start (get-vbox dialog) entry #f #f 0)

    (connect ok-button 'clicked
             (lambda (w) (let ((group-name (gtk-entry-get-text entry)))
                           (host-tree-add-group (get-host-tree obj) group-name)
                           (destroy dialog))))

    (connect cancel-button 'clicked (lambda (w) (destroy dialog)))

    (show-all dialog)))

(define-method (handle-rem-group (obj <lazy-gtk-cat>))
  (host-tree-rem-group (get-host-tree obj)))

(define-method (handle-rem-host (obj <lazy-gtk-cat>))
  (let* ((host-tree (get-host-tree obj))
         (host-id   (host-tree-get-selected host-tree)))
    (lazycat-remove-host obj host-id)))

(define-method (handle-add-host (obj <lazy-gtk-cat>))
  (let ((dialog     (add-host-dialog obj))
        (handler-id #f))

    (define (handle-dialog-response rsp)
      (if (= rsp 1)
          ;; Get attributes for a new host from dialog
          (let* ((group      (get-group            dialog))
                 (name       (get-host-name        dialog))
                 (proxy      (get-proxy-name       dialog))
                 (address    (get-address          dialog))
                 (descripion (get-host-description dialog))
                 (host-attributes (list name proxy address descripion)))
            ;; Add the host
            ;;
            ;; TODO: Should we do the check for an empty string passed
            ;;       as a group name here -- or somewhere else?
            (lazycat-add-host obj (if (eq? (string-length group) 0) #f group)
                              host-attributes)))
      (gsignal-handler-disconnect dialog handler-id))

    (set! handler-id
          (gtype-instance-signal-connect dialog 'response
                                         (lambda (w rsp)
                                           (handle-dialog-response rsp))))
    (show-all dialog)))

;; Handler for message sending.
(define-method (handle-send-message (obj <lazy-gtk-cat>))
  (let ((entry (gtk-entry obj)))
    (lazycat-send-message obj (gtk-entry-get-text entry))))

(define-method (handle-host-tree-button-press-event (obj           <lazy-gtk-cat>)
                                                    (button-number <number>))
  (cond
   ((= button-number 3)
    (let ((menu (gtk-host-tree-menu obj)))
      (if (not (eq? (host-tree-get-selected (get-host-tree obj)) #f))
          (begin
            (gtk-widget-set-sensitive (gtk-host-tree-menu-remove-group obj) #f)
            (gtk-widget-set-sensitive (gtk-host-tree-menu-set-master obj)   #t))
          (begin
            (gtk-widget-set-sensitive (gtk-host-tree-menu-remove-group obj) #t)
            (gtk-widget-set-sensitive (gtk-host-tree-menu-set-master obj)   #f)))
      (gtk-menu-popup menu
                      #f   ; parent-menu-shell
                      #f   ; parent-menu-item
                      #f   ; func
                      3    ; button
                      0)   ; activate-time
      #t))
   (else #f)))

;; Cancel the application
(define-method (handle-lazycat-quit (obj <lazy-gtk-cat>))
  (begin
    (gtk-main-quit)
    #f))


;;; Methods that do most of the hard work.

;; Send message to the server
(define-method (send-message (obj <lazy-gtk-cat>) (type <number>) message)
  (set-server-socket obj (socket PF_UNIX SOCK_STREAM 0))
  (connect (get-server-socket obj) AF_UNIX (get-server-socket-path obj))
  (let ((message     (list type message))
        (server-socket (get-server-socket obj)))

    ;; Send the message
    (write message server-socket)
    (newline server-socket)

    ;; Receive a response
    (let ((output (let r ((output "")
                          (line   ""))
                    (if (not (eof-object? line))
                        (r (string-append output line "\n")
                           (read-line server-socket 'trim))
                        output))))
      (close-port server-socket)
      (read (open-input-string output)))))

;; Refresh list of hosts
(define-method (refresh-host-list (obj <lazy-gtk-cat>))
  (let ((group-list (cadr (send-message obj *cmd-list* 'hosts)))
        (host-tree  (get-host-tree obj)))

    (host-tree-clear host-tree)

    (for-each
     (lambda (group)
       (for-each
        (lambda (host)
          (let ((group-name (car group))
                (host-attributes (list (list-ref host 0)    ; ID
                                       (list-ref host 1)    ; Name
                                       (list-ref host 2)    ; Proxy
                                       (list-ref host 3)    ; Address
                                       (list-ref host 4)))) ; Description
            (host-tree-add-host host-tree group-name host-attributes)))
        (cdr group)))
     group-list)

    (lazycat-get-master-host obj)))

;; Add host to the GTK host three and to the host list
;;
;; This function takes host attributes as the following list:
;;   '(name proxy address description)
(define-method (lazycat-add-host (obj <lazy-gtk-cat>) group (host-attributes <list>))
  (send-message obj *cmd-add-host* (cons group host-attributes))
  (refresh-host-list obj))

;; Remove host
(define-method (lazycat-remove-host (obj <lazy-gtk-cat>) (host-id <number>))
  (let ((host-tree (get-host-tree obj)))
    (host-tree-rem-host host-tree host-id)
    (send-message obj *cmd-rem-host* host-id)))

;; Set host with HOST-ID as a master host.
(define-method (lazycat-set-master-host (obj <lazy-gtk-cat>) (host-id <number>))
  (send-message obj *cmd-set* (list 'master (number->string host-id)))
  (set-master-host-id obj host-id)
  (host-tree-set-master-host (get-host-tree obj) host-id))

(define-method (lazycat-get-master-host (obj <lazy-gtk-cat>))
  (let* ((result  (send-message obj *cmd-get* 'master))
         (host-id (string->number (cadr result))))
    (set-master-host-id obj host-id)
    (host-tree-set-master-host (get-host-tree obj) host-id)))

;; Send a message
(define-method (lazycat-send-message (obj <lazy-gtk-cat>) (message <string>))
  (let ((handler-id #f))
    (if (string=? (get-mode obj) *mode-raw*)

        ;; Raw mode. Just send the message to all hosts and print responses.
        (call-with-new-thread
         (lambda ()
           (begin
             (gdk-threads-enter)
             (gtk-widget-set-sensitive (gtk-entry obj) #f)
             (gdk-threads-leave)

             (let ((output-view (get-output-view obj))
                   (result      (send-message obj *cmd-exec* message)))
               (gdk-threads-enter)
               (output-view-format-list output-view (cadr result))
               (gdk-threads-leave))

             (gdk-threads-enter)
             (gtk-widget-set-sensitive (gtk-entry obj) #t)
             (gtk-widget-grab-focus (gtk-entry obj))
             (gdk-threads-leave))))

        ;; Diff mode. An output from a master host will be taken as a
        ;; pattern. Then we will compare the output from every host
        ;; with the pattern.
        (let* ((output-preview-dialog (output-preview-dialog obj)))

          ;; Handler for a dialog response
          (define (handle-dialog-response rsp)
            (if (= rsp 1)
                ;; Execute
                (call-with-new-thread
                 (lambda ()
                   (begin
                     (gdk-threads-enter)
                     (gtk-widget-set-sensitive (gtk-entry obj) #f)
                     (gdk-threads-leave)

                     (let ((output-view (get-output-view obj))
                           (result      (send-message obj *cmd-diff*
                                                      (list 'continue))))
                       (gdk-threads-enter)
                       (output-view-format-diff output-view result)
                       (gdk-threads-leave))

                     (gdk-threads-enter)
                     (gtk-widget-set-sensitive (gtk-entry obj) #t)
                     (gtk-widget-grab-focus (gtk-entry obj))
                     (gdk-threads-leave))))

                ;; Cancel
                (let ((output-view (get-output-view obj)))
                  (send-message obj *cmd-diff* (list 'abort))
                  (output-view-append output-view "Canceled by user." "")))
            ;; Disconnect the connected handler
            (gsignal-handler-disconnect output-preview-dialog handler-id))

          ;; Connect the handler to a signal
          (set! handler-id
                (gtype-instance-signal-connect output-preview-dialog
                                               'response
                                               (lambda (w rsp)
                                                 (handle-dialog-response rsp))))
          ;; Show an output preview
          (let* ((message (list 'get-pattern message))
                 (result  (send-message obj *cmd-diff* message))
                 (pattern (cadr (cadadr result)))) ; Abra'cadadr'a!
            (show-output output-preview-dialog pattern))))

    ;; Clean up a previous content of the command line
    (gtk-entry-set-text (gtk-entry obj) "")))

;; Run the application.
(define-method (run (obj <lazy-gtk-cat>) args)
  (show-all (gtk-main-window obj))
  (refresh-host-list obj)
  (gdk-threads-enter)
  (gtk-main)
  (gdk-threads-leave))

;;; lazy-gtk-cat.scm ends here

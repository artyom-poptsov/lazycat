;;; curiosity.scm -- LazyCat's Curiosity

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;
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

;; Explore the network an collect information about hosts.
;; 
;; These methods are exported:
;; 
;;   curiosity
;;   curiosity-get-pid


;;; Code:

(define-module (lazycat daemon curiosity)
  ;; Logging
  #:use-module (logging logger)

  #:use-module (lazycat daemon host)
  #:use-module (lazycat daemon host-list)
  #:use-module (lazycat daemon proxy-list)
  #:use-module (lazycat proxy)
  #:use-module (lazycat message)
  #:use-module (lazycat protocol)
  #:use-module (lazycat prctl)
  #:use-module (oop goops)
  #:export (curiosity
            curiosity-get-pid))


;;; Constants

(define %curiosity-thread-prio 15)
(define %lsb-release-cmd "lsb_release -a")

;;;

(define pid #f)


(define (lsb->alist lsb)
  "Convert lsb_release output LSB to an alist"
  (map
   (lambda (str)
     (let* ((lst (string-split str #\:)))
       (if (not (null? (cdr lst)))
           (let ((key (car lst))
                 (val (string-drop (cadr lst) 1)))
             (cons key val))
           #f)))
   (filter (lambda (str) (not (string-null? str)))
           (string-split lsb #\newline))))

(define (curiosity-get-pid)
  "Get PID of the Curiosity thread"
  pid)

(define (set-host-lsb lsb host)
  "Set LSB information for the host HOST."
  (define (set-if-not-#f attr lsb-rec)
    (let ((rec-val (assoc-ref lsb lsb-rec)))
      (and rec-val (host-set-attr! host attr rec-val))))
  (set-if-not-#f "lsb/version"        "LSB Version")
  (set-if-not-#f "lsb/distributor-id" "Distributor ID")
  (set-if-not-#f "lsb/description"    "Description")
  (set-if-not-#f "lsb/release"        "Release")
  (set-if-not-#f "lsb/codename"       "Codename"))


(define (curiosity)
  "Explore network and store information about hosts in the LazyCat
host list."
  (setpriority PRIO_PROCESS 0 %curiosity-thread-prio)
  (set! pid (getpid))
  (prctl-pr-set-name! "curiosity")
  (while #t
    (map
     (lambda (host)
       (let ((status (host-get-status host)))
         (if (eq? status 'online)
             (catch 'proxy-error
               (lambda ()
                 (let* ((host-proxies (host-get-proxy-list host))
                        (host-addr    (host-get-address host))
                        (proxy        (proxy-list-get-proxy (car host-proxies)))
                        (msg-rsp      (proxy-send-message proxy host-addr %lsb-release-cmd)))
                   (if (not (message-error? msg-rsp))
                       (let ((lsb-info (lsb->alist (message-field-ref msg-rsp 'response))))
                         (set-host-lsb lsb-info host)))))
               (lambda (key . args)
                 (log-msg 'WARNING (object->string args)))))))

     (host-list-get-plain-list))
    (sleep 10)))

;;; curiosity.scm ends here.

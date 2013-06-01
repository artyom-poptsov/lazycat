;;; lctcpd.scm -- TCP server for LazyCat.

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; A simply TCP server that is used by LazyCat to control a host.


;;; Code:

(define-module (lazycat host lctcpd)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 getopt-long))

(define *program-name* "lctcpd")

(define *default-bind-addr* "127.0.0.1")
(define *default-port*      50001)

(define *option-spec*
  '((bind-addr (single-char #\b) (value #t))
    (port      (single-char #\p) (value #t))
    (help      (single-char #\h) (value #f))))


;; Open a socket.
;;
;; Return new socket.
(define (open-socket address port)
  (let ((server-socket (socket PF_INET SOCK_STREAM 0)))

    (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
    
    (bind server-socket AF_INET (inet-aton address) port)
    (listen server-socket 1)

    server-socket))

(define (print-help)
  (display
   (string-append
    "Usage: " *program-name* " [-p port] [-b address] [-h]\n"
    "\t -p -- Port number (default: " (string->number *default-port*) ")\n"
    "\t -b -- Bind to address (default: " *default-bind-addr* ")\n"
    "\t -h -- Display this help and exit\n")))

;; Wrapper for accept() that catch errors.
(define (accept-and-catch socket)
  (catch 'system-error
    (lambda ()
      (accept socket))
    (lambda (key . args)
      #f)))


;; Main loop of the process
(define (main-loop server-socket)
  (while #t
    (let* ((client-connection (accept-and-catch server-socket))
           (client            (car client-connection)))

      (let* ((command (read-line client))
             (port (open-input-pipe command))
             (result ""))

        (let read-next-line ((line (read-line port 'concat)))
          (if (not (eof-object? line))
              (begin
                (set! result (string-append result line))
                (read-next-line (read-line port 'concat)))
              (begin
                (close-input-port port))))

        (display result client)
        (newline client))

      (close client))))


;;; Entry point of the program

(define (main args)
  (let* ((options      (getopt-long args *option-spec*))
         (bind-addr    (option-ref options 'bind-addr *default-bind-addr*))
         (port-num     (option-ref options 'port      *default-port*))
         (help-needed? (option-ref options 'help      #f)))

    (if help-needed?
        (begin
          (print-help)
          (quit)))

    (let ((socket (open-socket bind-addr port-num))
          (pid (primitive-fork)))

      (cond
       ((= pid 0)
        (close-port (current-input-port))
        (close-port (current-output-port))
        (close-port (current-error-port))
        (setsid)
        (main-loop socket))

       ((> pid 0)
        (quit))

       ((< pid 0)
        (display "ERROR: Couldn't spawn a process.\n"))))))

;;; lctcpd.scm ends here

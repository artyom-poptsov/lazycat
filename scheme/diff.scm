;;; diff.scm -- Get diff between two files or strings.

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

;; This module describes a <diff> class. The class represents a diff
;; between two files. Also a convinient method make-string-diff is
;; provided for making a diff between two strings.
;;
;; This module exports:
;;
;;   (make-string-diff tmp-dir pattern output)
;;   (diff-empty? diff)
;;   (diff-get diff)


;;; Code:

(define-module (lazycat diff)
  #:use-module (oop goops)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export     (<diff> make-string-diff diff-empty? diff-get))

(define *tmp-file-name* "diff-XXXXXX")


;;; Main class
(define-class <diff> ()
  (file1       #:accessor file1 #:init-keyword #:file1)
  (file2       #:accessor file2 #:init-keyword #:file2)
  (diff-result #:accessor diff-result #:init-value ""))

;; Class initialization
(define-method (initialize (obj <diff>) args)
  (next-method)
  (calculate-diff obj))


;;; Public methods

;; Function returns a new <diff> instance based on comparsion strings PATTERN
;; and OUTPUT.
(define-method (make-string-diff (tmp-dir <string>)
                                 (pattern <string>)
                                 (output  <string>))
  (if (not (file-exists? tmp-dir))
      (mkdir tmp-dir #o700))

  (let* ((tmp-port1 (mkstemp!
                     (string-copy (string-append tmp-dir "/" *tmp-file-name*))))
         (tmp-port2 (mkstemp!
                     (string-copy (string-append tmp-dir "/" *tmp-file-name*))))
         (tmp-file1 (port-filename tmp-port1))
         (tmp-file2 (port-filename tmp-port2)))

    (close-output-port tmp-port1)
    (close-output-port tmp-port2)

    (with-output-to-file tmp-file1
      (lambda () (begin (write pattern) (newline))))
    (with-output-to-file tmp-file2
      (lambda () (begin (write output)  (newline))))

    (let ((diff (make <diff> #:file1 tmp-file1 #:file2 tmp-file2)))
      (delete-file tmp-file1)
      (delete-file tmp-file2)
      diff)))

;; This predicate checks that given files are different.
(define-method (diff-empty? (obj <diff>))
  (eqv? (string-length (diff-result obj)) 0))

;; Get a diff between given files.
(define-method (diff-get (obj <diff>))
  (diff-result obj))


;;; Private methods

;; This method calculates diff for given files.
(define-method (calculate-diff (obj <diff>))
  (let ((port (open-input-pipe
               (string-append "diff " (file1 obj) " " (file2 obj))))
        (diff (diff-result obj)))
    (let read-next-line ((line (read-line port 'concat)))
      (if (not (eof-object? line))
          (begin
            (set! diff (string-append diff line))
            (read-next-line (read-line port 'concat)))
          (begin
            (close-input-port port)
            (slot-set! obj 'diff-result diff))))))

;;; diff.scm ends here

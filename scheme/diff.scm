;;;; diff.scm -- Get diff between two files or strings.
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


;;; Module definition

(define-module (lazycat diff)
  #:use-module (oop goops)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export     (<diff> make-string-diff diff-empty? diff-get))

;;; Constants

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
  ;; Create a directory for temporary files if it doesn't exist.
  (if (not (file-exists? tmp-dir))
      (mkdir tmp-dir #o700))

  ;; Make a diff. First of all, create two temporary files for storing strings.
  (let* ((tmp-port1 (mkstemp!
                     (string-copy (string-append tmp-dir "/" *tmp-file-name*))))
         (tmp-port2 (mkstemp!
                     (string-copy (string-append tmp-dir "/" *tmp-file-name*))))
         ;; Get file names
         (tmp-file1 (port-filename tmp-port1))
         (tmp-file2 (port-filename tmp-port2)))

    ;; Close temporary output ports because we don't need them anymore.
    (close-output-port tmp-port1)
    (close-output-port tmp-port2)

    ;; Write strings to temporary files
    (with-output-to-file tmp-file1
      (lambda () (begin (write pattern) (newline))))
    (with-output-to-file tmp-file2
      (lambda () (begin (write output)  (newline))))

    ;; Make a diff between files
    (let ((diff (make <diff> #:file1 tmp-file1 #:file2 tmp-file2)))
      ;; Delete temporary files
      (delete-file tmp-file1)
      (delete-file tmp-file2)
      ;; Return the diff
      diff)))

;; This predicate checks that given files are different.
(define-method (diff-empty? (obj <diff>))
  (eqv? (string-length (diff-result obj)) 0))

;; Get a diff between given files.
(define-method (diff-get (obj <diff>))
  (diff-result obj))


;;; Private methods

(define-method (calculate-diff (obj <diff>))
  (let ((port (open-input-pipe
               (string-append "diff " (file1 obj) " " (file2 obj))))
        (diff (diff-result obj)))
    (let read-next-line ((line (read-line port 'concat)))
      (if (not (eof-object? line))
          ;; Store current line and read the next line.
          (begin
            (set! diff (string-append diff line))
            (read-next-line (read-line port 'concat)))
          ;; EOF reached.
          (begin
            (close-input-port port)
            ;; Store the diff result.
            (slot-set! obj 'diff-result diff))))))

;;;; EOF

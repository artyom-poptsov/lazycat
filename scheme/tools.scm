;;;; Wrappers for various Unix tools.
;;;;
;;;; Copyright (C) 2012 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (lazycat tools)
  #:use-module (ice-9 rdelim)
  #:export (touch diff sdiff))


;;; Functions

(define (mkdir dir)
  "Wrapper for the mkdir tool."
  (system (string-append "mkdir " dir)))

(define (touch . files)
  "Wrapper for the touch tool."
  (let ((arg ""))
    (for-each (lambda (f) (set! arg (string-append arg " " f))) files)
    (system (string-append "touch " arg))))

(define (diff file1 file2 diff-file)
  "Wrapper for the diff tool."
  (let ((diff-in   #f)
        (result    ""))

    ;; Execute diff on files
    (system (string-append "diff " file1 " " file2 " > " diff-file))

    (set! diff-in (open-input-file diff-file))

    ;; Read diff result from the diff-file, line by line, and store it as
    ;; a string.
    ;;
    ;; Probably this is not the better way to load a whole text file into the
    ;; scheme string, but at least it works better than recursive reading
    ;; (it caused stack overflow on the big files).
    (let f ((str (read-line diff-in 'concat)))
      (if (not (eof-object? str))
          (begin
            (set! result (string-append result str))
            (f (read-line diff-in 'concat)))
          ;; There is nothing to read anymore. Return the result.
          result))))

(define (sdiff tmp-dir pattern output)
  "Function returns diff between strings PATTERN and OUTPUT"
  (let* ((tmp-file1 (string-append tmp-dir "/tmp1"))
         (tmp-file2 (string-append tmp-dir "/tmp2"))
         (diff-file (string-append tmp-dir "/diff")))

    ;; Create needed files
    (touch tmp-file1 tmp-file2 diff-file)

    (with-output-to-file tmp-file1
      (lambda ()
        (write pattern)))
    
    (with-output-to-file tmp-file2
      (lambda ()
        (write output)))
    
    (diff tmp-file1 tmp-file2 diff-file)))

;;;; EOF

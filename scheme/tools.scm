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

(define (touch . files)
  "Wrapper for touch tool."
  (let ((arg ""))
    (for-each (lambda (f) (set! arg (string-append arg " " f))) files)
    (system (string-append "touch " arg))))

(define (diff file1 file2 diff-file)
  "Wrapper for diff tool."
  (let ((diff-in   #f)
        (ret       ""))

    ;; Execute diff on files
    (system (string-append "diff " file1 " " file2 " > " diff-file))

    (set! diff-in (open-input-file diff-file))

    ;; Read diff result from file
    (let f ((ret (read-char diff-out)))
      (if (eof-object? ret)
          ""
          (string-append (string ret) (f (read-char diff-out)))))))

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
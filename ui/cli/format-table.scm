;;; format-table.scm -- A simply table formatter

;; Copyright (C) 2013 Artyom Poptsov <poptsov.artyom@gmail.com>
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

;; Procedures for table formatting.  Basically it works like usual
;; 'format' procedure from (ice-9 format), but allows to fit long
;; lines in given column length.  The length should be specified by
;; format string.
;;
;; Syntax: format-table dest fmt rows-list
;;
;; Note that in the current implementation rows must be equal in
;; length, otherwise the error will be raised.
;;
;; Example:
;;
;;   (format-table #t "~15a  ~15a~%" '(("First row, first column" "First row, second column")
;;                                     ("Second row, first column" "Second row, second column")))
;;   => First row, firs  First row, seco
;;      t column         nd column
;;      Second row, fir  Second row, sec
;;      st column        ond column
;;
;; Don't forget to put the newline character at the end of a format
;; string, so each row will be placed on the new line.


;;; Code:

(define-module (lazycat ui cli format-table)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 common-list)
  #:export (format-table))

(define (get-padding-from-fmt fmt)
  "Get padding value from a format string FMT."
  (let ((p (string-match ".*~([0-9]+)[a-z].*" fmt)))
    (and (regexp-match? p)
         (string->number (match:substring p 1)))))

(define (equal-length? lst1 lst2)
  "Compare length of two lists."
  (eq? (length lst1) (length lst2)))

(define (cut padding-list string-list)
  "Cut off every element of the STRING-LIST according to the related
PADDING-LIST value.  Take the first part of the every string that is
fit to the given boundaries.

Lists are must be equal in length, otherwise the error will be raised.

Example:
  (cut '(5 12) '(\"Hello Scheme World!\" \"Hello Scheme World!\"))
  => '(\"Hello\" \"Hello Scheme\")

Return the list of processed strings."
  (or (equal-length? padding-list string-list)
      (error "Lists are differ in length"
             (length padding-list) (length string-list)))

  (let ((idx 0))
    (map-in-order (lambda (str)
                    (let* ((pad (list-ref padding-list idx))
                           (len (string-length str))
                           (end (if (< len pad) len pad))
                           (sub (substring str 0 end)))
                      (set! idx (1+ idx))
                      sub))
                  string-list)))

(define (rest padding-list string-list)
  "Cut off every element of the STRING-LIST according to the related
PADDING-LIST value.  Take the rest of the every string that is beyond
the given boundaries.

Lists are must be equal in length, otherwise the error will be raised.

Example:
  (rest '(5 12) '(\"Hello Scheme World!\" \"Hello Scheme World!\"))
  => (\" Scheme World!\" \" World!\")

Retrun the list of processed strings."
  (or (equal-length? padding-list string-list)
      (error "Lists are differ in length"
             (length padding-list) (length string-list)))

  (let ((idx 0))
    (map-in-order (lambda (str)
                    (let* ((pad   (list-ref padding-list idx))
                           (len   (string-length str))
                           (start (if (< len pad) 0 pad))
                           (end   (if (< len pad) 0 len))
                           (sub   (substring str start end)))
                      (set! idx (1+ idx))
                      sub))
                  string-list)))


(define (format-table dest fmt rows)
  "Write table specified by the FMT string to DEST.  ROWS is a list of
actual table rows, each row in turn is a list of values for each
column.  All rows must have the same number of columns."
  (let* ((columns (string-split fmt #\space))
         (columns (remove-if string-null? columns))
         (padding (map (lambda (fmt) (get-padding-from-fmt fmt)) columns)))
    (for-each (lambda (row)
                (let print ((r row))
                  (or (null? (filter (lambda (col) (not (string-null? col))) r))
                      (begin
                        (apply format dest fmt (cut padding r))
                        (print (rest padding r))))))
              rows)))

;;; format-table.scm ends here

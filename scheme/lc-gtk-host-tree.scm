;;;; LazyCat GTK host tree.
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

(define-module (lazycat lc-gtk-host-tree)
  #:use-module (oop goops)
  #:use-module (gnome-2)
  #:use-module (gnome gobject)
  #:use-module (gnome gtk)
  #:use-module (gnome gtk gdk-event)
  #:use-module (ice-9 receive)
  #:export (<lc-gtk-host-tree> lc-gtk-host-tree-add-host
                               lc-gtk-host-tree-rem-host
                               lc-gtk-host-tree-get-selected
                               lc-gtk-host-tree-get-selected-group))

;; Column numbers.
;;
;; TODO: What if someone change order of the columns? There is should
;;       be more flexible method for getting column numbers.
(define *column-id-number*          0)
(define *column-name-number*        1)
(define *column-description-number* 2)

;; Main class
(define-class <lc-gtk-host-tree> (<gtk-tree-view>))

;; Class initialization
(define-method (initialize (obj <lc-gtk-host-tree>) args)
  (next-method)

  (let ((tree-store (gtk-tree-store-new (list <gchararray>
                                              <gchararray>
                                              <gchararray>)))
        (column-id   (make <gtk-tree-view-column>
                       #:title "ID"))
        (column-name (make <gtk-tree-view-column>
                       #:resizable #f
                       #:title "Name"))
        (column-desc (make <gtk-tree-view-column>
                       #:resizable #f
                       #:title "Description"))
        (text-renderer (gtk-cell-renderer-text-new)))

    (set-model obj tree-store)

    (gtk-tree-view-column-pack-start column-id   text-renderer #t)
    (gtk-tree-view-column-pack-start column-name text-renderer #t)
    (gtk-tree-view-column-pack-end   column-desc text-renderer #t)

    (gtk-tree-view-column-add-attribute column-id text-renderer "text"
                                        *column-id-number*)
    (gtk-tree-view-column-add-attribute column-name text-renderer "text"
                                        *column-name-number*)
    (gtk-tree-view-column-add-attribute column-desc text-renderer "text"
                                        *column-description-number*)

    (gtk-tree-view-append-column obj column-id
                                 *column-id-number*)
    (gtk-tree-view-append-column obj column-name
                                 *column-name-number*)
    (gtk-tree-view-append-column obj column-desc
                                 *column-description-number*)))

;;
;; Public methods
;;

;; Add a new host to the host tree.
;;
;; This function takes host attributes as the following list:
;;   '(id name proxy address description)
;;
;; TODO: Add check that the new host is not exist in the host tree.
(define-method (lc-gtk-host-tree-add-host (obj <lc-gtk-host-tree>)
                                          group-name
                                          (host-attributes <list>))

  (display "DEGUG: lc-gtk-host-tree-add-host: group-name: ") (display group-name) (newline)

  (let* ((id          (list-ref host-attributes 0))
         (name        (list-ref host-attributes 1))
         (proxy       (list-ref host-attributes 2))
         (address     (list-ref host-attributes 3))
         (description (list-ref host-attributes 4))

         (model      (get-model obj))
         (top-level  (if (eq? group-name #f) (gtk-tree-store-append model #f) #f)))

    ;; Should we place this host in a group or not?
    (if (eq? top-level #f)

        ;; Place a new host to a group
        (let* ((iter       (gtk-tree-model-get-iter model "0"))
               (group-iter (get-group obj group-name)))

          (if (eq? group-iter #f)

              ;; Group is not exists. Create a new one.
              (let ((new-group (gtk-tree-store-append model #f)))
                (gtk-tree-store-set-value model new-group *column-name-number* group-name)
                (set! top-level (gtk-tree-store-append model new-group)))

              ;; Add the host to an existing group.
              (set! top-level (gtk-tree-store-append model group-iter)))))

    (gtk-tree-store-set-value model top-level 0 (number->string id))
    (gtk-tree-store-set-value model top-level 1 name)
    (gtk-tree-store-set-value model top-level 2 description)))

;; Remove a host from the host tree.
;;
;; TODO: This function should handle list of IDs instead of one ID.
(define-method (lc-gtk-host-tree-rem-host (obj <lc-gtk-host-tree>) (host-id <number>))
  (let* ((model (gtk-tree-view-get-model obj))
         (iter  (gtk-tree-model-get-iter model "0")))
    (let f ((it iter))
      (if (not (eq? it #f))
          (let ((current-value (gtk-tree-model-get-value model it *column-id-number*)))
            (if (or (eq? current-value #f) ; Skip groups
                    (not (eq? (string->number current-value) host-id)))
                ;; Current host ID is not matching to needed ID.
                ;; Try the next row.
                (f (gtk-tree-model-iter-next model it))
                ;; Remove host from the host tree.
                (gtk-tree-store-remove model it)))
          ;; There are no more rows for searching.
          #f))))

;; Get selected hosts as list of IDs
;;
;; TODO: This method should return list of IDs of all selected hosts. This
;;       feature depends on possibility of multiple selection in the host tree,
;;       so it should be implemented before.
(define-method (lc-gtk-host-tree-get-selected (obj <lc-gtk-host-tree>))
  (let* ((selection (gtk-tree-view-get-selection obj)))
    (receive (model iter) (gtk-tree-selection-get-selected selection)
      (string->number (gtk-tree-model-get-value model iter *column-id-number*)))))

;; Get the selected group.
;;
;; TODO: This is quite silly function, so it should merged with -get-selected.
(define-method (lc-gtk-host-tree-get-selected-group (obj <lc-gtk-host-tree>))
  (let* ((selection (gtk-tree-view-get-selection obj)))
    (receive (model iter) (gtk-tree-selection-get-selected selection)
      (gtk-tree-model-get-value model iter *column-name-number*))))
;;
;; Private methods.
;;

;; Get a group by name
(define-method (get-group (obj <lc-gtk-host-tree>) (group-name <string>))
  (let* ((model (gtk-tree-view-get-model obj))
         (iter  (gtk-tree-model-get-iter model "0")))
    (let f ((it iter))
      (if (not (eq? it #f))
          ;; Compare values
          (let ((current-value (gtk-tree-model-get-value model it *column-name-number*)))
            (if (not (string=? group-name current-value))
                ;; Try the next row.
                (f (gtk-tree-model-iter-next model it))
                ;; The needed group is found. Return the group.
                it))
          ;; There are no more rows to search in.
          #f))))

;; Add a new group
(define-method (add-group (obj <lc-gtk-host-tree>) (group-name <string>))
  (let* ((model     (get-model obj))
         (top-level (gtk-tree-store-append model #f)))
    (gtk-tree-store-set-value model top-level *column-name-number* group-name)))

;;;; EOF

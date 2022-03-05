;;; pds-list.el --- Persistent list                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Valeriy Litkovskyy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Persistent list

;;; Code:

(require 'cl-lib)

(cl-defstruct (pds-list (:constructor pds-list-create))
  list)

(defun pds-list-empty ()
  (pds-list-create :list nil))

(defun pds-list-empty-p (pds-list)
  (null (pds-list-list pds-list)))

(defun pds-list-cons (element pds-list)
  (pds-list-create :list (cons element (pds-list-list pds-list))))

(defun pds-list-head (pds-list)
  (car (pds-list-list pds-list)))

(defun pds-list-tail (pds-list)
  (pds-list-create :list (cdr (pds-list-list pds-list))))

(defun pds-list-concat (pds-list-a pds-list-b)
  (if (pds-list-empty-p pds-list-a)
      pds-list-b
    (pds-list-cons (pds-list-head pds-list-a)
                   (pds-list-concat (pds-list-tail pds-list-a) pds-list-b))))

(defun pds-list-update (pds-list index element)
  (cond ((pds-list-empty-p pds-list)
         (error "Pds list is empty"))
        ((zerop index)
         (pds-list-cons element (pds-list-tail pds-list)))
        (t
         (pds-list-cons (pds-list-head pds-list)
                        (pds-list-update (pds-list-tail pds-list)
                                         (1- index)
                                         element)))))

(defun pds-list-suffixes (pds-list)
  (if (pds-list-empty-p pds-list)
      (pds-list-cons pds-list pds-list)
    (pds-list-cons pds-list (pds-list-suffixes (pds-list-tail pds-list)))))

(provide 'pds-list)
;;; pds-list.el ends here

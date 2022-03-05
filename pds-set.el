;;; pds-set.el --- Persistent set                    -*- lexical-binding: t; -*-

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

;; Persistent set

;;; Code:

(require 'cl-lib)

(cl-defstruct (pds-set (:constructor nil)
                       (:constructor pds-set-create (cmp &optional node)))
  cmp node)

(cl-defstruct (pds-set--node (:constructor nil)
                             (:constructor pds-set--node-create
                                           (data &key cmp
                                                 (left (if cmp (pds-set-empty cmp) (error "No cmp")))
                                                 (right (if cmp (pds-set-empty cmp) (error "No cmp")))
                                                 value)))
  data left right value)

(defun pds-set-empty (cmp)
  (pds-set-create cmp))

(defun pds-set-empty-p (pds-set)
  (null (pds-set-node pds-set)))

(defun pds-set-member-p (element pds-set &optional previous-data)
  (pcase-let (((cl-struct pds-set cmp node) pds-set))
    (if (pds-set-empty-p pds-set)
        (when previous-data
          (not (funcall cmp previous-data element)))
      (pcase-let (((cl-struct pds-set--node data left right) node))
        (if (funcall cmp element data)
            (pds-set-member-p element left previous-data)
          (pds-set-member-p element right data))))))

(defun pds-set-insert (element pds-set &optional value)
  (cl-labels
      ((i (tree &optional previous-data)
          (pcase-let (((cl-struct pds-set cmp node) tree))
            (if (pds-set-empty-p tree)
                (if (and previous-data (not (funcall cmp previous-data element)))
                    (throw 'pds-set-insert pds-set)
                  (pds-set-create cmp (pds-set--node-create element :cmp cmp :value value)))
              (pcase-let (((cl-struct pds-set--node data left right) node))
                (if (funcall cmp element data)
                    (pds-set-create cmp (pds-set--node-create data :left (i left previous-data) :right right))
                  (pds-set-create cmp (pds-set--node-create data :left left :right (i right data)))))))))
    (catch 'pds-set-insert
      (i pds-set))))

(defun pds-set-lookup (element pds-set &optional previous-data previous-value)
  (pcase-let (((cl-struct pds-set cmp node) pds-set))
    (if (pds-set-empty-p pds-set)
        (when (and previous-data (not (funcall cmp previous-data element)))
          previous-value)
      (pcase-let (((cl-struct pds-set--node data left right value) node))
        (if (funcall cmp element data)
            (pds-set-lookup element left previous-data previous-value)
          (pds-set-lookup element right data value))))))

(defun pds-set-complete (cmp element depth)
  (let ((set (pds-set-empty cmp)))
    (cl-loop repeat depth
             do (setq set (pds-set-create cmp (pds-set--node-create element :left set :right set))))
    set))

(defun pds-set-balanced (cmp element size)
  (cl-labels
      ((b (size)
          (cond
           ((zerop size)
            (let ((set (pds-set-empty cmp)))
              (cons set (pds-set-insert element set))))
           ((cl-oddp size)
            (pcase-let ((`(,left . ,right) (b (/ size 2))))
              (cons (pds-set-create cmp (pds-set--node-create element :left left :right left))
                    (pds-set-create cmp (pds-set--node-create element :left left :right right)))))
           ((cl-evenp size)
            (pcase-let ((`(,left . ,right) (b (1- (/ size 2)))))
              (cons (pds-set-create cmp (pds-set--node-create element :left left :right right))
                    (pds-set-create cmp (pds-set--node-create element :left right :right right))))))))
    (car (b size))))

(provide 'pds-set)
;;; pds-set.el ends here

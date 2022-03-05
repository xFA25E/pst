;;; pds-test.el --- Tests for pds          -*- lexical-binding: t; -*-

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

;; Tests for pds

;;; Code:

(require 'ert)
(require 'pds-list)
(require 'pds-set)

(ert-deftest pds-test-list-concat ()
  (let ((a (pds-list-create :list (list 1 2 3)))
        (b (pds-list-create :list (list 4 5 6))))
    (should (equal (pds-list-concat a b)
                   (pds-list-create :list (list 1 2 3 4 5 6))))))

(ert-deftest pds-test-list-update ()
  (let ((a (pds-list-create :list (list 1 2 3 4 5 6))))
    (should (equal (pds-list-update a 0 7)
                   (pds-list-create :list (list 7 2 3 4 5 6))))
    (should (equal (pds-list-update a 1 7)
                   (pds-list-create :list (list 1 7 3 4 5 6))))
    (should (equal (pds-list-update a 3 7)
                   (pds-list-create :list (list 1 2 3 7 5 6))))
    (should-error (pds-list-update a 6 7))))

(ert-deftest pds-test-list-suffixes ()
  (should (equal (pds-list-suffixes (pds-list-create :list (list 1 2 3 4)))
                 (pds-list-create
                  :list (list (pds-list-create :list (list 1 2 3 4))
                              (pds-list-create :list (list 2 3 4))
                              (pds-list-create :list (list 3 4))
                              (pds-list-create :list (list 4))
                              (pds-list-create :list (list)))))))

(ert-deftest pds-test-set-member-insert ()
  (let ((set (pds-set-insert 1 (pds-set-insert 1 (pds-set-insert 5 (pds-set-insert 3 (pds-set-empty #'<)))))))
    (should (pds-set-member-p 3 set))
    (should (pds-set-member-p 1 set))
    (should (pds-set-member-p 5 set))
    (should-not (pds-set-member-p 0 set))
    (should-not (pds-set-member-p 2 set))
    (should-not (pds-set-member-p 4 set))
    (should-not (pds-set-member-p 6 set)))

  (let ((numbers nil)
        (set (pds-set-empty #'<)))
    (cl-loop repeat 1000
             for n = (random 1000000)
             unless (pds-set-member-p n set)
             do (cl-callf2 pds-set-insert n set)
             do (push n numbers))
    (cl-loop for n in numbers
             do (should (pds-set-member-p n set)))
    (cl-loop repeat 1000
             for n = (random 1000000)
             unless (member n numbers)
             do (should-not (pds-set-member-p n set)))))

(provide 'pds-test)
;;; pds-test.el ends here

;;; expenses.el --- Record expenses                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: expense tracking

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

;; Keep record of expenses

;;; Code:
(require 'org)

(defcustom expenses-directory nil
  "Directory to save and look for the expense files."
  :type 'string
  :group 'expenses)

(defcustom expenses-category-list nil
  "List of categories for expenses."
  :type 'list
  :group 'expenses)

(setq expenses-directory "~/Dropbox/Important_Works/Different Expenses/Monthly expenses/")
(setq expenses-category-list '("Grocery" "Shopping" "Travel" "Entertainment" "Rent" "Salary" "Others"))

(defun expenses--get-file-name (date)
  "Get the name of file from the date."
  (let ((year-month (substring date 0 7)))
    (concat expenses-directory year-month "-" "expenses.org")))

(defun expenses--create-initial-file (date)
  "Create a file for a DATE with initial structure."
  (let ((file-name (expenses--get-file-name date))
	(month (format-time-string "%B" (org-time-string-to-seconds date)))
	(year (format-time-string "%Y" (org-time-string-to-seconds date))))
    (message month)
    (with-temp-buffer
      (insert (format "#+TITLE: Expenses for %s %s\n\n" month year))
      (insert "* Expenses\n")
      (insert "|--|--|--|--|\n")
      (insert "|Date |Amount | Category | Details |\n")
      (insert "|--|--|--|--|\n")
      (append-to-file (point-min) (point-max) file-name))))

(defun expenses-add-expense ()
  "Add expense."
  (interactive)
  (let* ((date (org-read-date nil nil nil "Date: "))
	 (amount (read-string "Amount: "))
	 (category (completing-read "Category: " expenses-category-list))
	 (details (read-string "Details: "))
	 (file-name (expenses--get-file-name date)))
    (when (not (file-exists-p file-name))
      (expenses--create-initial-file date))
    (with-temp-buffer
      (insert (format "|%s |%s |%s |%s |\n" date amount category details))
      (insert "|--|--|--|--|\n")
      (append-to-file (point-min) (point-max) file-name))
    (when (string-equal (completing-read "Add another expense: " '("no" "yes")) "yes")
      (expenses-add-expense))
    (with-temp-buffer
      (switch-to-buffer (find-file-noselect file-name))
      (goto-char (point-max))
      (forward-line -1)
      (org-table-align)
      (write-region (point-min) (point-max) file-name))))

(provide 'expenses)
;;; expenses.el ends here

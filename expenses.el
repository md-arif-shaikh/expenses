;;; expenses.el --- Record expenses                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: expense tracking
;; Version: 0.0.1
;; Homepage: https://github.com/md-arif-shaikh/expenses
;; URL: https://github.com/md-arif-shaikh/expenses
;; Package-Requires: ((emacs "26.1") (dash "2.19.1"))

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
(require 'dash)

(defcustom expenses-directory nil
  "Directory to save and look for the expense files."
  :type 'string
  :group 'expenses)

(defcustom expenses-category-list nil
  "List of categories for expenses."
  :type 'list
  :group 'expenses)

(defcustom expenses-currency nil
  "Currency."
  :type 'string
  :group 'expenses)

(defcustom expenses-month-names nil
  "Month names."
  :type 'list
  :group 'expenses)

(defvar expenses-color--expense "#98C379"
  "Color to indicate a expense.")
(defvar expenses-color--date "#BE5046"
  "Color to indicate a date.")
(defvar expenses-color--message "#E5C07B"
  "Color for message.")

(defface expenses-face-expense
  `((t :foreground ,expenses-color--expense
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for expense."
  :group 'expenses)

(defface expenses-face-date
  `((t :foreground ,expenses-color--date
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for date."
  :group 'expenses)

(defface expenses-face-message
  `((t :foreground ,expenses-color--message
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for message."
  :group 'expenses)

(setq expenses-directory "~/Dropbox/Important_Works/Different Expenses/Monthly expenses/")
(setq expenses-category-list '("Grocery" "Shopping" "Travel" "Subscription" "Health" "Electronics" "Entertainment" "Rent" "Salary" "Others"))
(setq expenses-currency "Rs.")
(setq expenses-month-names '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun expenses--get-file-name (date)
  "Get the name of file from the DATE."
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
      (insert "#+TBLNAME: expenses\n")
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
      (set-buffer (find-file-noselect file-name))
      (goto-char (point-max))
      (forward-line -1)
      (org-table-align)
      (write-file file-name))))

(defun expenses-view-expense ()
  "View expense."
  (interactive)
  (let* ((date (org-read-date nil nil nil "Date: "))
	 (file-name (expenses--get-file-name date)))
    (if (file-exists-p file-name)
	(find-file-other-window file-name)
      (let ((month (format-time-string "%B" (org-time-string-to-seconds date)))
	    (year (format-time-string "%Y" (org-time-string-to-seconds date))))
	(message "No expense file is found for %s %s" month year)))))

(defun expenses--goto-table-end (name)
  "Go to end of table named NAME if point is not in any table."
  (unless (org-at-table-p)
    (let ((org-babel-results-keyword "NAME"))
      (org-babel-goto-named-result name)
      (forward-line 2)
      (goto-char (org-table-end)))))

(defun expenses--get-expense-for-file (file-name &optional table-name)
  "Calculate expenses for given FILE-NAME and TABLE-NAME."
  (with-temp-buffer
    (insert-buffer-substring (find-file-noselect file-name))
    (expenses--goto-table-end (or table-name "expenses"))
    (forward-line)
    (insert "|||||\n")
    (insert "#+TBLFM: @>$2 = vsum(@2..@-1)")
    (org-table-calc-current-TBLFM)
    (forward-line -1)
    (string-trim (org-table-get-field 2))))

(defun expenses--get-expense-filtered-by-date (dates amounts date-to-filter-with)
  "Given DATES list and AMOUNTS list filter the AMOUNT list using DATE-TO-FILTER-WITH and return the sum of the filterd list."
  (cl-loop for date in dates
	   for amount in amounts
	   if (equal date date-to-filter-with)
	   collect amount into filtered-amounts
	   finally return (-sum filtered-amounts)))

(defun expenses--get-expense-for-day (date &optional table-name)
  "Calculate expenses for a DATE and TABLE-NAME."
  (let ((file-name (expenses--get-file-name date)))
    (if (file-exists-p file-name)
	(with-temp-buffer
	  (insert-buffer-substring (find-file-noselect file-name))
	  (expenses--goto-table-end (or table-name "expenses"))
	  (forward-line)
	  (insert "|||||\n")
	  (insert (format "#+TBLFM: @>$2 = '(expenses--get-expense-filtered-by-date (split-string \"@2$1..@-1$1\" \" \") (list @2$2..@-1$2) \"%s\");L" date))
	  (org-table-calc-current-TBLFM)
	  (forward-line -1)
	  (string-trim (org-table-get-field 2)))
      nil)))

(defun expenses-calc-expense-for-day (date &optional table-name)
  "Calculate expense for DATE and TABLE-NAME and show message."
  (interactive
   (list (org-read-date nil nil nil "Date: ")))
  (let* ((month (format-time-string "%B" (org-time-string-to-seconds date)))
	 (year (format-time-string "%Y" (org-time-string-to-seconds date)))
	 (day (format-time-string "%d" (org-time-string-to-seconds date)))
	 (expenses (expenses--get-expense-for-day date)))
    (if expenses
	(message (format "%s %s %s %s = %s %s"
			 (propertize "Total expenses for" 'face 'expenses-face-message)
			 (propertize month 'face 'expenses-face-date)
			 (propertize day 'face 'expenses-face-date)
			 (propertize year 'face 'expenses-face-date)
			 (or expenses-currency "")
			 (propertize expenses 'face 'expenses-face-expense)))
      (message (format "%s %s %s %s"
		       (propertize "No expense file is found for" 'face 'expenses-face-message)
		       (propertize month 'face 'expenses-face-date)
		       (propertize day 'face 'expenses-face-date)
		       (propertize year 'face 'expenses-face-date))))))

(defun expenses--get-expense-for-month (date)
  "Calculate expense for a month specified by any DATE in that month in format YYYY-MM-DD."
  (let* ((file-name (expenses--get-file-name date)))
    (if (file-exists-p file-name)
	(expenses--get-expense-for-file file-name)
      nil)))

(defun expenses-calc-expense-for-month (date)
  "Calculate expense for a month specified by any DATE in that month in format YYYY-MM-DD."
  (interactive
   (let ((date (org-read-date nil nil nil "Date: ")))
     (list date)))
  (let* ((month (format-time-string "%B" (org-time-string-to-seconds date)))
	 (year (format-time-string "%Y" (org-time-string-to-seconds date)))
	 (expenses (expenses--get-expense-for-month date)))
    (if expenses
	(message (format "%s %s %s = %s %s"
			 (propertize "Total expenses for" 'face 'expenses-face-message)
			 (propertize month 'face 'expenses-face-date)
			 (propertize year 'face 'expenses-face-date)
			 (or expenses-currency "")
			 (propertize expenses 'face 'expenses-face-expense)))
      (message (format "%s %s %s"
		       (propertize "No expense file is found for" 'face 'expenses-face-message)
		       (propertize month 'face 'expenses-face-date)
		       (propertize year 'face 'expenses-face-date))))))

(defun expenses--get-expense-for-year (year &optional start end)
  "Calculate expenses for a YEAR with optional arguments START month and END month START and END should be 1-12."
  (let* ((expenses-list (cl-loop for n in (number-sequence (or start 1) (or end 12))
				      collect (let* ((date (format "%s-%02d-01" year n))
						     (month (format-time-string "%B" (org-time-string-to-seconds date))))
						(cons month (expenses--get-expense-for-month date)))))
	 (months (mapcar 'car expenses-list))
	 (expenses (mapcar 'cdr expenses-list))
	 (total-expense (-sum (-map-when 'stringp 'string-to-number (-replace nil 0 expenses)))))
    `(("months" . ,months)
      ("expenses" . ,expenses)
      ("total" . ,total-expense))))

(defun expenses-calc-expense-for-months (year &optional start end)
  "Calculate expenses for months between START and END of a YEAR."
  (interactive
   (let* ((current-year (string-to-number (format-time-string "%Y")))
	  (picked-year (completing-read "Enter year: " (cl-loop for year-num in (number-sequence current-year (- current-year 10) -1) collect (number-to-string year-num))))
	  (picked-start (1+ (-elem-index (completing-read "Start month: " expenses-month-names) expenses-month-names)))
	  (picked-end (1+ (-elem-index (completing-read "End month: " expenses-month-names) expenses-month-names))))
     (list picked-year picked-start picked-end)))
  (let* ((expense-alist (expenses--get-expense-for-year year start end))
	 (months (cdr (assoc "months" expense-alist)))
	 (expenses (cdr (assoc "expenses" expense-alist)))
	 (total (cdr (assoc "total" expense-alist)))
	 (num-res (length months))
	 (message-strings (cl-loop for n from 0 to (1- num-res)
				  collect (let ((month (nth n months))
						(expense (nth n expenses)))
					    (format "%s = %s %s"
						    (propertize month 'face 'expenses-face-date)
						    expenses-currency
						    (if (stringp expense)
							(propertize expense 'face 'expenses-face-expense)
						      expense)))))
	 (buffer-name (concat "*Expenses: " year "*")))
    (generate-new-buffer buffer-name)
    (with-current-buffer buffer-name
      (insert (propertize "---------------------------------\n" 'face 'expenses-face-message))
      (insert (propertize (format "   Expenses for the year %s\n" year) 'face 'expenses-face-message))
      (insert (propertize "---------------------------------\n" 'face 'expenses-face-message))
      (insert (string-join message-strings "\n"))
      (insert (propertize "\n---------------------------------\n" 'face 'expenses-face-message))
      (insert (format "%s = %s %s"
		      (propertize "Total" 'face 'expenses-face-date)
		      expenses-currency
		      (propertize (number-to-string total) 'face 'expenses-face-expense)))
      (insert (propertize "\n---------------------------------\n" 'face 'expenses-face-message))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)="))
    (switch-to-buffer-other-window buffer-name)))

(defun expenses-calc-expense-for-year (year)
  "Calculate expenses in a YEAR."
  (interactive
   (let* ((current-year (string-to-number (format-time-string "%Y")))
	  (picked-year (completing-read "Enter year: " (cl-loop for year-num in (number-sequence current-year (- current-year 10) -1) collect (number-to-string year-num)))))
     (list picked-year)))
  (expenses-calc-expense-for-months year))

(provide 'expenses)
;;; expenses.el ends here

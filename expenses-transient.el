;;; expenses-transient.el --- Transient menu for expenses.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: tools, convenience
;; Homepage: https://github.com/md-arif-shaikh/expenses
;; URL: https://github.com/md-arif-shaikh/expenses

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

;; A single entry point to the package: `expenses-menu' is a transient that
;; gathers recording, reporting, importing and the dashboard.  Bind it to a
;; key of your choosing, for example
;;
;;   (global-set-key (kbd "C-c e") #'expenses-menu)
;;
;; Inside the dashboard the same menu is on `?'.

;;; Code:
(require 'transient)
(require 'expenses)
(require 'expenses-dashboard)

(defun expenses-transient--describe-directory ()
  "Return a short description of where the expense files live."
  (if expenses-directory
      (abbreviate-file-name expenses-directory)
    (propertize "unset" 'face 'transient-inapt-suffix)))

(defun expenses-transient--describe-dashboard-state ()
  "Describe what the dashboard is currently showing, if it is open."
  (let ((buffer (get-buffer "*Expenses Dashboard*")))
    (if buffer
	(with-current-buffer buffer
	  (format "%s, %s"
		  (expenses-dashboard--period-label)
		  (if (string-blank-p (or expenses-dashboard--user ""))
		      "all users"
		    expenses-dashboard--user)))
      "not open")))

;;;###autoload (autoload 'expenses-report-menu "expenses-transient" nil t)
(transient-define-prefix expenses-report-menu ()
  "Reports over recorded expenses."
  ["Totals"
   ("d" "for a day" expenses-calc-expense-for-day)
   ("m" "for a month" expenses-calc-expense-for-month)
   ("y" "for a year" expenses-calc-expense-for-year)
   ("M" "for a range of months" expenses-calc-expense-for-months)
   ("r" "for a range of dates" expenses-calc-expenses-for-date-range)]
  ["By category"
   ("c" "pick period, then categories" expenses-calc-expense-by-category)
   ("D" "day, by category" expenses-calc-expense-for-day-filtered-by-categories)
   ("N" "month, by category" expenses-calc-expense-for-month-filtered-by-categories)
   ("Y" "year, by category" expenses-calc-expense-for-year-filtered-by-categories)]
  ["Charts"
   ("p" "pie chart for a month (needs python)"
    expenses-pie-expense-for-month-filtered-by-categories)]
  ["" ("q" "back" transient-quit-one)])

;;;###autoload (autoload 'expenses-import-menu "expenses-transient" nil t)
(transient-define-prefix expenses-import-menu ()
  "Import expenses from bank statements."
  ["Import"
   ("b" "using a saved bank profile" expenses-import-expense-with-bank-profile)
   ("i" "specifying the columns by hand" expenses-import-expense)
   ("t" "dry run: preview the parsed rows" expenses-test-import-data)]
  ["Profiles"
   ("c" "customize `expenses-bank-profiles'"
    (lambda () (interactive) (customize-variable 'expenses-bank-profiles)))]
  ["" ("q" "back" transient-quit-one)])

;;;###autoload (autoload 'expenses-menu "expenses-transient" nil t)
(transient-define-prefix expenses-menu ()
  "Main menu for `expenses'."
  [:description
   (lambda ()
     (format "Expenses  (files: %s)\nDashboard: %s"
	     (expenses-transient--describe-directory)
	     (expenses-transient--describe-dashboard-state)))
   ["Record"
    ("a" "add an expense" expenses-add-expense)
    ("A" "add several expenses" expenses-add-multiple-expenses)
    ("s" "sort a month's table by date" expenses-sort-and-save-table-by-dates)
    ("b" "set a budget" expenses-set-budget)]
   ["View"
    ("v" "dashboard" expenses-dashboard)
    ("o" "open a month's org file" expenses-view-expense)
    ("O" "open the expenses directory" expenses-open-expense-directory)]]
  ["More"
   ("r" "reports" expenses-report-menu)
   ("i" "import from a bank statement" expenses-import-menu)
   ("C" "customize expenses"
    (lambda () (interactive) (customize-group 'expenses)))]
  ["" ("q" "quit" transient-quit-one)])

(provide 'expenses-transient)
;;; expenses-transient.el ends here

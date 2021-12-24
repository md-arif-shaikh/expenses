;;; expenses-utils.el --- Utilities for expenses.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Keywords: tools
;; Version: 0.0.1
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

;; Some utilities needed for the expenses package to auto assign categories
;; while exporting data from bank statements.

;;; Code:
(require 'ht)

(defvar expenses-utils-keyword-category-ht (ht ("CAFE" "Food")
					       ("HOTEL" "Food")
					       ("CHEMIST" "Health")
					       ("ZOO" "Entertainment")
					       ("MUSEUM" "Entertainment")
					       ("MOVIE" "Entertainment")
					       ("CINEMA" "Entertainment"))
  "Hash-table for keyword category.")

(defun expenses-utils-auto-assign-category (narrative)
  "Given a NARRATIVE, auto-assign a category using `expenses-utils-keyword-category-ht`."
  (let ((words (split-string narrative nil t)))
    (cl-loop for word in words
	     if (ht-get expenses-utils-keyword-category-ht (upcase word))
	     collect (ht-get expenses-utils-keyword-category-ht (upcase word)) into key
	     finally return (car key))))

(provide 'expenses-utils)
;;; expenses-utils.el ends here

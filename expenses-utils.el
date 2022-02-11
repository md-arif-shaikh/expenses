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

(defcustom expenses-utils-auto-assign-categies-on-import nil
  "Auto-assign category while importing bank statements."
  :type 'boolean
  :group 'expenses)

(defcustom expenses-utils-ignore-keywords-list nil
  "List of keywords to ignore transactions."
  :type 'list
  :group 'expenses)

(defcustom expenses-utils-ignore-phrases-list nil
  "List of phrases to ignore."
  :type 'list
  :group 'expenses)

(defcustom expenses-utils-phrases-alist nil
  "Alist of common phrases to search for to auto-assign category.
Used for importing bank statements."
  :type 'alist
  :group 'expenses)

(defcustom expenses-utils-keyword-category-ht (ht ("CAFE" "Food")
						  ("HOTEL" "Food")
						  ("RESTAURANT" "Food")
						  ("CHEMIST" "Health")
						  ("HOSPITAL" "Health")
						  ("CLINIC" "Health")
						  ("MEDICAL" "Health")
						  ("CHECKUP" "Health")
						  ("ZOO" "Entertainment")
						  ("MUSEUM" "Entertainment")
						  ("MOVIE" "Entertainment")
						  ("CINEMA" "Entertainment"))
  "Hash-table for keyword category."
  :type 'hash-table
  :group 'expenses)

(defun expenses-utils-narrative-has-keyword? (narrative keywords-list)
  "Check if NARRATIVE has any keyword from the KEYWORDS-LIST."
  (let* ((words (mapcar #'upcase (split-string (string-trim narrative "\"" "\"") nil t)))
	 (len (length words))
	 (iter 0)
	 (has-keyword nil)
	 (keywords (mapcar #'upcase keywords-list)))
    (while (< iter len)
      (cond ((member (nth iter words) keywords)
	     (setq has-keyword t)
	     (setq iter len))
	    (t (setq iter (1+ iter)))))
    has-keyword))

(defun expenses-utils-narrative-has-phrase? (narrative phrases-list)
  "Check if NARRATIVE has any phrase from the PHRASES-LIST."
  (let* ((len (length phrases-list))
	 (has-phrase nil)
	 (iter 0))
    (while (< iter len)
      (cond ((string-match-p (regexp-quote (nth iter phrases-list)) (upcase (string-trim narrative "\"" "\"")))
	    (setq has-phrase t)
	    (setq iter len))
	    (t (setq iter (1+ iter)))))
    has-phrase))

(defun expenses-utils-ignore-transaction? (narrative keywords-list phrases-list)
  "Ignore transaction if NARRATIVE contains keyword from KEYWORDS-LIST or phrases from PHRASES-LIST."
  (or (expenses-utils-narrative-has-keyword? narrative keywords-list) (expenses-utils-narrative-has-phrase? narrative phrases-list)))

(defun expenses-utils-auto-assign-category-using-keywords (narrative)
  "Given a NARRATIVE, auto-assign a category using `expenses-utils-keyword-category-ht`."
  (let* ((words (split-string (string-trim narrative "\"" "\"") nil t))
	(len (length words))
	(category)
	(iter 0))
    (while (< iter len)
      (if (ht-get expenses-utils-keyword-category-ht (upcase (nth iter words)))
	  (progn (setq category (ht-get expenses-utils-keyword-category-ht (upcase (nth iter words))))
		 (setq iter len)))
      (setq iter (1+ iter)))
    category))

(defun expenses-utils-auto-assign-category-using-phrases (narrative)
  "Given a NARRATIVE, auto-assign a catagory using phrases from `expenses-utils-phrases-alist`."
  (let* ((phrases (mapcar #'car expenses-utils-phrases-alist))
	 (len (length phrases))
	 (category)
	 (iter 0))
    (while (< iter len)
      (if (string-match-p (regexp-quote (nth iter phrases)) (upcase (string-trim narrative "\"" "\"")))
	  (progn
	    (setq category (cdr (assoc (nth iter phrases) expenses-utils-phrases-alist)))
	    (setq iter len))
	(setq iter (1+ iter))))
    category))

(provide 'expenses-utils)
;;; expenses-utils.el ends here

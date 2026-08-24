;;; expenses-dashboard.el --- Dashboard for expenses.el  -*- lexical-binding: t; -*-

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

;; An interactive dashboard for `expenses'.  It shows, for one user and one
;; period, a summary line, a per-category breakdown with inline bars, and the
;; individual entries.  Everything is recomputed from the org files on `g',
;; and the period, user and category filter can be changed without leaving
;; the buffer.
;;
;; Open it with `expenses-dashboard', or from the transient menu bound to
;; `expenses-menu'.

;;; Code:
(require 'expenses)
(require 'tabulated-list)
(require 'cl-lib)
(require 'seq)

(defgroup expenses-dashboard nil
  "Dashboard for expenses."
  :group 'expenses)

(defcustom expenses-dashboard-bar-width 24
  "Width in characters of the bars in the category breakdown."
  :type 'integer
  :group 'expenses-dashboard)

(defcustom expenses-dashboard-default-period 'month
  "Period the dashboard opens with.
One of the symbols `day', `month', `year' or `all'."
  :type '(choice (const day) (const month) (const year) (const all))
  :group 'expenses-dashboard)

(defface expenses-dashboard-header
  '((t :inherit expenses-face-message :height 1.1))
  "Face for the dashboard header."
  :group 'expenses-dashboard)

(defface expenses-dashboard-bar
  '((t :inherit expenses-face-expense :inverse-video t))
  "Face for the bars in the category breakdown."
  :group 'expenses-dashboard)

(defface expenses-dashboard-button
  '((t :inherit link :underline nil :box (:line-width 1 :style released-button)))
  "Face for the clickable controls at the top of the dashboard."
  :group 'expenses-dashboard)

(defface expenses-dashboard-button-active
  '((t :inherit expenses-dashboard-button :weight bold :inverse-video t))
  "Face for a control matching what the dashboard is currently showing."
  :group 'expenses-dashboard)

(defface expenses-dashboard-button-key
  '((t :inherit help-key-binding))
  "Face for the key shown inside a dashboard control."
  :group 'expenses-dashboard)

(defvar-local expenses-dashboard--user nil
  "User whose expenses the dashboard is showing.")
(defvar-local expenses-dashboard--date nil
  "Reference date, as YYYY-MM-DD, anchoring the shown period.")
(defvar-local expenses-dashboard--period nil
  "Period shown: one of `day', `month', `year' or `all'.")
(defvar-local expenses-dashboard--categories nil
  "Categories to include, or nil for all of them.")
(defvar-local expenses-dashboard--entries nil
  "Entries currently shown, as `expenses-entry' structs.")

(defconst expenses-dashboard--buffer-name "*Expenses Dashboard*")

;;; Period handling

(defun expenses-dashboard--range ()
  "Return (FROM . TO) for the current period, either side possibly nil."
  (pcase expenses-dashboard--period
    ('day (cons expenses-dashboard--date expenses-dashboard--date))
    ('month (expenses-month-bounds expenses-dashboard--date))
    ('year (let ((year (substring expenses-dashboard--date 0 4)))
	     (cons (concat year "-01-01") (concat year "-12-31"))))
    (_ (cons nil nil))))

(defun expenses-dashboard--period-label ()
  "Describe the current period in a human readable way."
  (let ((seconds (org-time-string-to-seconds expenses-dashboard--date)))
    (pcase expenses-dashboard--period
      ('day (format-time-string "%e %B %Y" seconds))
      ('month (format-time-string "%B %Y" seconds))
      ('year (format-time-string "%Y" seconds))
      (_ "All time"))))

(defun expenses-dashboard--shift (n)
  "Move the reference date N periods forward, or backward when N is negative."
  (let* ((decoded (decode-time (org-time-string-to-seconds
				expenses-dashboard--date)))
	 (day (nth 3 decoded))
	 (month (nth 4 decoded))
	 (year (nth 5 decoded)))
    (pcase expenses-dashboard--period
      ('day (setq expenses-dashboard--date
		  (format-time-string
		   "%Y-%m-%d"
		   (time-add (org-time-string-to-seconds expenses-dashboard--date)
			     (* n 24 3600)))))
      ('month (let* ((total (+ (* year 12) (1- month) n))
		     (new-year (/ total 12))
		     (new-month (1+ (mod total 12))))
		(setq expenses-dashboard--date
		      (format "%04d-%02d-%02d" new-year new-month
			      (min day (date-days-in-month new-year new-month))))))
      ('year (setq expenses-dashboard--date
		   (format "%04d-%02d-%02d" (+ year n) month
			   (min day (date-days-in-month (+ year n) month)))))
      (_ (user-error "The period is `all'; there is nothing to move through")))))

;;; Rendering

(defun expenses-dashboard--bar (fraction)
  "Return a bar of FRACTION of `expenses-dashboard-bar-width'."
  (let ((filled (max (if (> fraction 0) 1 0)
		     (round (* fraction expenses-dashboard-bar-width)))))
    (concat (propertize (make-string filled ?\s) 'face 'expenses-dashboard-bar)
	    (make-string (- expenses-dashboard-bar-width filled) ?\s))))

(defun expenses-dashboard--button (key label help thunk &optional active)
  "Return a clickable control for LABEL, reachable with KEY, calling THUNK.
HELP describes it in the echo area.  When ACTIVE is non-nil, draw it as
the selected control."
  (make-text-button
   (concat " " (propertize key 'face 'expenses-dashboard-button-key)
	   " " label " ")
   nil
   'action (lambda (_button) (funcall thunk))
   'help-echo (format "%s (%s)" help key)
   'follow-link t
   'face (if active 'expenses-dashboard-button-active 'expenses-dashboard-button)))

(defun expenses-dashboard--period-button (key period label command)
  "Return a control reachable with KEY running COMMAND, shown as LABEL.
It is drawn as selected while the dashboard is showing PERIOD."
  (expenses-dashboard--button
   key label
   (if (eq period 'all)
       "Show every expense on record"
     (format "Show one %s at a time" label))
   command
   (eq expenses-dashboard--period period)))

(defun expenses-dashboard--toolbar ()
  "Build the rows of clickable controls shown at the top of the dashboard."
  (concat
   (string-join
    (list (expenses-dashboard--button
	   "p" "<" "Show the previous period"
	   #'expenses-dashboard-previous-period)
	  (expenses-dashboard--button
	   "n" ">" "Show the next period"
	   #'expenses-dashboard-next-period)
	  (expenses-dashboard--period-button
	   "D" 'day "day" #'expenses-dashboard-show-day)
	  (expenses-dashboard--period-button
	   "M" 'month "month" #'expenses-dashboard-show-month)
	  (expenses-dashboard--period-button
	   "Y" 'year "year" #'expenses-dashboard-show-year)
	  (expenses-dashboard--period-button
	   "A" 'all "all" #'expenses-dashboard-show-all))
    " ")
   "\n"
   (string-join
    (list (expenses-dashboard--button
	   "d" "date" "Anchor the shown period at another date"
	   (lambda () (call-interactively #'expenses-dashboard-set-date)))
	  (expenses-dashboard--button
	   "u" "user" "Show another user's expenses"
	   (lambda () (call-interactively #'expenses-dashboard-set-user)))
	  (expenses-dashboard--button
	   "c" "category" "Restrict the dashboard to some categories"
	   (lambda () (call-interactively #'expenses-dashboard-filter-categories))
	   expenses-dashboard--categories)
	  (expenses-dashboard--button
	   "a" "add" "Record a new expense"
	   (lambda () (call-interactively #'expenses-dashboard-add-expense)))
	  (expenses-dashboard--button
	   "g" "refresh" "Re-read the expense files"
	   #'expenses-dashboard-refresh)
	  (expenses-dashboard--button
	   "?" "menu" "Open the expenses menu"
	   (lambda () (call-interactively #'expenses-menu))))
    " ")
   "\n\n"))

(defun expenses-dashboard--header ()
  "Build the header shown above the entry list."
  (let* ((entries expenses-dashboard--entries)
	 (total (expenses-total entries))
	 (by-category (expenses-group-by entries #'expenses-entry-category))
	 (biggest (if by-category (cdar by-category) 0))
	 (days (length (seq-uniq (mapcar #'expenses-entry-date entries)))))
    (concat
     (expenses-dashboard--toolbar)
     (propertize (format "%s  —  %s\n"
			 (expenses-dashboard--period-label)
			 (if (string-blank-p (or expenses-dashboard--user ""))
			     "all users"
			   expenses-dashboard--user))
		 'face 'expenses-dashboard-header)
     (format "%s   %s entries over %s day%s%s\n"
	     (propertize (expenses-format-amount total) 'face 'expenses-face-expense)
	     (length entries) days (if (= days 1) "" "s")
	     (if (> days 0)
		 (format "   avg %s/day" (expenses-format-amount (/ total days)))
	       ""))
     (if expenses-dashboard--categories
	 (format "filter: %s\n" (string-join expenses-dashboard--categories ", "))
       "")
     "\n"
     (mapconcat
      (lambda (cell)
	(format "  %-14s %s %12s  %5.1f%%\n"
		(propertize (truncate-string-to-width (car cell) 14)
			    'face 'expenses-face-message)
		(expenses-dashboard--bar (if (> biggest 0) (/ (cdr cell) biggest) 0))
		(propertize (expenses-format-amount (cdr cell))
			    'face 'expenses-face-expense)
		(if (> total 0) (* 100 (/ (cdr cell) total)) 0)))
      by-category "")
     (if by-category "\n" "  No expenses recorded for this period.\n\n"))))

(defun expenses-dashboard--refresh ()
  "Recompute entries and repopulate the dashboard buffer."
  (let* ((range (expenses-dashboard--range))
	 (users (if (string-blank-p (or expenses-dashboard--user ""))
		    (expenses-users)
		  (list expenses-dashboard--user)))
	 (entries (apply #'append
			 (mapcar (lambda (u)
				   (expenses-entries u (car range) (cdr range)))
				 users))))
    (when expenses-dashboard--categories
      (setq entries (seq-filter
		     (lambda (e) (member (expenses-entry-category e)
					 expenses-dashboard--categories))
		     entries)))
    (setq expenses-dashboard--entries
	  (sort entries (lambda (a b) (string< (expenses-entry-date a)
					       (expenses-entry-date b)))))
    (setq tabulated-list-entries
	  (let ((n 0))
	    (mapcar (lambda (e)
		      (cl-incf n)
		      (list (cons n e)
			    (vector (expenses-entry-date e)
				    (propertize (format "%.2f" (expenses-entry-amount e))
						'face 'expenses-face-expense)
				    (expenses-entry-category e)
				    (expenses-entry-details e))))
		    expenses-dashboard--entries)))
    (setq tabulated-list-use-header-line nil)
    (tabulated-list-init-header)
    (setq header-line-format nil)
    (tabulated-list-print t)
    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(insert (expenses-dashboard--header))))))

;;; Commands

(defun expenses-dashboard-refresh ()
  "Re-read the expense files and redraw the dashboard."
  (interactive nil expenses-dashboard-mode)
  (expenses-dashboard--refresh))

(defun expenses-dashboard-next-period ()
  "Show the next period."
  (interactive nil expenses-dashboard-mode)
  (expenses-dashboard--shift 1)
  (expenses-dashboard--refresh))

(defun expenses-dashboard-previous-period ()
  "Show the previous period."
  (interactive nil expenses-dashboard-mode)
  (expenses-dashboard--shift -1)
  (expenses-dashboard--refresh))

(defun expenses-dashboard-set-period (period)
  "Show PERIOD instead of the current one."
  (interactive
   (list (intern (completing-read "Period: " '("day" "month" "year" "all") nil t)))
   expenses-dashboard-mode)
  (setq expenses-dashboard--period period)
  (expenses-dashboard--refresh))

(defun expenses-dashboard-set-date (date)
  "Anchor the shown period at DATE."
  (interactive (list (org-read-date nil nil nil "Date: ")) expenses-dashboard-mode)
  (setq expenses-dashboard--date date)
  (expenses-dashboard--refresh))

(defun expenses-dashboard-set-user (user)
  "Show USER's expenses.  An empty USER means every user."
  (interactive
   (list (completing-read "User (empty for all): " (expenses-users)
			  nil nil nil nil ""))
   expenses-dashboard-mode)
  (setq expenses-dashboard--user user)
  (expenses-dashboard--refresh))

(defun expenses-dashboard-filter-categories (categories)
  "Restrict the dashboard to CATEGORIES; with no selection, show all of them."
  (interactive
   (list (completing-read-multiple
	  "Categories (empty for all): "
	  (or expenses-category-list
	      (seq-uniq (mapcar #'expenses-entry-category
				expenses-dashboard--entries)))))
   expenses-dashboard-mode)
  (setq expenses-dashboard--categories (and categories (delete "" categories)))
  (expenses-dashboard--refresh))

(defun expenses-dashboard-visit-entry ()
  "Open the org file backing the entry at point, on its own line."
  (interactive nil expenses-dashboard-mode)
  (let ((entry (cdr (tabulated-list-get-id))))
    (unless entry (user-error "No entry at point"))
    (find-file-other-window (expenses-entry-file entry))
    (goto-char (point-min))
    (when (search-forward (expenses-entry-date entry) nil t)
      (beginning-of-line))))

(defun expenses-dashboard-add-expense ()
  "Add an expense, then refresh the dashboard."
  (interactive nil expenses-dashboard-mode)
  (call-interactively #'expenses-add-expense)
  (expenses-dashboard--refresh))

(defun expenses-dashboard-show-day ()
  "Show one day at a time."
  (interactive nil expenses-dashboard-mode)
  (expenses-dashboard-set-period 'day))

(defun expenses-dashboard-show-month ()
  "Show one month at a time."
  (interactive nil expenses-dashboard-mode)
  (expenses-dashboard-set-period 'month))

(defun expenses-dashboard-show-year ()
  "Show one year at a time."
  (interactive nil expenses-dashboard-mode)
  (expenses-dashboard-set-period 'year))

(defun expenses-dashboard-show-all ()
  "Show every expense on record."
  (interactive nil expenses-dashboard-mode)
  (expenses-dashboard-set-period 'all))

(autoload 'expenses-menu "expenses-transient" nil t)

(defvar expenses-dashboard-mode-map (make-sparse-keymap)
  "Keymap for `expenses-dashboard-mode'.")

;; Filled in at top level rather than in the `defvar' above, so that reloading
;; this file picks up new bindings: `defvar' leaves an already-bound map alone.
(set-keymap-parent expenses-dashboard-mode-map tabulated-list-mode-map)
(dolist (binding '(("g" . expenses-dashboard-refresh)
		   ("n" . expenses-dashboard-next-period)
		   ("p" . expenses-dashboard-previous-period)
		   ("f" . expenses-dashboard-next-period)
		   ("b" . expenses-dashboard-previous-period)
		   ("P" . expenses-dashboard-set-period)
		   ("D" . expenses-dashboard-show-day)
		   ("M" . expenses-dashboard-show-month)
		   ("Y" . expenses-dashboard-show-year)
		   ("A" . expenses-dashboard-show-all)
		   ("d" . expenses-dashboard-set-date)
		   ("u" . expenses-dashboard-set-user)
		   ("c" . expenses-dashboard-filter-categories)
		   ("a" . expenses-dashboard-add-expense)
		   ("RET" . expenses-dashboard-visit-entry)
		   ("?" . expenses-menu)))
  (define-key expenses-dashboard-mode-map
	      (kbd (car binding)) (cdr binding)))

(define-derived-mode expenses-dashboard-mode tabulated-list-mode "Expenses"
  "Major mode for the expenses dashboard."
  (setq tabulated-list-format
	[("Date" 12 t)
	 ("Amount" 12 (lambda (a b)
			(< (expenses-entry-amount (cdr (car a)))
			   (expenses-entry-amount (cdr (car b))))))
	 ("Category" 16 t)
	 ("Details" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (hl-line-mode 1))

;;;###autoload
(defun expenses-dashboard (&optional user date period)
  "Open the expenses dashboard for USER, anchored at DATE, showing PERIOD.
Called interactively, ask for USER and default to the current month."
  (interactive
   (list (completing-read "Dashboard for (empty for all users): "
			  (expenses-users) nil nil nil nil
			  expenses-default-user-name)
	 nil nil))
  (expenses--check-directory)
  (with-current-buffer (get-buffer-create expenses-dashboard--buffer-name)
    (expenses-dashboard-mode)
    (setq expenses-dashboard--user (or user expenses-default-user-name))
    (setq expenses-dashboard--date (or date (format-time-string "%Y-%m-%d")))
    (setq expenses-dashboard--period (or period expenses-dashboard-default-period))
    (expenses-dashboard--refresh)
    (pop-to-buffer (current-buffer))))

(provide 'expenses-dashboard)
;;; expenses-dashboard.el ends here

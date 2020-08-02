;;; org-autosort.el --- Sort entries in org files automatically -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 Ihor Radchenko

;; Version: 0.11
;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Created: 10 Dec 2017
;; Keywords: matching, outlines
;; Homepage: https://github.com/yantar92/org-autosort
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;

;;; Commentary:

;; This package aims to implement an automatic sorting of the subtrees in org files.
;; The sorting order can be set globally through all the org files, locally in file, or locally in a subtree using :SORT: property.

;; Example configuration:
;; 
;; (use-package org-autosort
;; :config
;; (bind-key "C-c C-S-s" 'org-autosort-sort-entries-at-point org-mode-map)
;; (add-hook 'org-mode-hook #'org-autosort-sort-entries-in-file-maybe)
;; )

;;; Code:

;; [[id:43318d0e-0c79-467b-ac2b-bb107575f6d1][Requires:1]]
(require 'org)
;; Requires:1 ends here

;; [[id:08e58824-f88a-4d3b-a79e-00a1514eb68a][Custom variables:1]]
(defgroup org-autosort nil
  "Options concerning automatic sorting of entries in Org mode."
  :tag "Org Autosort"
  :group 'org)
;; Custom variables:1 ends here

;; [[id:08e58824-f88a-4d3b-a79e-00a1514eb68a][Custom variables:2]]
(defcustom org-autosort-sort-all nil
  "Sort entries if :SORT: property is not defined."
  :type '(boolean))
;; Custom variables:2 ends here

;; [[id:08e58824-f88a-4d3b-a79e-00a1514eb68a][autosort-triggers]]
(defcustom org-autosort-sort-at-file-open t
  "Non nil states for sorting of all items in the org file after opening."
  :type '(boolean))
;; autosort-triggers ends here

;; [[id:08e58824-f88a-4d3b-a79e-00a1514eb68a][autosort-functions]]
(defcustom org-autosort-functions-alist '((todo-up-0 . (:key org-autosort-get-todo :cmp <)) ; default org-sort comparison
					  (todo-down-0 . (:key org-autosort-get-todo :cmp >))
					  ;; compare according to `org-autosort-todo-cmp-order'
					  (todo-up . (:key org-get-todo-state :cmp org-autosort-custom-cmp-todo))
					  (todo-down . (:key org-get-todo-state :cmp (lambda (a b)
										       (not (org-autosort-custom-cmp-todo a b)))))
					  ;;
					  (text-up . (:key org-autosort-get-text :cmp string<))
					  (text-down . (:key org-autosort-get-text :cmp string>))
                                          (priority-up . (:key (org-autosort-get-property "PRIORITY") :cmp string<))
                                          (priority-down . (:key (org-autosort-get-property "PRIORITY") :cmp string>))
                                          (inactive-timestamp-up . (:key org-autosort-get-inactive-timestamp :cmp time-less-p))
					  (inactive-timestamp-down . (:key org-autosort-get-inactive-timestamp :cmp (lambda (a b) (time-less-p b a)))))
  "Alist, defining aliases to sorting rules.
Each value in the list defines a sorting rule.
The rule is a property list with :key and :cmp properties.

:key property defines a function to calculate the key value.
:cmp property defines a function to compare the keys.
In both cases, function can be defined as
 1. lambda expression
 2. function symbol
 3. list, containing function symbol or lambda expression and their arguments

:key function is called with pos at the entry, without arguments.
If :key is defined as in 3, all the nesessary arguments should be in the list.

:cmp function must accept two arguments (after all the arguments as in 3).
It must satisfy the rules of cmp function for `sort'.
If :cmp is omitted, `org-autosort-default-cmp-function' is used."
  :type '(alist :key-type symbol
		:value-type (plist :value-type (choise function
						       (list function (repeat sexp))))))

(defcustom org-autosort-default-cmp-function #'string<
  "Default function, used to compare two entry keys.
Can be also a list of function and its arguments.
It is used if cmp function is not defined.
It must accept two arguments - first and second sorting key to compare.
Non nil return value means that first key is lesser than second key."
  :type '(function))
;; autosort-functions ends here

;; [[id:08e58824-f88a-4d3b-a79e-00a1514eb68a][autosort-default-strategy]]
(defcustom org-autosort-global-sorting-strategy '(priority-down todo-up)
  "Sorting strategy, used to sort entries with :SORT: property not set or nil.
This is a list, which elements can be:
- key of the sorting rule from `org-autosort-functions-alist'
- sorting rule, defined as in `org-autosort-functions-alist'
- :key values as from `org-autosort-functions-alist'
Sorting rules are applied accorting the their position in the list.
nil means that no sorting should be done by default."
  :type '(choice symbol
		 (plist :value-type (choise function
					    (list function (repeat sexp))))))
;; autosort-default-strategy ends here

;; [[id:51552471-6f2b-4792-a8a3-b4bb0d3618d8][by property:1]]
(defun org-autosort-get-property (property)
  "Get the value of PROPERTY for sorting."
  (org-entry-get (point) property 'selective))
;; by property:1 ends here

;; [[id:0d4d78c1-a4a2-4091-8142-ea9e70434d73][By todo keyword:1]]
(defun org-autosort-get-todo ()
  "Get the value of todo keyword for sorting." ; stolen from org-sort-entries in org.el
  (let* ((m (org-get-todo-state))
	 (s (if (member m org-done-keywords)
		'- '+)))
    (- 99 (funcall s
		   (length (member m org-todo-keywords-1))))))
;; By todo keyword:1 ends here

;; [[id:87e5b164-fe1f-4618-9b07-741c27e37bc0][By todo keyword, custom:1]]
(defvar org-autosort-todo-cmp-order nil
  "Order of todo keywords to be shown in sorted subtrees.
       Follow `org-todo-keywords-1' if nil.")

(defun org-autosort-custom-cmp-todo (a b)
  "Compare todo keywords A and B.  Return non nil if A<B."
  (let* ((todo-cmp-order (or org-autosort-todo-cmp-order
			      org-todo-keywords-1))
	 (posa (or (seq-position todo-cmp-order a)
		   0))
	 (posb (or (seq-position todo-cmp-order b)
		   0)))
    (< posa posb)))
;; By todo keyword, custom:1 ends here

;; [[id:5205ed5d-cb92-4711-86b7-c2bf9549f0f5][Alphabetic:1]]
(defun org-autosort-get-text ()
  "Get the text or tags (if text is empty) of the current entry."
  (nth 4 (org-heading-components)))
;; Alphabetic:1 ends here

;; [[id:46525723-2950-4cf9-9f84-12cd9ee8f67e][By first inactive timestamp:1]]
(defun org-autosort-get-inactive-timestamp ()
  "Get the first inactive timestamp of the entry."
  (when (re-search-forward (org-re-timestamp 'inactive) (save-excursion
							  (outline-next-heading))
			   't)
    (goto-char (match-beginning 0))
    (let* ((timestamp (cadr (org-element-timestamp-parser)))
	   (year (plist-get timestamp :year-start))
           (month (plist-get timestamp :month-start))
           (day (plist-get timestamp :day-start))
           (hour (plist-get timestamp :hour-start))
           (minute (plist-get timestamp :minute-start)))
      (encode-time 0 (or minute 0) (or hour 0) day month year))))
;; By first inactive timestamp:1 ends here

;; [[id:7b077f97-a744-4197-9b4f-015af71ab95f][General sorting routine:1]]
(defun org-autosort--org-back-to-heading ()
  "Back to current heading or move to the first heading if before the first heading."
  (when (eq major-mode 'org-mode)
    (condition-case err
	(org-back-to-heading)
      (error
       (if (string-match-p "Before first headline at position"
			   (error-message-string err))
           (outline-next-heading)
	 (signal (car err) (cdr err)))))))

(defun org-autosort-sorting-strategy-elementp (elm)
  "Validate element ELM of sorting strategy.  Return (:key ... [:cmp ...]) if element and nil otherwise."
  (pcase elm
    (`(quote val)
     (org-autosort-sorting-strategy-elementp val))
    ((pred functionp)
     (list :key elm))
    ((pred (lambda (arg) (assoc arg org-autosort-functions-alist)))
     (alist-get elm org-autosort-functions-alist))
    ((pred (lambda (arg) (plist-get arg :key)))
     (let ((key (org-autosort-sorting-strategy-elementp (plist-get elm :key)))
	   (cmp (org-autosort-sorting-strategy-elementp (plist-get elm :cmp))))
       (cond ((and key (not cmp)) key)
	     ((and key cmp) (plist-put key :cmp (plist-get cmp :key)))
	     (t nil))))
    (`(,func . ,args)
     (if (functionp func)
	 (list :key elm)
       nil))
    (_ nil)))

(defun org-autosort-sorting-strategyp (sorting-strategy)
  "Validate if SORTING-STRATEGY is valid and return it.
The strategy is ensured to be a list.
Signal user error and return nil if argument is not a sorting strategy."
  (if (not sorting-strategy)
      nil
    (or (let ((res (org-autosort-sorting-strategy-elementp sorting-strategy)))
	  (if res (list res)))
	(let* ((testresult (mapcar (lambda (elm) (cons (org-autosort-sorting-strategy-elementp elm)
						  elm))
				   sorting-strategy))
	       (err-elm (alist-get nil testresult 'none)))
	  (if (equal err-elm 'none)
	      sorting-strategy
	    nil
	    (user-error "Wrong element of sorting strategy: \"%s\" in buffer: %s"
			err-elm (buffer-name)))))))

(defun org-autosort-get-sorting-strategy ()
  "Get sorting strategy at point for the current entry's subtree being sorted."
  (save-excursion
    (org-autosort--org-back-to-heading)
    (condition-case err
	(org-back-to-heading)
      (error (unless (string-match-p "Before first headline" (cadr err)) (signal (car err) (cdr err)))))
    (let ((property (org-entry-get (point) "SORT" 'selective)))
      (pcase property
	('t (org-autosort-sorting-strategyp org-autosort-global-sorting-strategy))
	('nil (and org-autosort-sort-all
		   (org-autosort-sorting-strategyp org-autosort-global-sorting-strategy)))
	("" nil)
	('none nil)
	(_ (if (= (cdr (read-from-string property))
		  (length property))
	       (org-autosort-sorting-strategyp (car (read-from-string property)))
	     (user-error "Cannot read :SORT: property: \"%s\" in buffer: %s" property (buffer-name))
	     nil))))))

(defun org-autosort-construct-get-value-function-atom (sorting-strategy-elm)
  "Construct get-value function for single element of sorting strategy (SORTING-STRATEGY-ELM)."
  (let ((key (plist-get (org-autosort-sorting-strategy-elementp sorting-strategy-elm) :key)))
    (pcase key
      ((pred functionp)
       key)
      (`(,func . ,args)
       (when (functionp func)
	 (lambda () (apply (car key) (cdr key)))))
      ('nil (lambda () nil)))))

(defun org-autosort-construct-get-value-function ()
  "Return get-value function at point.
This function returns a list of sorting keys."
  (let ((sorting-strategy (org-autosort-get-sorting-strategy)))
    (if sorting-strategy
	(let ((func-list (mapcar #'org-autosort-construct-get-value-function-atom sorting-strategy)))
	  (lambda () (mapcar #'funcall func-list)))
      (lambda () (list nil)))))

(defun org-autosort-construct-cmp-function-atom (sorting-strategy-elm)
  "Construct cmp function for single element of sorting strategy (SORTING-STRATEGY-ELM)."
  (let* ((sorting-strategy-elm (org-autosort-sorting-strategy-elementp sorting-strategy-elm))
	 (cmp (and sorting-strategy-elm
		   (or (plist-get sorting-strategy-elm :cmp)
		       org-autosort-default-cmp-function))))
    (pcase cmp
      ((pred functionp)
       (lambda (a b) (funcall cmp a b)))
      (`(,func . ,args)
       (when (functionp func)
	 (lambda (a b) (apply func a b args))))
      ('nil (lambda (a b) nil)))))

(defun org-autosort-construct-cmp-function ()
  "Return cmp function at point."
  (let ((sorting-strategy (org-autosort-get-sorting-strategy)))
    (if (not sorting-strategy)
	(lambda (lista listb) nil)
      (let ((cmp-func-list (mapcar #'org-autosort-construct-cmp-function-atom sorting-strategy)))
	(lambda (lista listb)
	  (let ((resultlist (seq-mapn (lambda (func a b)
					(cons (funcall func a b)
					      (funcall func b a)))
				      cmp-func-list lista listb)) ; list of cons (a<b . b<a)
		(done nil)
		result)
	    (while (and (not done)
			(not (seq-empty-p resultlist)))
	      (let ((elem (pop resultlist)))
		(unless (eq (car elem)
			    (cdr elem)) ; not equal
		  (setq done t)
		  (setq result (car elem)))))
	    result))))))

(defun org-autosort-org-sort-entries-wrapper (&rest args)
  "Run `org-sort-entries' at point with ARGS if nesessary.
Make sure, folding state is not changed."
  (save-excursion
    (org-autosort--org-back-to-heading)
    (when (org-autosort-get-sorting-strategy)
      (let ((subtree-end (save-excursion (org-end-of-subtree)))
	    (next-heading (save-excursion (or (outline-next-heading)
					      (buffer-end +1)))))
	(when (< next-heading subtree-end)
	  (save-excursion
	    (save-restriction
	      (condition-case err
		  (apply #'org-sort-entries args)
		(user-error
		 (unless (string-match-p "Nothing to sort"
					 (error-message-string err))
		   (signal (car err) (cdr err))))))))))))

(defun org-autosort-sort-entries-at-point-nonrecursive ()
  "Sort org-entries at point nonrecursively."
  (interactive)
  (funcall #'org-autosort-org-sort-entries-wrapper
	   nil ?f
	   (org-autosort-construct-get-value-function)
	   (org-autosort-construct-cmp-function)))

(defun org-autosort-sort-entries-at-point-recursive ()
  "Sort org-entries at point recursively."
  (interactive)
  (save-excursion
    (let ((subtree-end))
      (condition-case err
	  (org-back-to-heading)
	(error
	 (if (string-match-p "Before first headline at position"
			     (error-message-string err))
	     (progn
               (outline-next-heading)
	       (setq subtree-end (point-max)))
	   (signal (car err) (cdr err)))))
      (setq subtree-end (or subtree-end
			    (save-excursion (org-end-of-subtree))))
      (while (< (point) subtree-end)
	(funcall #'org-autosort-org-sort-entries-wrapper
		 nil ?f
		 (org-autosort-construct-get-value-function)
		 (org-autosort-construct-cmp-function))
	(outline-next-heading)))))

;;;###autoload
(defun org-autosort-sort-entries-at-point (&optional ARG)
  "Sort org entries at point.
Sort recursively if invoked with \\[universal-argument]."
  (interactive "P")
  (save-excursion
    (org-autosort--org-back-to-heading)
    (if (equal ARG '(4))
	(org-autosort-sort-entries-at-point-recursive)
      (org-autosort-sort-entries-at-point-nonrecursive))
    (while (not (eq org-cycle-subtree-status 'folded)) ;; magic constant...
      (org-cycle))
    (outline-show-branches) ;; probably, it should be configurable
    ))

;;;###autoload
(defun org-autosort-sort-entries-in-file ()
  "Sort all entries in the file recursively."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((subtree-end (point-max)))
      (while (< (point) subtree-end)
	(funcall #'org-autosort-org-sort-entries-wrapper
		 nil ?f
		 (org-autosort-construct-get-value-function)
		 (org-autosort-construct-cmp-function))
	(outline-next-heading)
        ;; optimise large files with many entries
        (unless org-autosort-sort-all
          (and
	   (search-forward ":SORT:" subtree-end 'noerror)
           (while (and (< (point) subtree-end)
		       (save-excursion
			 (re-search-backward org-property-start-re (save-excursion (org-back-to-heading)) t)
                         (looking-at org-property-drawer-re))
                       (not (string-match ":SORT:" (match-string 0))))
             (search-forward ":SORT:" subtree-end 'noerror))))))
    (outline-hide-sublevels 1)))

;;;###autoload
(defun org-autosort-sort-entries-in-file-maybe ()
  "Sort all entries in the file recursively if `org-autosort-sort-at-file-open' is not nil."
  (when org-autosort-sort-at-file-open (org-autosort-sort-entries-in-file)))
;; General sorting routine:1 ends here

;; [[id:cf53b069-fcbb-45f9-9a80-e05f88d1fec5][File epilogue:1]]
(provide 'org-autosort)

;;; org-autosort.el ends here
;; File epilogue:1 ends here

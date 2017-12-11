;;; org-autosort.el --- Sort entries in org files automatically

;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Created: 10 Dec 2017
;; Version: 0.1
;; Keywords: matching, outlines
;; Homepage:
;; Package-Requires: (org)

;;; Commentary:

;; This package aims to implement an automatic sorting of the subtrees in org files.
;; The sorting order can be set globally through all the org files, locally in file, or locally in a subtree using =:SORT:= property.
;; Everything, except global sorting order, can be set using standard inheritance capabilities of the org properties (file local, subtree local with or without inheritance for subtrees inside the subtree).
;; Global sorting order can be set via =org-autosort-global-sorting-strategy= variable.

;;; Code:

;;; -*- lexical-binding: t; -*-

(defgroup org-autosort nil
  "Customization options of org-autosort package.")

(defcustom org-autosort-sort-at-file-open t
  "Non nil states for sorting of all items in the org file after opening."
  :type '(boolean))

(defcustom org-autosort-sort-after-C-c-C-c t
  "Non nil means sorting of a subtree at point after \org-ctrl-c-ctrl-c.
All the items are sorted if point is at the file header.
Sorting is recursive if org-autosort-sort-after-C-c-C-c-recursive is non nil."
  :type '(boolean))

(defcustom org-autosort-sort-after-C-c-C-c-recursive nil
  "If non nil, sort the subtrees recursively at a point after \org-ctrl-c-ctrl-c."
  :type '(boolean))

(defcustom org-autosort-functions-alist '((org-todo-state-up . (org-autosort-get-todo . <))
    				      (org-todo-state-down . (org-autosort-get-todo . >))
    				      )
  "Alist, defining how to sort entries.
The values in alist can be function or cons.
If the value is function, it should return the sorting key of the entry at point and should not require arguments.
If the value is cons, its car should be sorting key function or the list, and its cdr should be a function,
accepting at least two sorting keys and returning non nil if first key is lesser than second."
  :type '(alist :key-type symbol :value-type (choise (function)
    						 (cons function function)
    						 )))

(defcustom org-autosort-default-cmp-function #'string<
  "Default function, used to compare two entry keys.
It is used if cmp function for org-autosort-functions-alist is not defined.
It must accept two arguments - first and second sorting key to compare.  Non nil return value means that first key is lesser than second key."
  :type '(function))

(defcustom org-autosort-global-sorting-strategy nil
  "Sorting strategy, used to sort entries with :SORT: property not set or nil.
This is a symbol/function/cons or a list of symbols/functions/lists/cons.
If an element is a symbol, this symbol should be key from org-autosort-functions-alist.
If an element is a function, this function will be called at point with no arguments and return sorting key.  The keys will be compared using org-autosort-default-cmp-function.
If an element is a list, its first element should be sorting function and remaining elements will be supplied to the function during the call.
If an element is cons, its car should be a symbol/function/list, which defines sorting key function.  Its cdr should be a function/list, defining function to compare the keys.  This function must accept at least two arguments - first and second key to compare.  It should return non nil if the first key is lesser than second.
nil means that no sorting should be done by default."
  :type '(choice
      (choise (symbol)
    	      (function)
    	      (list function
    		    (repeat :inline t sexp)))
      (repeat (choise (symbol)
    		      (function)
    		      (list function
    			    (repeat :inline t sexp))
    		      (cons (list function
    				  (repeat :inline t sexp))
    			    (list function
    				  (repeat :inline t sexp)))))))

(defun org-autosort-get-property (property)
  "Get the value of PROPERTY for sorting."
  (org-entry-get (point)
     	    property
     	    'selective))

(defun org-autosort-get-todo ()
  "Get the value of todo keyword for sorting." ; stolen from org-sort-entries in org.el
  (let* ((m (org-get-todo-state))
    (s (if (member m
     		   org-done-keywords) '- '+))
    )
    (- 99
  (funcall s
     	   (length (member m
     			   org-todo-keywords-1))))))

(defun list-but-not-consp (arg)
  "Return non nil if ARG is list and not cons."
  (if (null arg)
  t
    (when (listp arg)
  (list-but-not-consp (cdr arg)))))

(defun org-autosort-sorting-strategy-elementp (elm)
  "Validate element ELM of sorting strategy.  Return t if element and nil otherwise."
  (pcase elm
    ((pred (lambda (arg) (assoc arg
        		   org-autosort-functions-alist)))
     t
     )
    ((pred functionp)
     t
     )
    (`(quote val)
     (org-autosort-sorting-strategy-elementp val)
     )
    (`(,keyfunc . ,cmpfunc)
     (if (list-but-not-consp elm) ; not a cons cell
     (org-autosort-sorting-strategy-elementp keyfunc)
   (and (org-autosort-sorting-strategy-elementp keyfunc)
  (org-autosort-sorting-strategy-elementp cmpfunc)
  )))
    (_ nil)))

(defun org-autosort-sorting-strategyp (sorting-strategy)
  "Validate if SORTING-STRATEGY is a valid org-autosort-sorting-strategy and return ensure that it is a list.  Signal error if not."
  (if (not sorting-strategy)
  nil
    (let* ((sorting-strategy (if (or (symbolp sorting-strategy)
    				(functionp sorting-strategy)
    				(not (list-but-not-consp sorting-strategy)))
    			     (list sorting-strategy)
    			   sorting-strategy))
       (testresult (mapcar (lambda (elm) (cons (org-autosort-sorting-strategy-elementp elm)
    					  elm))
    			   sorting-strategy))
       (err-elm (alist-get nil
    			   testresult
    			   'none)))
  (if (equal err-elm 'none)
      sorting-strategy
    (error "Wrong element of sorting strategy: \"%s\""
    	   err-elm)))))

(defun org-autosort-get-sorting-strategy ()
  "Determine sorting strategy at the point."
  (let ((property (org-entry-get (point)
        			 "SORT"
        			 'USE-INHERITANCE)))
    (if (seq-empty-p property)
    (org-autosort-sorting-strategyp org-autosort-global-sorting-strategy)
  (if (= (cdr (read-from-string property))
             (length property))
  (org-autosort-sorting-strategyp (car (read-from-string property)))
    (error "Invalid value in :SORT: property: \"%s\"" property)
    ))))

(defun org-autosort-construct-get-value-function-atom (sorting-strategy-elm)
  "Return result of get-value function for single element of sorting strategy (SORTING-STRATEGY-ELM)."
  (pcase sorting-strategy-elm
    ((app (lambda (arg) (assoc arg
        		  org-autosort-functions-alist))
  `(_ . func) )
     (org-autosort-construct-get-value-function-atom func))
    ((pred functionp)
     (funcall sorting-strategy-elm))
    (`(quote val)
     (org-autosort-sorting-strategy-elementp val))
    (`(,keyfunc . ,cmpfunc)
     (if (list-but-not-consp sorting-strategy-elm) ; not a cons cell
     (apply keyfunc
        	cmpfunc)
   (org-autosort-construct-get-value-function-atom keyfunc)
   ))))

(defun org-autosort-construct-get-value-function ()
  "Return get-value function at point.
This function returns a list of sorting keys."
  (let ((sorting-strategy (org-autosort-get-sorting-strategy)))
    (if sorting-strategy
    (mapcar #'org-autosort-construct-get-value-function-atom
        	sorting-strategy)
  nil
  )))

(defun org-autosort-construct-cmp-function-atom (sorting-strategy-elm a b)
  "Return result of application of cmp function for single element of sorting strategy (SORTING-STRATEGY-ELM) called with A and B arguments."
  (pcase sorting-strategy-elm
    ((app (lambda (arg) (assoc arg
        		  org-autosort-functions-alist))
  `(_ . func))
     (org-autosort-construct-cmp-function-atom func
        				       a
        				       b))
    ((pred functionp)
     (funcall org-autosort-default-cmp-function
              a
              b))
    (`(quote val)
     (org-autosort-sorting-strategy-elementp val))
    (`(,keyfunc . ,cmpfunc)
     (if (list-but-not-consp sorting-strategy-elm) ; not a cons cell
     (funcall org-autosort-default-cmp-function
        	  a
        	  b)
   (if (listp cmpfunc)
   (apply (car cmpfunc)
        	  a
        	  b
        	  (cdr cmpfunc))
     (funcall cmpfunc
        	  a
        	  b))))))

(defun org-autosort-construct-cmp-function (lista listb)
  "Return cmp at point."
  (let ((sorting-strategy (org-autosort-get-sorting-strategy)))
    (if (not sorting-strategy)
    nil
  (let ((resultlist (seq-mapn (lambda (arg a b)
        			  (cons (org-autosort-construct-cmp-function-atom arg
        									  a
        									  b)
        				(org-autosort-construct-cmp-function-atom arg
        									  b
        									  a)))
        		      sorting-strategy
        		      lista
        		      listb)) ; list of cons (a<b . b<a)
    (done nil)
    result
    )
    (while (and (not done)
              (not (seq-empty-p resultlist))
              )
  (let ((elem (pop resultlist)))
    (unless (and (car elem)
        	   (cdr elem)) ; not equal
  (setq done t)
  (setq result (car elem)))))
    result
    ))))

(defun org-autosort-sort-entries-at-point-nonrecursive ()
  "Sort org-entries at point nonrecursively.  Sort all entries _recursively_ if at the file header."
  (funcall #'org-sort-entries
   nil
   ?f
   #'org-autosort-construct-get-value-function
   #'org-autosort-construct-cmp-function))

(defun org-autosort-sort-entries-at-point-recursive ()
  "Sort org-entries at point recursively."
  (condition-case err
  (org-map-entries (lambda nil (funcall #'org-sort-entries
        			       nil
        			       ?f
        			       #'org-autosort-construct-get-value-function
        			       #'org-autosort-construct-cmp-function))
        	       nil
        	       'tree)
    (error
     (if (string-match-p "Before first headline at position"
        		 (error-message-string err))
     (org-map-entries (lambda nil (funcall #'org-sort-entries
        				  nil
        				  ?f
        				  #'org-autosort-construct-get-value-function
        				  #'org-autosort-construct-cmp-function))
        		  nil
        		  'file)
   (signal (car err)
               (cdr err))
   ))))

(defun org-autosort-sort-entries-at-point (&optional force)
  "Sort org entries at point.  Respect value of org-autosort-sort-after-C-c-C-c if FORCE is non nil."
  (when (or org-autosort-sort-after-C-c-C-c force)
    (if org-autosort-sort-after-C-c-C-c-recursive
    (org-autosort-sort-entries-at-point-recursive)
  (org-autosort-sort-entries-at-point-nonrecursive)
  )))

(defun org-autosort-sort-entries-in-file (&optional force)
  "Sort all entries in the file recursively.  Do not respect org-autosort-sort-at-file-open if FORCE is non nil."
  (when (or org-autosort-sort-at-file-open force)
    (org-map-entries (lambda nil (funcall #'org-sort-entries
        			     nil
        			     ?f
        			     #'org-autosort-construct-get-value-function
        			     #'org-autosort-construct-cmp-function))
        	     nil
        	     'file)))

(add-hook 'org-load-hook
  #'org-autosort-sort-entries-in-file)

(add-hook 'org-ctrl-c-ctrl-c-hook
  #'org-autosort-sort-entries-at-point)

(provide 'org-autosort)

  ;;; org-autosort.el ends here

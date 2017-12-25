<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgfd39da4">NEXT Automatic sorting of items in org mode</a>
<ul>
<li><a href="#org383b60a">Motivation</a></li>
<li><a href="#org78610fc">Overview</a></li>
<li><a href="#org7c459ac">Configuration</a></li>
<li><a href="#orgad2aba2">Defaults</a></li>
<li><a href="#org9100e55">NEXT Implementation</a>
<ul>
<li><a href="#orgd6d4e2a">Header</a></li>
<li><a href="#orgf2bb7c5">Custom variables</a></li>
<li><a href="#org75f961d">NEXT Standard sorting functions</a></li>
<li><a href="#org7943f88">NEXT [#A] General sorting routine</a></li>
<li><a href="#org4f3d4bd">File epilogue</a></li>
</ul>
</li>
<li><a href="#orge5dbee6">NEXT Ideas</a>
<ul>
<li><a href="#org6d6e0eb">Sort items when opening org file, on edit??</a></li>
<li><a href="#orga9e85b3">CANCELLED do not use org-sort, because it does not allow to combine sorts (i.e. sort by one criteria, if equal - by other)</a></li>
<li><a href="#org7466077">allow to define sort criteria like a lisp function in the properties field</a></li>
<li><a href="#org7aff27a">Do not sort only but filter items in org files/agenda</a></li>
<li><a href="#org4f2376b">CANCELLED Take care about exact position for <code>C-c C-c</code> (say, we are inside the table - user may not want to sort)</a></li>
<li><a href="#orge43526c">Sort only items, matching org search regex</a></li>
<li><a href="#org3d728e5">Handle nothing to sort</a></li>
<li><a href="#org3aae133">make interactive versions of sorting functions</a></li>
<li><a href="#org5d31964">autosort - do not sort but show agenda</a></li>
<li><a href="#org72676ab">add hooks to to autosort</a></li>
<li><a href="#org8a81abe">auto add hooks according to the sort type - should be able to define hooks for every sort type</a></li>
<li><a href="#orgfd37580">get rid of annoying unfolding after <code>org-sort</code></a></li>
<li><a href="#org127a626">put buffer name in error report for wrong element of sorting strategy</a></li>
<li><a href="#org07ac02f">should be able to define alias in sorting strategy</a></li>
<li><a href="#org92cbef6">NEXT rewrite sorting strategy to use assoc lists</a></li>
<li><a href="#org8504982">use local hook in autosort for toggle hooks</a></li>
<li><a href="#org904669a">add this functionality? Sorting Org Mode lists using a sequence of regular expressions  13</a></li>
<li><a href="#org0c27c8e">do not raise error but put a message and do not sort on wrong :SORTING: format</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>


<a id="orgfd39da4"></a>

# NEXT Automatic sorting of items in org mode


<a id="org383b60a"></a>

## Motivation

For now, standard org capabilities allow to sort entries in the agenda
views automatically. However, sorting of the actual org files is
rather limited. We only have `org-sort` function, which allows to sort
by one criteria only (not like `org-agenda-sorting-strategy` with
multiple sort criteria). Moreover, it can be done manually only.  

Sometimes, one may have a need to look at some kind of long list of
TODO items (for example, this infinitely growing list of things you
wish to do one day, or list of ideas). It is not comfortable to scroll
across such kind of list to look for specific item. Sorting is
something, that can help here.

-   Add some example here

-   END

    Of course, you can still use `org-sort` or agenda view with restriction
    to the current subtree, but it may be disrupting if you want to look
    through multiple of such a lists.  
    
    The solution is to implement automatic sorting of subtrees in org
    files.


<a id="org78610fc"></a>

## Overview

This package aims to implement an automatic sorting of the subtrees
in org files. The sorting order can be set globally through all the
org files, locally in file, or locally in a subtree using `:SORT:`
property.  

Everything, except global sorting order, can be set using standard
inheritance capabilities of the org properties (file local, subtree
local with or without inheritance for subtrees inside the
subtree). Global sorting order can be set via
`org-autosort-global-sorting-strategy` variable.


<a id="org7c459ac"></a>

## Configuration

Both `:SORT:` property and `org-autosort-global-sorting-strategy`
are lists, which determine how to sort the entries.

`org-autosort-global-sorting-strategy` defined how to sort entries by
default. It is a list of , defining the comparison
between sorted entries. First, the entries are sorted via first rule
from the list. If the calculated keys are equal, second rule is used,
and so on.

`:SORT:` can be either `nil`, `t`, or the same format as
`org-autosort-global-sorting-strategy`:

-   **`nil`:** do not sort entries
-   **`t`:** use `org-autosort-global-sorting-strategy`
-   **list:** define separate sorting strategy
-   **not defined:** sort if `org-autosort-sort-all` is non `nil`

The sorting can be done after:

-   opening the org file
-   `org-autosort-sort-entries-at-point` command
-   `org-autosort-sort-entries-in-file` command


<a id="orgad2aba2"></a>

## Defaults

The package provide some predefined <a id="org4b4b81c"></a>, all are listed in
`org-autosort-functions-alist`.

    (defcustom org-autosort-functions-alist '((todo-up-0 . (:key org-autosort-get-todo :sort <))
    					  (todo-down-0 . (:key org-autosort-get-todo :sort >))
    					  (todo-up . (:key org-get-todo-state :sort org-autosort-custom-cmp-todo))
    					  (todo-down . (:key org-get-todo-state :sort (lambda (a b)
    											(not (org-autosort-custom-cmp-todo a b)))))
    					  (org-text-up . (:key org-autosort-get-text :sort string<))
    					  (org-text-down . (:key org-autosort-get-text :sort string>)))
      "Alist, defining aliases to sorting rules.
    Each value in the list defines a sorting rule.
    The rule is a property list with :key and :sort properties.
    
    :key property defines a function to calculate the key value.
    :sort property defines a function to compare the keys.
    In both cases, function can be defined as
     1. lambda expression
     2. function symbol
     3. list, containing function symbol or lambda expression and their arguments
    
    :key function is called with pos at the entry, without arguments.
    If :key is defined as in 3, all the nesessary arguments should be in the list.
    
    :sort function must accept two arguments (after all the arguments as in 3).
    It must satisfy the rules of cmp function for `sort'.
    If :sort is omitted, `org-autosort-default-cmp-function' is used."
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

You can control automatic sorting by setting sort triggers

    (defcustom org-autosort-sort-at-file-open t
      "Non nil states for sorting of all items in the org file after opening."
      :type '(boolean))

Default sorting strategy is

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


<a id="org9100e55"></a>

## NEXT Implementation


<a id="orgd6d4e2a"></a>

### Header

    ;;; org-autosort.el --- Sort entries in org files automatically
    
    ;; Version: 0.1
    ;; Author: Ihor Radchenko <yantar92@gmail.com>
    ;; Created: 10 Dec 2017
    ;; Keywords: matching, outlines
    ;; Homepage: https://github.com/yantar92/org-autosort
    ;; Package-Requires: (org)
    
    ;;; Commentary:
    
    ;; This package aims to implement an automatic sorting of the subtrees in org files.
    ;; The sorting order can be set globally through all the org files, locally in file, or locally in a subtree using :SORT: property.
    
    ;;; Code:
    
    ;;; -*- lexical-binding: t; -*-


<a id="orgf2bb7c5"></a>

### Custom variables

    (defgroup org-autosort nil
      "Customization options of org-autosort package.")

-   to sort or not to sort

    (defcustom org-autosort-sort-all nil
      "Sort entries is :SORT: property is not defined.")

-   auto sort triggers

    (defcustom org-autosort-sort-at-file-open t
      "Non nil states for sorting of all items in the org file after opening."
      :type '(boolean))

-   predefined sorts

    (defcustom org-autosort-functions-alist '((todo-up-0 . (:key org-autosort-get-todo :sort <))
    					  (todo-down-0 . (:key org-autosort-get-todo :sort >))
    					  (todo-up . (:key org-get-todo-state :sort org-autosort-custom-cmp-todo))
    					  (todo-down . (:key org-get-todo-state :sort (lambda (a b)
    											(not (org-autosort-custom-cmp-todo a b)))))
    					  (org-text-up . (:key org-autosort-get-text :sort string<))
    					  (org-text-down . (:key org-autosort-get-text :sort string>)))
      "Alist, defining aliases to sorting rules.
    Each value in the list defines a sorting rule.
    The rule is a property list with :key and :sort properties.
    
    :key property defines a function to calculate the key value.
    :sort property defines a function to compare the keys.
    In both cases, function can be defined as
     1. lambda expression
     2. function symbol
     3. list, containing function symbol or lambda expression and their arguments
    
    :key function is called with pos at the entry, without arguments.
    If :key is defined as in 3, all the nesessary arguments should be in the list.
    
    :sort function must accept two arguments (after all the arguments as in 3).
    It must satisfy the rules of cmp function for `sort'.
    If :sort is omitted, `org-autosort-default-cmp-function' is used."
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

-   default sorting strategy

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


<a id="org75f961d"></a>

### NEXT Standard sorting functions

-   by property

        (defun org-autosort-get-property (property)
          "Get the value of PROPERTY for sorting."
          (org-entry-get (point)
        		 property
        		 'selective))

-   By todo keyword

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

-   By todo keyword, custom

        (defvar org-autosort-todo-cmp-order nil
          "Order of todo keywords to be shown in sorted subtrees.
               Follow `org-todo-keywords-1' if nil."
          )
        (defun org-autosort-custom-cmp-todo (a b)
          "Compare todo keywords A and B.  Return non nil if A<B."
          (let* ((todo-cmp-orgder (or org-autosort-todo-cmp-order
        			      org-todo-keywords-1))
        	 (posa (or (seq-position org-autosort-todo-cmp-order
        				 a)
        		   0))
        	 (posb (or (seq-position org-autosort-todo-cmp-order
        				 b)
        		   0)))
            (< posa posb)))

-   Alphabetic

        (defun org-autosort-get-text ()
          "Get the text or tags (if text is empty) of the current entry."
          (nth 4 (org-heading-components))
          )

-   By first inactive timestamp

-   By deadline

-   By clocking time

-   Numerically, beginning of the entry/item

-   By Priority

-   By scheduled time/date

-   By active timestamp

-   By any timestamp


<a id="org7943f88"></a>

### NEXT [#A] General sorting routine

    (defun org-autosort-sorting-strategy-elementp (elm)
      "Validate element ELM of sorting strategy.  Return (:key ... [:cmp ...]) if element and nil otherwise."
      (pcase elm
        (`(quote val)
         (org-autosort-sorting-strategy-elementp val))
        ((pred functionp)
         (list :key elm))
        ((pred (lambda (arg) (assoc arg org-autosort-functions-alist)))
         (assoc elm org-autosort-functions-alist))
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
      "Validate if SORTING-STRATEGY is a valid and return it.
    The strategy is ensured to be a list.
    Signal user error and return nil if argument is not a sorting strategy."
      (if (not sorting-strategy)
          nil
        (or (let ((res (org-autosort-sorting-strategy-elementp sorting-strategy)))
    	  (if res (list res)))
    	(let* ((testresult (mapcar #'org-autosort-sorting-strategy-elementp sorting-strategy))
    	       (err-elm (seq-contains testresult 'nil)))
    	  (if err-elm
    	      sorting-strategy
    	    nil
    	    (user-error "Wrong element of sorting strategy: \"%s\"\nBuffer: %s"
    			err-elm (buffer-name)))))))
    
    (defun org-autosort-get-sorting-strategy ()
      "Get sorting strategy at point for the current entry's subtree being sorted."
      (let ((property (org-entry-get (point) "SORT" 'selective)))
        (pcase property
          ('nil (org-autosort-sorting-strategyp org-autosort-global-sorting-strategy))
          ("" nil)
          (_ (if (= (cdr (read-from-string property))
    		(length property))
    	     (org-autosort-sorting-strategyp (car (read-from-string property)))
    	   (user-error "Cannot read :SORT: property: \"%s\"\nBuffer: %s" property (buffer-name))
    	   nil)))))
    
    (defun org-autosort-construct-get-value-function-atom (sorting-strategy-elm)
      "Construct get-value function for single element of sorting strategy (SORTING-STRATEGY-ELM)."
      (let ((sorting-strategy-elm (plist-get (org-autosort-sorting-strategy-elementp sorting-strategy-elm) :key)))
        (pcase sorting-strategy-elm
          ((pred functionp)
           sorting-strategy-elm)
          (`(,func . ,args)
           (when (functionp func)
    	 (lambda () (apply func args))))
          ('nil (lambda () nil)))))
    
    (defun org-autosort-construct-get-value-function (&optional atparent)
      "Return get-value function at point.
    This function returns a list of sorting keys."
      (let ((sorting-strategy (org-autosort-get-sorting-strategy)))
        (if sorting-strategy
    	(mapcar #'org-autosort-construct-get-value-function-atom
    		sorting-strategy)
          nil)))
    
    (defun org-autosort-construct-cmp-function-atom (sorting-strategy-elm a b)
      "Return result of application of cmp function for single element of sorting strategy (SORTING-STRATEGY-ELM) called with A and B arguments."
      (pcase sorting-strategy-elm
        ((app (lambda (arg) (assoc arg
    			  org-autosort-functions-alist))
    	  `(,_ . ,func))
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
    
    (defun org-autosort-org-sort-entries-wrapper (&rest args)
      "Run 'org-sort-entries' at point with ARGS if nesessary.
          Make sure, folding state is not changed."
      (when (org-autosort-get-sorting-strategy 'atparent)
        (save-excursion
          (save-restriction
    	(condition-case err
    	    (apply #'org-sort-entries
    		   args)
    	  (user-error
    	   (if (string-match-p "Nothing to sort"
    			       (error-message-string err))
    	       t
    	     (signal (car err)
    		     (cdr err)))))))))
    
    (defun org-autosort-sort-entries-at-point-nonrecursive ()
      "Sort org-entries at point nonrecursively.  Sort all entries _recursively_ if at the file header."
      (funcall #'org-autosort-org-sort-entries-wrapper
    	   nil
    	   ?f
    	   #'org-autosort-construct-get-value-function
    	   #'org-autosort-construct-cmp-function))
    
    (defun org-autosort-sort-entries-at-point-recursive ()
      "Sort org-entries at point recursively."
      (condition-case err
          (org-map-entries (lambda nil (funcall #'org-autosort-org-sort-entries-wrapper
    				     nil
    				     ?f
    				     #'org-autosort-construct-get-value-function
    				     #'org-autosort-construct-cmp-function))
    		       nil
    		       'tree)
        (error
         (if (string-match-p "Before first headline at position"
    			 (error-message-string err))
    	 (org-map-entries (lambda nil (funcall #'org-autosort-org-sort-entries-wrapper
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
      "Sort org entries at point."
      (when (or org-autosort-sort-after-C-c-C-c force)
        (if org-autosort-sort-after-C-c-C-c-recursive
    	(org-autosort-sort-entries-at-point-recursive)
          (org-autosort-sort-entries-at-point-nonrecursive)
          )))
    
    (defun org-autosort-sort-entries-in-file (&optional force)
      "Sort all entries in the file recursively.  Do not respect org-autosort-sort-at-file-open if FORCE is non nil."
      (when (or org-autosort-sort-at-file-open force)
        (org-map-entries (lambda nil (funcall #'org-autosort-org-sort-entries-wrapper
    				   nil
    				   ?f
    				   #'org-autosort-construct-get-value-function
    				   #'org-autosort-construct-cmp-function))
    		     nil
    		     'file)))
    
    (add-hook 'org-mode-hook
    	  #'org-autosort-sort-entries-in-file)


<a id="org4f3d4bd"></a>

### File epilogue

    (provide 'org-autosort)
    
          ;;; org-autosort.el ends here


<a id="orge5dbee6"></a>

## NEXT Ideas


<a id="org6d6e0eb"></a>

### Sort items when opening org file, on edit??


<a id="orga9e85b3"></a>

### CANCELLED do not use org-sort, because it does not allow to combine sorts (i.e. sort by one criteria, if equal - by other)


<a id="org7466077"></a>

### allow to define sort criteria like a lisp function in the properties field


<a id="org7aff27a"></a>

### Do not sort only but filter items in org files/agenda


<a id="org4f2376b"></a>

### CANCELLED Take care about exact position for `C-c C-c` (say, we are inside the table - user may not want to sort)


<a id="orge43526c"></a>

### Sort only items, matching org search regex


<a id="org3d728e5"></a>

### Handle nothing to sort


<a id="org3aae133"></a>

### make interactive versions of sorting functions


<a id="org5d31964"></a>

### autosort - do not sort but show agenda


<a id="org72676ab"></a>

### add hooks to to autosort


<a id="org8a81abe"></a>

### auto add hooks according to the sort type - should be able to define hooks for every sort type


<a id="orgfd37580"></a>

### get rid of annoying unfolding after `org-sort`


<a id="org127a626"></a>

### put buffer name in error report for wrong element of sorting strategy


<a id="org07ac02f"></a>

### should be able to define alias in sorting strategy


<a id="org92cbef6"></a>

### NEXT rewrite sorting strategy to use assoc lists


<a id="org8504982"></a>

### use local hook in autosort for toggle hooks


<a id="org904669a"></a>

### add this functionality? [Sorting Org Mode lists using a sequence of regular expressions  13](http://sachachua.com/blog/2017/12/sorting-org-mode-lists-using-a-sequence-of-regular-expressions/)


<a id="org0c27c8e"></a>

### do not raise error but put a message and do not sort on wrong :SORTING: format


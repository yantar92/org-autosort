<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgde06795">1. <span class="todo NEXT">NEXT</span> Automatic sorting of items in org mode&#xa0;&#xa0;&#xa0;<span class="tag"><span class="ORGMODE">ORGMODE</span>&#xa0;<span class="EMACS">EMACS</span>&#xa0;<span class="COMMON">COMMON</span></span></a>
<ul>
<li><a href="#org44a9fad">1.1. Motivation</a></li>
<li><a href="#org5be1673">1.2. Overview</a></li>
<li><a href="#orgc28155b">1.3. Configuration</a></li>
<li><a href="#org50f23e8">1.4. Defaults</a></li>
</ul>
</li>
</ul>
</div>
</div>


<a id="orgde06795"></a>

# Automatic sorting of items in org mode     :ORGMODE:EMACS:COMMON:


<a id="org44a9fad"></a>

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

<div class="inlinetask">
<b><span class="todo TODO">TODO</span> Add some example here</b><br />
nil</div>

Of course, you can still use `org-sort` or agenda view with restriction
to the current subtree, but it may be disrupting if you want to look
through multiple of such a lists.  

The solution is to implement automatic sorting of subtrees in org
files.


<a id="org5be1673"></a>

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


<a id="orgc28155b"></a>

## Configuration

Both `:SORT:` property and `org-autosort-global-sorting-strategy`
are lists, which determine how to sort the entries.

`org-autosort-global-sorting-strategy` defined how to sort entries by
default. It is a list of [1.4](#orge8fc901), defining the comparison
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


<a id="org50f23e8"></a>

## Defaults

The package provide some predefined <a id="orge8fc901"></a>, all are listed in
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
    Sorting rules are applied accorting the their position in the list.
    nil means that no sorting should be done by default."
      :type '(choice symbol
    		 (plist :value-type (choise function
    					    (list function (repeat sexp))))))


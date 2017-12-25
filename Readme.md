<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgf71feb5">Automatic sorting of items in org mode</a>
<ul>
<li><a href="#orgc3bcda3">Motivation</a></li>
<li><a href="#orgb0bdc1a">Overview</a></li>
<li><a href="#orgcac81f8">Configuration</a></li>
<li><a href="#orgc2ce2bd">Defaults</a></li>
</ul>
</li>
</ul>
</div>
</div>


<a id="orgf71feb5"></a>

# Automatic sorting of items in org mode


<a id="orgc3bcda3"></a>

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

Of course, you can still use `org-sort` or agenda view with restriction
to the current subtree, but it may be disrupting if you want to look
through multiple of such a lists.  

The solution is to implement automatic sorting of subtrees in org
files.


<a id="orgb0bdc1a"></a>

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


<a id="orgcac81f8"></a>

## Configuration

Both `:SORT:` property and `org-autosort-global-sorting-strategy`
are lists, which determine how to sort the entries.

<a id="org97904cd"></a>
`org-autosort-global-sorting-strategy` defines how to sort entries by
default. It is a list of [sorting rules](#org6d3dec0), defining the comparison
between sorted entries. First, the entries are sorted via first rule
from the list. If the calculated keys are equal, second rule is used,
and so on.

`:SORT:` can be either `nil`, `t`, or the same format as
`org-autosort-global-sorting-strategy`:

-   **`t`:** use `org-autosort-global-sorting-strategy`
-   **not defined or `nil`:** sort if `org-autosort-sort-all` is non `nil`
-   **empty or `none`:** do not sort entries
-   **list:** define separate sorting strategy

The sorting can be done after:

-   opening the org file
-   `org-autosort-sort-entries-at-point` command
-   `org-autosort-sort-entries-in-file` command

(see [sorting triggers](#org6e106bb) for details)


<a id="orgc2ce2bd"></a>

## Defaults

The package provide some predefined sorting rules <a id="org6d3dec0"></a>,
all are listed in `org-autosort-functions-alist`.

    (defcustom org-autosort-functions-alist '((todo-up-0 . (:key org-autosort-get-todo :cmp <)) ; default org-sort comparison
    					  (todo-down-0 . (:key org-autosort-get-todo :cmp >))
    					  ;; compare according to `org-autosort-todo-cmp-order'
    					  (todo-up . (:key org-get-todo-state :cmp org-autosort-custom-cmp-todo))
    					  (todo-down . (:key org-get-todo-state :cmp (lambda (a b)
    										       (not (org-autosort-custom-cmp-todo a b)))))
    					  ;;					  
    					  (text-up . (:key org-autosort-get:cmp :cmp string<))
    					  (text-down . (:key org-autosort-get-text :cmp string>))
                                              (priority-up . (:key (org-autosort-get-property "PRIORITY") :cmp string<))
                                              (priority-down . (:key (org-autosort-get-property "PRIORITY") :cmp string>)))
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

You can control automatic sorting by setting <a id="org6e106bb"></a>

    (defcustom org-autosort-sort-at-file-open t
      "Non nil states for sorting of all items in the org file after opening."
      :type '(boolean))

Default [sorting strategy](#org97904cd) is

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


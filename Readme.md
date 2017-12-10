<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#org4ffb192">1. Motivation</a></li>
<li><a href="#org709d2c6">2. Overview</a></li>
<li><a href="#orgf0dc7eb">3. Configuration</a></li>
</ul>
</div>
</div>


<a id="org4ffb192"></a>

# Motivation

For now, standard org capabilities allow to sort entries in the agenda views automatically. However, sorting of the actual org files is rather limited. We only have `org=sort` function, which allows to sort by one criteria only (not like `org-agenda-sorting-strategy` with multiple sort criteria). Moreover, it can be done manually only. 

Sometimes, one may have a need to look at some kind of long list of TODO items (for example, this infinitely growing list of things you wish to do one day, or list of ideas). It is not comfortable to scroll across such kind of list to look for specific item. Sorting is something, that can help here. 

Of course, you can still you `org-sort` or agenda view with restriction to the current subtree, but it may be disrupting if you want to look through multiple of such a lists. 

The solution is to implement automatic sorting of subtrees in org files. 


<a id="org709d2c6"></a>

# Overview

This package aims to implement an automatic sorting of the subtrees in org files. The sorting order can be set globally through all the org files, locally in file, or locally in a subtree using `:SORT:` property. 

Everything, except global sorting order, can be set using standard inheritance capabilities of the org properties (file local, subtree local with or without inheritance for subtrees inside the subtree). Global sorting order can be set via `org-autosort-global-sorting-strategy` variable.


<a id="orgf0dc7eb"></a>

# Configuration

Both `:SORT:` property and `org-autosort-global-sorting-strategy` (default - `nil`) determine how to sort the entries.

Sorting is done using the first function in the list. If the keys, returned by function, are equal - comparison is done using keys, according to the second function from the list and so on.

The package provide some sorting functions, all are listed in `org-autosort-predicates-alist`.

The sorting can be done after opening the org file of after `C-c C-c`. `C-c C-c` will sort all entries in the file if the point is at the org file header or all entries in a subtree and its children subtrees if the point is at the org entry.

Sorting at file open can be controlled by `org-autosort-sort-at-file-open`. When not `nil` - sort all entries in the org file after opening. Default is `t`

Sorting after `C-c C-c` can be controlled by `org-autosort-sort-after-C-c-C-c`. When not `nil` - sort all entries in the subtree. Default is `t`. 

When `org-autosort-sort-after-C-c-C-c-recutsive` is not `nil`, sort subtrees of current tree as well. Default is `nil`.


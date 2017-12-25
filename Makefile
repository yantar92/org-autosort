org-autosort: org-autosort.el

%.elc : %.el
	rm -f $@
	emacs -Q --batch --eval '(byte-compile-file "$<")'

%.el : %.org
	rm -f $@
	emacs -q --batch --eval "(require 'ob-tangle)"  --eval '(org-babel-tangle-file "$<")'


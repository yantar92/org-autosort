org-autosort: org-autosort.el

docs: org-autosort.org
	emacs $< --batch -f org-md-export-to-markdown --kill

%.elc : %.el
	rm -f $@
	emacs -Q --batch --eval '(byte-compile-file "$<")'

%.el : %.org
	rm -f $@
	emacs -q --batch --eval "(require 'ob-tangle)"  --eval '(org-babel-tangle-file "$<")'


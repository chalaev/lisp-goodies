SBCL = ~/local/bin/sbcl
# where my local packages are stored:
quicklispDir = ~/quicklisp/local-projects/shalaev

$(quicklispDir)/shalaev.lisp: README.md generated/macros.el $(quicklispDir)
	echo "(eval-when (:compile-toplevel :load-toplevel :execute)" > $@
	cat generated/macros.lisp generated/functions.lisp generated/file-functions.lisp >> $@
	echo ")" >> $@
	cp -a shalaev.asd $(quicklispDir)/

$(quicklispDir):
	[ -d $(quicklispDir)] || mkdir $(quicklispDir)

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	-chgrp tmp $@

generated/macros.el: goodies.org
	emacsclient -e '(org-babel-tangle-file "goodies.org")'
	-chgrp tmp generated/*
	-chmod a-x generated/*
	-rsync -au generated/*.el ../cloud/goodies/
	# -rsync -au generated/macros.lisp ../simple-log/goodies/
	# -rsync -au generated/*.lisp ../signal-handler/goodies/

clean:
	-rm `find . -type f -group tmp`

.PHONY: clean all

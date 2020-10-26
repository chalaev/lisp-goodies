
all: README.md generated/macros.el

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	-chgrp tmp $@

generated/macros.el: goodies.org
	emacsclient -e '(org-babel-tangle-file "goodies.org")'
	-chgrp tmp generated/*
	-chmod a-x generated/*
	-rsync -au generated/* ../cloud/goodies/

clean:
	-rm `find . -type f -group tmp`

.PHONY: clean all

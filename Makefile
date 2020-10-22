
all: goodies.org
	emacsclient -e '(org-babel-tangle-file "goodies.org")'
	-chgrp tmp generated/*
	-chmod a-x generated/*

clean:
	-rm generated/*

.PHONY: clean all


all: goodies.org
	emacsclient -e '(org-babel-tangle-file "goodies.org")'
	-chgrp tmp generated/*
	-chmod a-x generated/*
	-rsync -avu generated/*.el ../cloud.el/goodies/

clean:
	-rm generated/*

.PHONY: clean all

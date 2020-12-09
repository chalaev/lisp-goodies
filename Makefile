SBCL = ~/local/bin/sbcl
# where my local packages are stored:
quicklispDir = $$HOME/quicklisp/local-projects/shalaev
headersDir = generated/headers

LFNs = macros files tests shalaev
LISPs = $(addsuffix .lisp, $(LFNs))
package = $(LISPs) shalaev.asd version.org

OFNs = shalaev packaging
ORGs = $(addsuffix .org, $(OFNs))

# unless I mention generated/from/*.org files here, they will be considered temporary and auto-erased so emacsclient will always be called on every make:
all: packaged/start.el quicklisp README.md packaged/el-shalaev.tbz packaged/shalaev.el packaged/cl-shalaev.tbz $(addprefix generated/from/, $(ORGs))
quicklisp: $(quicklispDir)/ $(addprefix $(quicklispDir)/, $(package)) $(addprefix generated/from/, $(ORGs))

packaged/el-shalaev.tbz: generated/from/shalaev.org packaged/
	@echo "\nTesting before we package it:"
	emacs --no-site-file --batch -l ert  --eval "(require 'cl)" -l generated/macros.el -l generated/functions.el -l generated/file-functions.el -l generated/load.el -l generated/cl.el -l generated/tests.el -f ert-run-tests-batch-and-exit
	@echo "`date '+%m/%d %H:%M'` EL TESTS PASSED :)\n"
	tar jcfv $@ --transform s/^generated/shalaev/ generated/*.el
	-@chgrp tmp $@

packaged/shalaev.el: version.org header.el packaged/
	sed "s/the-version/`head -n1 $<`/" header.el > $@
	cat generated/cl.el  generated/file-functions.el generated/functions.el generated/logging.el generated/macros.el >> $@
	echo "(provide 'shalaev)" >> $@
	echo ";;; shalaev.el ends here" >> $@
	emacsclient -e '(untilde (cdr (assoc "local-packages" package-archives)))' | xargs cp $@
	echo ";; -*- lexical-binding: t; -*-" > packaged/start.el
	echo "\n;; I load this file at startup\n"  >> packaged/start.el
	cat generated/local-packages.el generated/make.el generated/load.el >> packaged/start.el
	-@chgrp tmp $@

packaged/start.el: generated/local-packages.el generated/make.el generated/load.el packaged/
	echo ";; -*- lexical-binding: t; -*-" > $@
	echo "\n;; This file is a part of https://github.com/chalaev/lisp-goodies"  >> $@
	echo "\n;; I load this file at startup\n"  >> $@
	cat generated/local-packages.el generated/make.el generated/load.el >> $@
	-@chgrp tmp $@
	-for d in ../*/goodies/ ; do rsync -avu $@ $$d ; done

packaged/cl-shalaev.tbz: quicklisp packaged/
	@echo "\nTesting before we package it:"
	@$(SBCL) --eval "(asdf:operate 'asdf:test-op :shalaev)" --eval "(uiop:quit shalaev/tests:N-failed)"
	@echo "\n\n`date '+%m/%d %H:%M'` CL TESTS PASSED :)\n"
	tar jcfv $@ --directory=$(quicklispDir)/..  shalaev
	-@chgrp tmp $@

$(quicklispDir)/%.lisp: generated/from/shalaev.org generated/from/headers.org
	cat generated/headers/$(notdir $@) generated/$(notdir $@) > $@
	-@chgrp tmp $@

$(quicklispDir)/%.asd: %.asd
	cat $< > $@
	-@chgrp tmp $@

$(quicklispDir)/%.org: %.org
	cat $< > $@
	-@chgrp tmp $@

version.org: change-log.org helpers/derive-version.el
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (format-version "$<"))' | xargs > $@
	echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	echo "by [[file:helpers/derive-version.el][derive-version.el]]" >> $@
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/ generated/headers/
	@echo "\nNow emacs is probably waiting for your responce..."
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (printangle "$<"))' | xargs > $@
	-@chgrp tmp $@ `cat $@`
	-@chmod a-x `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	sed -i "s/\.md)/.org)/g"  $@
	-@chgrp tmp $@
	-@chmod a-x $@

clean:
	-$(SBCL) --quit --eval '(progn (asdf:clear-system :shalaev) (asdf:clear-system :shalaev/tests))'
	-rm -r $(quicklispDir) generated packaged version.org

.PHONY: clean quicklisp all

%/:
	[ -d $@ ] || mkdir -p $@

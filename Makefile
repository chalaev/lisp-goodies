SBCL = ~/local/bin/sbcl
# where my local packages are stored:
quicklispDir = $$HOME/quicklisp/local-projects/shalaev
headersDir = generated/headers

LFNs = macros files tests shalaev
LISPs = $(addsuffix .lisp, $(LFNs))
package = $(LISPs) shalaev.asd

OFNs = shalaev packaging
ORGs = $(addsuffix .org, $(OFNs))

# unless I mention generated/from/*.org files here, they will be considered temporary and auto-erased so emacsclient will always be called on every make:
all: quicklisp README.md generated/shalaev.tbz $(addprefix generated/from/, $(ORGs)) git
quicklisp: $(quicklispDir)/ $(addprefix $(quicklispDir)/, $(package)) $(addprefix generated/from/, $(ORGs))

generated/shalaev.tbz: quicklisp
	@echo "Testing before we package it:"
	@$(SBCL) --eval "(asdf:operate 'asdf:test-op :shalaev)" --eval "(uiop:quit shalaev/tests:N-failed)"
	@echo "\n\n`date '+%m/%d %H:%M'` ALL TESTS PASSED :)\n"
	tar jcfv $@ --directory=$(quicklispDir)/..  shalaev
	-chgrp tmp $@

$(quicklispDir)/%.lisp: generated/from/shalaev.org generated/from/packaging.org
	cat generated/headers/$(notdir $@) generated/$(notdir $@) > $@
	-chgrp tmp $@

$(quicklispDir)/%.asd: %.asd
	cat $< > $@
	-chgrp tmp $@

$(quicklispDir)/%.org: %.org
	cat $< > $@
	-chgrp tmp $@

generated/from/%.org: %.org generated/from/ generated/headers/
	echo `emacsclient -e '(printangle "$<")'` | sed 's/"//g' > $@
	-chgrp tmp $@ `cat $@`
	-chmod a-x `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	-chgrp tmp $@
	-chmod a-x $@

clean:
	-$(SBCL) --quit --eval '(progn (asdf:clear-system :shalaev) (asdf:clear-system :shalaev/tests))'
	-rm -r $(quicklispDir) generated

.PHONY: clean quicklisp all git

%/:
	[ -d $@ ] || mkdir -p $@

git: generated/shalaev.tbz next-commit.txt README.md
	@echo "===="
	@echo "git commit -am '"`head -n1 next-commit.txt`"'"
	@echo "git push origin master"

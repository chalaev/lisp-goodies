SBCL = ~/local/bin/sbcl
# where my local packages are stored:
quicklispDir = $$HOME/quicklisp/local-projects/shalaev
headersDir = generated/headers

all: test README.md

test: $(quicklispDir)/shalaev.lisp $(quicklispDir)/files.lisp $(quicklispDir)/macros.lisp $(quicklispDir)/tests.lisp $(quicklispDir) shalaev.asd
	rsync -avu shalaev.asd $(quicklispDir)/
	-chgrp tmp $(quicklispDir)/*
	-chmod a-x $(quicklispDir)/*
	@echo "Starting tests..."
	@$(SBCL) --eval "(asdf:operate 'asdf:test-op :shalaev)" --eval "(uiop:quit shalaev/tests:N-failed)"
	@echo "\n\nALL TESTS PASSED :)\n"
	tar jcfv generated/cl-package.tbz --directory=$(quicklispDir)/..  shalaev

# I am annnoyed by these GNU make restrictions that force me to write false dependences, see my el-make project:
$(quicklispDir)/shalaev.lisp: generated/headers/macros.lisp generated/macros.lisp
	cat generated/headers/shalaev.lisp > $@

$(quicklispDir)/files.lisp: generated/headers/macros.lisp generated/macros.lisp
	cat generated/headers/files.lisp generated/files.lisp > $@

$(quicklispDir)/macros.lisp: generated/headers/macros.lisp generated/macros.lisp
	cat generated/headers/macros.lisp generated/macros.lisp > $@

$(quicklispDir)/tests.lisp: generated/headers/macros.lisp generated/macros.lisp
	cat generated/headers/tests.lisp generated/tests.lisp > $@

generated/headers/macros.lisp: headers.org generated/macros.el $(headersDir)
	emacsclient -e '(org-babel-tangle-file "headers.org")'
	-chmod a-x generated/dot.* generated/*.lisp generated/*/*.lisp generated/*.el

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown) (kill-buffer))'
	-chgrp tmp $@

generated/macros.el: goodies.org $(headersDir)
	emacsclient -e '(org-babel-tangle-file "goodies.org")'
	-chgrp -R tmp generated
	-chmod a-x generated/dot.* generated/*.lisp generated/*.el
	-rsync -au generated/*.el ../cloud/goodies/

clean:
	-rm -rf $(quicklispDir)/* generated/*

.PHONY: clean test all

$(quicklispDir):
	[ -d $(quicklispDir) ] || mkdir $(quicklispDir)

$(headersDir):
	[ -d $(headersDir) ] || mkdir -p $(headersDir)

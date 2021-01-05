;; -*-  lexical-binding: t; -*-
;;
;; see https://github.com/chalaev/lisp-goodies/blob/master/packaged/batch-start.el
(load-file "/path-to/batch-start.el")

;; creating PID file for emacs; useful when it gets started from ~/.login
;; see https://github.com/chalaev/lisp-goodies/blob/master/.login
(make-temp-file "emacs-" nil ".pid" (format "%d
" (emacs-pid))); requires version(emacs) > 26

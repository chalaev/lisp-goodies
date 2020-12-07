;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;;
;; creating PID file for emacs; useful when it gets started from ~/.login
;; see https://github.com/chalaev/lisp-goodies/blob/master/.login
(make-temp-file "emacs-" nil ".pid" (format "%d
" (emacs-pid))); requires version(emacs) > 26

;; see https://github.com/chalaev/lisp-goodies/blob/master/packaged/start.el
(load-file "/path-to/start.el")

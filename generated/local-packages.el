;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
(defvar ~ (file-name-as-directory (getenv "HOME")))
(defun tilde(x) (replace-regexp-in-string (concat "^" ~) "~/" x))
(defun untilde(x) (replace-regexp-in-string "^~/" ~ x))
(defvar emacs-d (concat "~/" (file-name-as-directory ".emacs.d")))

(require 'package)
(unless (assoc "local-packages" package-archives)
  (push (cons  "local-packages" (concat emacs-d (file-name-as-directory "local-packages")))
	package-archives))
(make-directory (cdr (assoc "local-packages" package-archives)) t)

(unless (member (cdr (assoc "local-packages" package-archives)) load-path)
  (add-to-list 'load-path (cdr (assoc "local-packages" package-archives))))

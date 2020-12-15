(unless (functionp 'caddr) (defun caddr(x) (car(cddr x)))); for emacs versions <26
(unless (functionp 'cadar) (defun cadar(x) (car (cdar x))))

(defun emacs-ver()
  (mapcar #'string-to-number (split-string
   (caddr (split-string (emacs-version))) "\\.")))

(unless (< 25 (car (emacs-ver)))
  (defun upgrade-make-temp-file(old-function PREFIX &optional DIR-FLAG SUFFIX TEXT)
    (let((FN (funcall old-function PREFIX DIR-FLAG SUFFIX)))
      (when (and TEXT (stringp TEXT))
      (write-region TEXT nil FN))
    FN))
(add-function :around (symbol-function 'make-temp-file) #'upgrade-make-temp-file))

;; -*-  lexical-binding: t; -*-
(defvar ~ (file-name-as-directory (getenv "HOME")))
(defun   tilde(x) (replace-regexp-in-string (concat "^" ~) "~/" x))
(defun untilde(x) (replace-regexp-in-string "^~/" ~ x))
(defvar emacs-d (concat "~/" (file-name-as-directory ".emacs.d")))

(require 'package)
(unless (assoc "local-packages" package-archives)
  (push (cons  "local-packages" (concat emacs-d (file-name-as-directory "local-packages")))
	package-archives))
(make-directory (cdr (assoc "local-packages" package-archives)) t)

(unless (member (cdr (assoc "local-packages" package-archives)) load-path)
  (add-to-list 'load-path (cdr (assoc "local-packages" package-archives))))

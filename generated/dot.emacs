(make-temp-file "emacs-" nil ".pid" (format "%d
" (emacs-pid))); requires version(emacs) > 26

;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
(defvar ~ (file-name-as-directory (getenv "HOME")))
(defvar emacs-d (concat ~ (file-name-as-directory ".emacs.d")))

(unless (assoc "local-packages" package-archives)
  (push (cons  "local-packages" (concat emacs-d (file-name-as-directory "local-packages")))
	package-archives))
(make-directory (cdr (assoc "local-packages" package-archives)) t)

(unless (member (cdr (assoc "local-packages" package-archives)) load-path)
  (add-to-list 'load-path (cdr (assoc "local-packages" package-archives))))

(defvar *loaded* nil "prevents duplicate evaluation of files")
(defun load-file* (x &optional el-prefix)
  (let ((FN (file-chase-links 
             (if (= ?/ (aref x 0)) x
               (concat (or el-prefix 
 (file-name-as-directory (getenv "HOME")))
x)))))
    (unless (member (car (last (split-string FN "\\." t))) '("el" "elc"))
      (setf FN (concat FN ".el")))
    (unless (member FN *loaded*) (load-file FN) (push (barename FN) *loaded*))))

(require 'shalaev) ; ← needed for perms-from-str
(defun after-tangle()
  "mark tangled files as non-backupable (chgrp tmp files) and non-excecutable"
  (let ((FN (buffer-file-name)))

(set-file-modes FN (logand #o666 (perms-from-str (nth 8 (file-attributes FN 'string)))))
    (chgrp "tmp" FN)))
(add-hook 'org-babel-post-tangle-hook #'after-tangle)

(defun printangle(FN)
  "to be used in Makefile instead of org-babel-tangle-file"
  (let ((l (length default-directory)))
    (apply #'concat (mapcar #'(lambda(x) (substring (format "%s " x) l)) (org-babel-tangle-file FN)))))

(defun run-init-block ()
  (org-babel-goto-named-src-block "init")
  (org-babel-execute-src-block))

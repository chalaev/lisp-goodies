;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; Generated from https://notabug.org/shalaev/lisp-goodies/src/master/goodies.org
;; See explanations therein. Edit this code before using it.

(defun barename (FN)
  (let ((SS (split-string (file-name-nondirectory FN)  "\\." t)))
    (mapconcat #'(lambda(x)x) (butlast SS) ".")))

(defvar *loaded* nil "prevents duplicate evaluation of files")
(defun load-file* (x &optional el-prefix)
  (let ((FN (file-chase-links 
             (if (= ?/ (aref x 0)) x
               (concat (or el-prefix 
 (concat (getenv "HOME") "/"))
x)))))
    (unless (member (car (last (split-string FN "\\." t))) '("el" "elc"))
      (setf FN (concat FN ".el")))
    (unless (member FN *loaded*) (load-file FN) (push (barename FN) *loaded*))))

(mapcar #'(lambda(x) (load-file* x "~/programming/emacs/"))
	'("macros" "functions" "logging"  ....))

(defun after-tangle()
  "mark tangled files as non-backupable (chgrp tmp files) and non-excecutable"
  (let ((FN (buffer-file-name)))

(set-file-modes FN (logand #o666 (perms-from-str (nth 8 (file-attributes FN 'string)))))
    (chgrp "tmp" FN)))
(add-hook 'org-babel-post-tangle-hook #'after-tangle)

(defun run-init-block ()
  (org-babel-goto-named-src-block "init")
  (org-babel-execute-src-block))

;;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-

;; Generated from https://github.com/chalaev/elisp-goodies/blob/master/goodies.org
;; See explanations therein.
;; Do not forget to edit this file before using it.

(defun basename (FN) (car(last(split-string FN "/" t))))

(defun barename (FN)
  (let ((SS (split-string (basename FN)  "\\." t)))
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
  "tangled files are temporary, should not be backuped"
  (let ((FN (buffer-file-name)))

(set-file-modes FN (logand #o666 (perms-from-str (nth 8 (file-attributes FN 'string)))))
    (chgrp "tmp" FN)))
(add-hook 'org-babel-post-tangle-hook #'after-tangle)

(defun safe-mkdir (dirname)
"creates a directory returning the report"
(condition-case err
  (progn (make-directory dirname)  (list t))
 (file-already-exists (cons nil :exists))
 (file-error (cons nil :permission))))

;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; generated from https://notabug.org/shalaev/lisp-goodies/src/master/shalaev.org
(defun chgrp(group file-name)
  (= 0 (call-process "chgrp" nil nil nil group file-name)))

(defun rm(FN)
"erases files only, not directories"
  (condition-case err (cons t (delete-file FN))
    (file-error (cons nil (error-message-string err)))))

(defun safe-delete-dir (FN &optional recursive)
  (condition-case err (progn (delete-directory FN recursive) (list t))
    (file-error (cons nil (error-message-string err)))))

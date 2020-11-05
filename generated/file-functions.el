;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; generated from https://notabug.org/shalaev/elisp-goodies/src/master/goodies.org
(defun chgrp(group file-name)
  (= 0 (call-process "chgrp" nil nil nil group file-name)))

(defun safe-mkdir (dirname)
  (if (file-exists-p dirname)
    (cons nil (if (file-directory-p dirname) :exists :file))
    (condition-case err
        (progn (make-directory dirname t) (list t))
      (file-already-exists (cons nil :strange))
      (file-error (cons nil :permission)))))

(defun safe-delete-file (FN)
  (condition-case err (progn (delete-file FN) (list t))
    (file-error (cons nil (error-message-string err)))))

(defun safe-delete-dir (FN)
  (condition-case err (progn (delete-directory FN) (list t))
    (file-error (cons nil (error-message-string err)))))

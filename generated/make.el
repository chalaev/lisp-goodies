(require 'shalaev) ; ← needed for perms-from-str unless you have loaded it in another way
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
"runs code block labeled 'init' when an org-file is opened in emacs"
  (org-babel-goto-named-src-block "init")
  (org-babel-execute-src-block))

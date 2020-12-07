(defun barename (FN)
  (let ((SS (split-string (file-name-nondirectory FN)  "\\." t)))
    (mapconcat #'(lambda(x)x) (butlast SS) ".")))

(let (loaded); prevents duplicate evaluation of files
(defun load* (x &optional el-prefix)
  (let ((FN (tilde (file-chase-links (concat (or el-prefix "~/") x)))))
    (unless (member (car (last (split-string FN "\\." t))) '("el" "elc"))
      (setf FN (concat FN ".el")))
    (unless (member FN loaded) (load-file FN) (push (barename FN) loaded)))))

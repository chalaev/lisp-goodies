;; -*-  lexical-binding: t; -*-
(defun safe-mkdir(dirname)
"creates a directory returning the report"
(condition-case err
  (progn (make-directory dirname t)  (list t))
 (file-already-exists (cons nil :exists))
 (file-error (cons nil :permission))))

(defun ensure-dir-exists (dirname)
(let ((SMD (safe-mkdir dirname)))
  (or (car SMD) (eql (cdr SMD) :exists))))

(require 'cl); hopefully one day I will remove this line
(defun perms-from-str (str)
"parses file mode string into integer"
  (let ((text-mode (reverse (cdr (append str nil)))) (mode 0) (fac 1))
    (loop for c in text-mode for i from 0
          unless (= c ?-) do (s-incf mode fac)
          do (setf fac (* 2 fac)))
    mode))

(defun perms-to-str(file-mode)
"formats integer file mode into string"
(let ((ll '((1 . 0))))
  (apply #'concat (mapcar
		   #'(lambda(x) (format "%c" (if (= 0 (logand file-mode (car x))) ?- (aref "xwr" (cdr x)))))
  (dotimes (i 8 ll)
     (push (cons (* 2 (caar ll)) (mod (1+ i) 3))  ll))))))

(defun chgrp(group file-name)
  (= 0 (call-process "chgrp" nil nil nil group file-name)))

(defun mv(FN-1 FN-2)
"renaming/moving files (not dirs)"
  (condition-case err (cons t (rename-file FN-1 FN-2 t))
    (file-error (cons nil (error-message-string err)))))

(defun cp(FN-1 FN-2)
"copying ONE file (not dirs)"
  (condition-case err (cons t (copy-file FN-1 FN-2 t))
    (file-error (cons nil (error-message-string err)))))

(defun rm(FN)
"erases files only, not directories"
  (condition-case err (cons t (delete-file FN))
    (file-error (cons nil (error-message-string err)))))

(defun safe-delete-dir (FN &optional recursive)
  (condition-case err (progn (delete-directory FN recursive) (list t))
    (file-error (cons nil (error-message-string err)))))

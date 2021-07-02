;; -*-  lexical-binding: t; -*-
(require 'cl-lib); hopefully one day I will remove this line
(defun perms-from-str (str)
"parses file mode string into integer"
  (let ((text-mode (reverse (cdr (append str nil)))) (mode 0) (fac 1))
    (cl-loop for c in text-mode for i from 0
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
  (= 0 (call-process "chgrp" nil nil nil group (untilde file-name))))

(defun get-file-properties(FN)
  (when-let((FA (and (file-exists-p FN) (file-attributes FN 'string))))
      (cl-destructuring-bind
	  (uid gid acess-time mod-time status-time fsize ms void inode fsNum)
	  (cddr FA)
(vector FN uid gid mod-time fsize (perms-from-str ms)))))

(defun ensure-dir-exists (DN)
  (condition-case err
      (progn (make-directory DN t) DN)
    ;; (file-already-exists (clog :debug "%s already exists" DN))
    (file-error (clog :debug "cannot create %s" DN))))

(defun FN(FN0 &rest other-FN-parts)
"concatenates arguments into file name inside (sub)directory"
(if (car other-FN-parts)
    (apply #'FN
(cons 
  (concat (file-name-as-directory FN0) (car other-FN-parts))
  (cdr other-FN-parts)))
  FN0))
(defun to-dir(root &rest dirs)
(if (car dirs)
    (apply #'to-dir
(cons 
  (file-name-as-directory (concat (file-name-as-directory root) (car dirs)))
  (cdr dirs)))
  (file-name-as-directory root)))
(defun need-dir(&rest DNs)
  (ensure-dir-exists (untilde(apply #'to-dir DNs))))
(defvar *config-directory* (need-dir *emacs-d* "conf") "where config files for el-packages are stored")

(defun cat-file(FN)
"converts file to string"
(with-temp-buffer
    (insert-file-contents FN)
    (buffer-string)))

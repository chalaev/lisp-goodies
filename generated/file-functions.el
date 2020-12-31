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

(defun get-file-properties(FN)
  (when-let ((FA (and (file-exists-p FN) (file-attributes FN 'string))))
      (destructuring-bind
	  (uid gid acess-time mod-time status-time fsize ms void inode fsNum)
	  (cddr FA)
(vector FN uid gid mod-time fsize (perms-from-str ms)))))

(defun ensure-dir-exists (DN)
(condition-case err
(make-directory DN t)
(file-already-exists (clog :debug "%s already exists" DN)))
DN)

(defun to-dir(root &rest dirs)
(if (car dirs)
    (apply #'to-dir
(cons 
  (file-name-as-directory (concat (file-name-as-directory root) (car dirs)))
  (cdr dirs)))
  (file-name-as-directory root)))

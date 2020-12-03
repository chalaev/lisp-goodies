;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
;; generated from https://notabug.org/shalaev/lisp-goodies/src/master/shalaev.org
;; Some day this file will probably replace standard cl.el in my projects
(let ((counter 0))
  (defun gensym(&optional starts-with)
    "for those who miss gensym from Common Lisp"
    (unless starts-with (setf starts-with "gs"))
    (let (sym)
      (while (progn
               (setf sym (make-symbol (concat starts-with (number-to-string counter))))
               (or (special-form-p sym) (functionp sym) (macrop sym) (boundp sym)))
        (incf counter))
      (incf counter)
      sym)))

(defun s-find(item seq &optional key test)
  (let ((test (or test #'=)))
    (dolist (CS seq)
      (when (funcall test item (if key (funcall key CS) CS))
	(return CS)))))

(unless (or (boundp 'decf) (functionp 'decf) (macrop 'decf))
(defmacro decf (var &optional amount)
  (unless amount (setf amount 1))
  `(setf ,var (- ,var ,amount))))

(unless (or (boundp 'incf) (functionp 'incf) (macrop 'incf))
(defmacro incf (var &optional amount)
  (unless amount (setf amount 1))
  `(setf ,var (+ ,var ,amount))))

(defmacro flet(fun-defs &rest body)
(let ((GSs (mapcar #'(lambda(FD) (cons (car FD) (gensym))) fun-defs)))
`(let ,(mapcar #'(lambda(FD)
(list (cdr (assoc (car FD) GSs))
`(lambda ,(cadr FD) ,@(cddr FD)))) fun-defs)
(macrolet ,(mapcar #'(lambda(FD)
(list (car FD) (cadr FD) `(funcall ,(cdr (assoc (car FD) GSs)) ,@(cadr FD)))) fun-defs)
 ,@body))))

(defun s-select (from-where match-test)
  "select items matching the test"
    (let (collected)
       (dolist (list-item from-where)
	 (when (funcall match-test list-item)
	   (push list-item collected)))
      (reverse collected)))

(defun without(source &rest wrong-items)
  "returns (copy of) source without wrong-items"
  (s-select source #'(lambda(x) (not (member x wrong-items)))))

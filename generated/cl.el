;; -*-  lexical-binding: t; -*-
(let ((counter 0))
  (defun s-gensym(&optional starts-with)
    "for those who miss s-gensym from Common Lisp"
    (unless starts-with (setf starts-with "gs"))
    (let (sym)
      (while (progn
               (setf sym (make-symbol (concat starts-with (number-to-string counter))))
               (or (special-form-p sym) (functionp sym) (macrop sym) (boundp sym)))
        (s-incf counter))
      (s-incf counter)
      sym)))

(defun s-find(item seq &optional key test)
  (let ((CS(car seq)) found (test (or test 
(cond
  ((stringp item) #'string=)
  ((numberp item) #'=)
  (t #'eq)))))
  (while
     (and
       (not (setf found (funcall test item (if key (funcall key CS) CS))))
       (setf seq (cdr seq)))
     (setf CS(car seq)))
     (when found CS)))

(defmacro s-decf (var &optional amount)
  (unless amount (setf amount 1))
  `(setf ,var (- ,var ,amount)))

(defmacro s-incf (var &optional amount)
  (unless amount (setf amount 1))
  `(setf ,var (+ ,var ,amount)))

(defmacro s-flet(fun-defs &rest body)
(let ((GSs (mapcar #'(lambda(FD) (cons (car FD) (s-gensym))) fun-defs)))
`(let ,(mapcar #'(lambda(FD)
(list (cdr (assoc (car FD) GSs))
`(lambda ,(cadr FD) ,@(cddr FD)))) fun-defs)
(macrolet ,(mapcar #'(lambda(FD)
(list (car FD) (cadr FD) `(funcall ,(cdr (assoc (car FD) GSs)) ,@(cadr FD)))) fun-defs)
 ,@body))))

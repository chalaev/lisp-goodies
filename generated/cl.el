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

(defmacro letf(var-defs &rest body)
  (if(car var-defs)
      (let((ME (macroexpand-1 `(letf ,(cdr var-defs) ,@body))))
      (if(and(listp (car var-defs))(eql 'defun (caar var-defs)))
	  (let((func-data (cdar var-defs)))
	    `(let((,(car func-data) (lambda ,(cadr func-data) ,@(cddr func-data))))
	        ,ME))
	`(let(,(car var-defs)) ,ME)))
    `(progn ,@body)))

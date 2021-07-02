(defmacro cond-let(&rest conds)
  "cond with let"
  (let ((c (car conds)) (r (cdr conds)))
    (if (equal (car c) 'otherwise) `(progn ,@(cdr c))
    (if r
	`(if-let ,(car c) (progn ,@(cdr c)) ,(macroexpand-1 `(cond-let ,@r)))
	`(when-let ,(car c) ,@(cdr c))))))

(defmacro when-let (vars &rest body)
  "when with let using standard let-notation"
  (if (caar vars)
  `(let ((,(caar vars) ,(cadar vars)))
     ,(if (cdr vars)
	  `(when ,(caar vars)
	     ,(macroexpand-1 `(when-let ,(cdr vars) ,@body)))
	(append `(when ,(caar vars)) body)))
  (if (cdr vars)
      `(when ,(cadar vars)
	     ,(macroexpand-1 `(when-let ,(cdr vars) ,@body)))
    (append `(when ,(cadar vars)) body))))

(defmacro if-let (vars ifyes &rest body)
  "if with let using standard let-notation"
  (let ((if-true (gensym "it")) (result (gensym "r")))
    `(let (,if-true ,result)
       (when-let ,vars
		 (setf ,if-true t
		       ,result ,ifyes))
       (iff ,if-true ,result ,@body))))

(defmacro ifn-let (vars ifno &rest body)
  `(if-let ,vars
      (progn ,@body)
      ,ifno))

(defmacro needs(vardefs &rest body)
  "unifying when-let and if-let"
  (let ((vardef (car vardefs)))
    (if (and (listp vardef) (not (functionp (car vardef))))
    `(let ((,(car vardef) ,(cadr vardef)))
       ,(if (cddr vardef)
	    `(if ,(car vardef)
		,(if (cdr vardefs)
		     (macroexpand-1 `(needs ,(cdr vardefs) ,@body))
		   `(progn ,@body))
	       ,(car (cddr vardef)))
	  (append `(when ,(car vardef))
		  (if (cdr vardefs)
		      (list (macroexpand-1 `(needs ,(cdr vardefs) ,@body)))
		    body))))
    (append `(when ,vardef)
		  (if (cdr vardefs)
		      (list (macroexpand-1 `(needs ,(cdr vardefs) ,@body)))
		    body)))))

(defmacro case* (expr test &rest cases)
  "case with arbitrary test function"
  (let ((v (gensym "v")))
    `(let ((,v ,expr))
       (cond
        ,@(mapcar #'(lambda (VR)
(let ((val (car VR)) (rest (cdr VR)))
  (if (eql val 'otherwise)
      `(t ,@rest)
    `((,test ,v ,val) ,@rest))))
 cases)))))

(defmacro case-expand (input (&key (test #'char=)) &body cases)
  "same as case, allows macros as entries (marked with keyword expand)"
  (let ((block-sym (gensym "block")))
    `(block ,block-sym
       ,@(loop for (value . body) in cases
	    if (eql value 'otherwise)
	    collect `(return-from ,block-sym (progn ,@body))
	    else if (eql value 'expand)
	    collect
	      (destructuring-bind (args . code) (macroexpand-1 body)
		`(when ,(if (consp args); ← might be a list or one value
			    `(apply ,input-function ,args)
			    `(funcall ,input-function ,args))
		   (return-from ,block-sym (progn ,@code))))
	    else
	    collect `(when (funcall ,test ,input ,value)
		       (return-from ,block-sym (progn ,@body)))))))

(defmacro case-f (input-function r &body cases)
  "Same as case-expand, but the first arg is function applied to each case,
and the variable r contains the result of such application."
  (let ((block-sym (gensym "block")))
    `(block ,block-sym
       ,@(loop for (value . body) in cases
	    if (eql value 'otherwise)
	    collect `(return-from ,block-sym (progn ,@body))
	    else if (eql value 'expand)
	    collect
	      (destructuring-bind (args . code) (macroexpand-1 body)
		`(when ,(if (consp args); ← might be a list or one value
			    `(apply ,input-function ,args)
			    `(funcall ,input-function ,args))
		   (return-from ,block-sym (progn ,@code))))
	    else
	    collect
	      `(progn
		 (setf ,r (funcall ,input-function ,value))
		 (when ,r (return-from ,block-sym (progn ,@body))))))))

(defmacro iff (test-form then &rest else)
  "elisp-kind of if"
  (if (cdr else)
      `(if ,test-form ,then (progn ,@else))
      (if (car else)
	  `(if ,test-form ,then ,@else)
	  `(when ,test-form ,then))))

(defmacro ifn (test ifnot &rest ifyes)
`(iff (not ,test) ,ifnot ,@ifyes))

(defmacro concat (&rest strs)
  `(concatenate 'string ,@strs))

(defmacro aset(arr pos val)
  `(setf (aref ,arr ,pos) ,val))

(defmacro hset(arr pos val)
  `(setf (gethash ,pos ,arr) ,val))

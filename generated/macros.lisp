(defmacro ifn-let (vars ifno &rest body)
  `(if-let ,vars
      (progn ,@body)
      ,ifno))

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

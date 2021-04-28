;; -*-  lexical-binding: t; -*-
(require 'subr-x)

(unless (< 25 (car (emacs-ver)))
(defmacro when-let-key(key vars &rest body)
  "when with let using standard let-notation, but every item in vars must be a list"
  (if (car vars)
  `(let ((,(caar vars) ,(cadar vars)))
     ,(if (cdr vars)
	  `(when (funcall ,key ,(caar vars))
	     ,(macroexpand-1 `(when-let-key ,key ,(cdr vars) ,@body)))
	(append `(when (funcall ,key ,(caar vars))) body)))
  (if (cdr vars)
      `(when ,(cadar vars)
	     ,(macroexpand-1 `(when-let-key ,key ,(cdr vars) ,@body)))
    (append `(when (funcall ,key ,(cadar vars))) body)))))

(unless (< 25 (car (emacs-ver)))
(defmacro when-let*(vars &rest body)
  "when with let using standard let-notation"
`(when-let-key #'identity
   ,(mapcar #'(lambda(v) (if(listp v) v (list v nil))) vars)
    ,@body)))

(defmacro sif-let (vars ifyes &rest body)
  "if with let using standard let-notation"
  (let ((if-true (s-gensym "it")) (result (s-gensym "r")))
    `(let (,if-true ,result)
       (when-let* ,vars
		 (setf ,if-true t
		  ,result ,ifyes))
       (if ,if-true
	   ,result
	 ,@body))))

(defmacro ifn-let (vars ifno &rest body)
  `(if-let ,vars
      (progn ,@body)
      ,ifno))

(defmacro needs(vardefs &rest body)
  "unifying when-let and if-let"
  (let((vardef (car vardefs)))
;; was →    (if(and (listp vardef) (not (or (special-form-p (car vardef)) (functionp (car vardef)) (macrop (car vardef)))))
;; ← 03/22 replaced by
    (if(listp vardef)
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

(defmacro first2(ll) `(firstN ,ll 2))
(defmacro needs-set (vardefs &rest body)
  "needs with 'let' being replaced with 'setf'"
  (let ((vardef (car vardefs)))
    (if (cddr vardef)
      `(if-set (,(first2 vardef))
	  ,(if (cdr vardefs)
	       (macroexpand-1 `(needs-set ,(cdr vardefs) ,@body))
	     `(progn ,@body))
	  ,(caddr vardef))
      `(when-set (,(car vardefs))
	   ,(if (cdr vardefs)
	       (macroexpand-1 `(needs-set ,(cdr vardefs) ,@body))
	      `(progn ,@body))))))

(defmacro directory-lock(locked-dir by &rest body)
(let ((LD (s-gensym "LD")) (lock-file (s-gensym "LF")))
`(let* ((,LD (file-name-as-directory ,locked-dir))
        (,lock-file (concat ,LD "by")))
 (make-directory ,LD t)
 (write-region ,by nil ,lock-file)
(prog1 (progn ,@body)
(delete-file ,lock-file)
(delete-directory ,LD)))))

(defmacro drop (from-where &rest what)
`(setf ,from-where (without ,from-where ,@what)))

(defmacro case* (expr test &rest cases)
  "case with arbitrary test function"
  (let ((v (s-gensym "v")))
    `(let ((,v ,expr))
       (cond
        ,@(mapcar #'(lambda (VR)
(let ((val (car VR)) (rest (cdr VR)))
  (if (eql val 'otherwise)
      `(t ,@rest)
    `((,test ,v ,val) ,@rest))))
 cases)))))

(defmacro when-set (vars &rest body)
  "when-let using global variable instead of defining local one"
(let ((GV (s-gensym)))
  `(let ((,GV ,(cadar vars)))
     ,(if (cdr vars)
	  `(when ,GV
              (setf ,(caar vars) ,GV)
	     ,(macroexpand-1 `(when-set ,(cdr vars) ,@body)))
	(append `(when ,GV (setf ,(caar vars) ,GV)) body)))))

(defmacro unless-set (vars &rest body)
  "unless-let using global variable instead of defining local one"
(let ((GV (s-gensym)))
  `(let ((,GV ,(cadar vars)))
     ,(if (cdr vars)
	  `(if ,GV
              (setf ,(caar vars) ,GV)
	     ,(macroexpand-1 `(unless-set ,(cdr vars) ,@body)))
	(append `(if ,GV (setf ,(caar vars) ,GV)) body)))))

(defmacro if-set (vars &rest body)
  (let((if-true (s-gensym "it")) (result (s-gensym "r")))
    `(let(,if-true ,result)
       (setf ,result (when-set ,vars
		  (setf ,if-true t)
		  ,(car body)))
       (if ,if-true ,result
	 ,@(cdr body)))))

(defmacro ifn-set (vars ifno &rest body)
`(if-set ,vars
   (progn ,@body)
   ,ifno))

(defmacro cond-let (&rest conds)
  "cond with let"
  (let ((c (car conds)) (r (cdr conds)))
    (if (equal (car c) 'otherwise) `(progn ,@(cdr c))
    (if r
	`(if-let ,(car c) (progn ,@(cdr c)) ,(macroexpand-1 `(cond-let ,@r)))
	`(when-let ,(car c) ,@(cdr c))))))

(defmacro error-in(where &rest body)
"handles unrecognized errors"
`(condition-case err (progn ,@body)
   (error(clog :error (concat "error in " ,where " because
%s") (error-message-string err)))))

(defmacro ifn (test ifnot &rest ifyes)
`(if (not ,test) ,ifnot ,@ifyes))

(defmacro end-push (what where)
"adds an item to the end of the list, resembles 'add-to-list'"
  `(if ,where (push ,what (cdr (last ,where)))
      (push ,what ,where)))

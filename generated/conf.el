;; -*-  lexical-binding: t; -*-
(defmacro while-let(var-defs while-cond &rest body)
  `(let* (,@var-defs)
     (while ,while-cond
       ,@body)))

(defun read-conf-file(FN &optional max-size)
  "reads configuration file"
  (error-in "read-conf-file"
(with-temp-buffer(insert-file-contents (untilde FN))
(setf buffer-read-only t)
(let(res)					      
  (dolist(str(split-string (buffer-string) "\n"))
    (clog :debug "str= %s" str)
	    (unless(or
(string= "
" str)
(= ?# (string-to-char str))); ignoring comments or empty lines
(if(string-match "^\\(\\ca+\\)=\\([[:print:]]+\\)$" str)
	  (let((key(intern(match-string 1 str))) (val(match-string 2 str)))
	    (if(assoc key res)
		(setcdr (assoc key res) val)
	      (push (cons key val) res)))
(unless(string= "" str)
	(clog :warning "invalid string in %s: %s" FN str)))))
(reverse res)))))

(defun parse-parameter(str &optional par-type)
"for =parse-conf=: optionally parses first (string) argument into the specified type"
  (when str
    (cond
     ((eql par-type 'integer) (string-to-number str))

((consp par-type) (mapcar #'(lambda(s)(parse-parameter s (car par-type))) (split-string str)))
(t str))))

(defun typeof(v)
"when v is a keyword, returns corresp. symbol; otherwise returns v's type"
  (if(consp v) (if(eql(car v) 'quote) (typeof(cadr v))  (list(typeof(car v))))
    (cond
     ((keywordp v) (intern(substring(symbol-name v)1)))
     (t(type-of v)))))

(defun KP(x)(if(listp x)(KP(car x))(keywordp x)))
(defun typeof-expr(expr)
  (cond
   ((listp expr) (when-let((TO(typeof-expr(car expr)))) (list TO)))
   ((consp expr) nil)
   (t (let((TO(typeof expr)))
	(unless(eql TO 'symbol) TO)))))

(defmacro parse-conf(conf-expr vars)
"for =let-conf=: produces configuration (list of conses) updates variables =vars= conf-expr"
  (let((conf(s-gensym "conf")))
    `(let((,conf ,conf-expr))
(list ,@(mapcar
    #'(lambda(CV)
	(if(listp CV)
	    (if(cddr CV)

(let((TO(typeof(car CV))))
  `(cons (quote ,(cadr CV))
	 ,(if-let((cCV(caddr CV)))
	      `(or(parse-parameter(cdr(assoc (quote ,(cadr CV)) ,conf)) (quote ,TO)) ,cCV)
	    `(parse-parameter(cdr(assoc (quote ,(cadr CV)) ,conf)) (quote ,TO)))))

(let((TO(typeof-expr(car(if(KP(car CV)) CV (cdr CV)))))
     (varName(if(KP(car CV)) (cadr CV) (car CV)))
     (default(unless(KP(car CV))(cadr CV))))
  `(cons (quote ,varName)
	 ,(if default
	      `(or(parse-parameter(cdr(assoc (quote ,varName) ,conf)) (quote ,TO)) ,default)
	    `(parse-parameter(cdr(assoc (quote ,varName) ,conf)) (quote ,TO))))))

`(cons (quote ,CV) (parse-parameter(cdr(assoc (quote ,CV) ,conf))))))
	 vars)))))

(defmacro let-conf(configuration vars &rest body)
  `(let((,(car configuration) (parse-conf ,(cdr configuration) ,vars)))
,@body))

(defmacro let-conf-c(CName variable-name)
"for let-conf"
 `(cdr(assoc(quote ,variable-name) ,CName)))

(defmacro letc(conf-data vars &rest body)
(let((conf-var(s-gensym)))
  `(let((,conf-var (parse-conf ,conf-data ,vars)))
(let(,@(mapcar (lambda(v)
(let((VN(ifn(listp v)v(if(KP(car v))(cadr v)(car v)))))
`(,VN (let-conf-c ,conf-var ,VN))))
vars))
,@body))))

(defmacro setc(FN vars &rest body)
`(letc(read-conf-file ,FN) ,vars ,@body))

(defun print-variable(var)
(cond
((stringp var) var)
((integerp var) (format "%d" var))
((listp var) (together(mapcar #'print-variable var)))
(t
(clog :error "type of %s is undefined" var)
 "N/A")))
(defun write-conf(FN conf)
  (with-temp-file FN
(dolist(conf-item conf)
(insert(format "%s=%s
" (car conf-item)
(if(cdr conf-item) (print-variable (cdr conf-item)) "")))))
 t)

(defmacro make-conf(&rest variable-names)
  `(list
    ,@(mapcar (lambda(VN) `(cons (quote ,VN) ,VN)) variable-names)))

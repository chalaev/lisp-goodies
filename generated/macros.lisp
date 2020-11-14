;; -*- mode: Lisp; -*-
;; generated from https://notabug.org/shalaev/lisp-goodies/src/master/goodies.org
(defmacro iff (test-form then &rest else)
  "elisp-kind of if"
  (if (cdr else)
      `(if ,test-form ,then (progn ,@else))
      (if (car else)
	  `(if ,test-form ,then ,@else)
	  `(when ,test-form ,then))))

(defmacro when-let (vars &body body)
  `(let ((,(car vars) ,(car body)))
     (when ,(car vars)
	,(if (cdr vars)
	     (macroexpand-1 `(when-let ,(cdr vars) ,@(cdr body)))
	     `(progn ,@(cdr body))))))

(defmacro if-let (vars &body body)
  (let ((if-true (gensym "it")) (result (gensym "r")))
  `(let (,if-true)
     (let ((,result ,(append `(when-let ,vars) (subseq body 0 (length vars)) (list `(setf ,if-true T) (nth (length vars) body)))))
     (if ,if-true ,result ,(nth (1+ (length vars)) body))))))

(defmacro ifn (test ifnot &rest ifyes)
`(iff (not ,test) ,ifnot ,@ifyes))

(defmacro concat (&rest strs)
  `(concatenate 'string ,@strs))

(defmacro directory-lock(locked-dir by &rest body)
(let ((LD (gensym "ld")) (lock-file (gensym "LF")) (mkdir (gensym "md")) 
      (result (gensym "r")))
`(let* ((,LD (uiop:ensure-directory-pathname  ,locked-dir))
        (,mkdir (safe-mkdir ,LD)))
  (ifn (car ,mkdir) (cons nil (cons :lock (cdr ,mkdir)))
(let ((,lock-file (merge-pathnames #p"by" ,LD)))
  (echo-to-file ,lock-file ,by)
  (let ((,result (progn ,@body)))

(ifn (car (rm ,lock-file))) (cons nil (cons :file ,result))
(ifn (car (rmdir ,LD))) (cons nil (cons :dir ,result))
(cons t ,result)))))))

(defmacro cond-let (&rest conds)
  "cond with let"
  (let ((c (car conds)) (r (cdr conds)))
    (if (equal (car c) 'otherwise) (cons 'progn (cdr c))
    (if r
	`(if-let ,(car c) ,(cons 'progn (cdr c)) ,(macroexpand-1 `(cond-let ,@r)))
	`(when-let ,(car c) ,@(cdr c))))))

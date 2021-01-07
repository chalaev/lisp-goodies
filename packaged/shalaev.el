;;; shalaev.el --- my useful macros and functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Oleg Shalaev <oleg@chalaev.com>

;; Author:     Oleg Shalaev <oleg@chalaev.com>
;; Version:    2.0.8

;; URL:        https://github.com/chalaev/lisp-goodies

;;; Commentary:

;; This package contains macros and functions that I find particularly useful.
  
;;; Code:

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

(defmacro lett(var-defs &rest body)
"let where one can define (usual) local variables as well as local functions"
  (if(car var-defs)
      (let((ME (macroexpand-1 `(lett ,(cdr var-defs) ,@body))))
      (if(and(listp (car var-defs))(eql 'defun (caar var-defs)))
	  (let((func-data (cdar var-defs)))
	    `(let((,(car func-data) (lambda ,(cadr func-data) ,@(cddr func-data))))
	        ,ME))
	`(let(,(car var-defs)) ,ME)))
    `(progn ,@body)))
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
;; -*-  lexical-binding: t; -*-
(defun select (from-where match-test)
  "select items matching the test"
    (let (collected wasted)
       (dolist (list-item from-where)
	 (if (funcall match-test list-item)
	   (push list-item collected)
	   (push list-item wasted)))
(cons (reverse collected) (reverse wasted))))

(defun without(source &rest wrong-items)
  "returns (copy of) source without wrong-items"
  (car (select source #'(lambda(x) (not (member x wrong-items))))))

(defun email (addr &optional subject body)
  "fast non-interactive way to send an email"
  (compose-mail addr (if subject subject ""))
  (when body (insert body))
  (message-send-and-exit))

(defun pos (el ll)
  (let ((i 0) r)
  (dolist (e ll r)
    (if (eql e el)
	(setf r i)
      (s-incf i)))))

(defun time< (t1 t2)
  (and
    (time-less-p (time-add t1 3) t2)
    (not (time-less-p (time-add t2 3) t1))))

(defun parse-date (str)
  (mapcar 'string-to-number
	  (cond
 ((string-match "\\([0-9]\\{4\\}\\)[/-]\\([0-9][0-9]\\)[/-]\\([0-9][0-9]\\)" str) (mapcar #'(lambda (x) (match-string x str)) '(3 2 1)))
 ((string-match "\\([0-9][0-9]\\)[/-]\\([0-9][0-9]\\)[/-]\\([0-9]\\{4\\}\\)" str) (mapcar #'(lambda (x) (match-string x str)) '(2 1 3)))
 ((string-match "\\([0-9][0-9]\\)\\.\\([0-9][0-9]\\)\\.\\([0-9]\\{4\\}\\)" str) (mapcar #'(lambda (x) (match-string x str)) '(1 2 3)))
 ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9]\\{2\\}\\)" str) (mapcar #'(lambda (x) (match-string x str)) '(2 1 3)))
 ((string-match "\\([0-9]\\{2\\}\\)[/-]\\([0-9][0-9]\\)" str) (append (mapcar #'(lambda (x) (match-string x str)) '(2 1)) (list (format-time-string "%Y" (current-time)))))
 (t (clog :error "date format not recognized in %s" str) nil))))

(defun parse-only-time (str)
  (firstN (parse-time-string str) 3))

(defun parse-date-time(str)
  (if (string-match "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]" str)
      (parse-time-string str)
    (let ((SS (split-string str)))
      (append (parse-only-time (cadr SS))
	      (parse-date (car SS))))))

(defun read-conf-file(FN)
  "reads configuration file"
(with-temp-buffer(insert-file-contents FN)
(let (res)
(while-let(str) (< (line-end-position) (point-max))
(setf str (read-line))
  (unless(= ?# (string-to-char str)); ignoring comments
    (if (string-match "^\\(\\ca+\\)=\\(\\ca+\\)$" str)
      (push (cons (match-string 1 str) (match-string 2 str)) res))))
      (reverse res))))

(defun update-conf(conf &rest conf-params)
  (dolist (CP conf-params)
    (when-let ((CPV (cdr (assoc CP conf)))) (set (intern CP) CPV))))

(defun firstN(lista N)
  "returning first N elments of the list"
  (when (and (< 0 N) (car lista))
    (cons (car lista) (firstN (cdr lista) (1- N)))))

(require 'cl)
(defvar *good-chars*
(let ((forbidden-symbols '(?! ?@ ?# ?$ ?% ?& ?* ?\( ?\) ?+ ?= ?/ ?{ ?} ?\[ ?\] ?: ?\; ?< ?> ?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\")))
    (append
     (loop for i from ?A to ?Z unless (member i forbidden-symbols) collect i)
     (loop for i from ?a to ?z unless (member i forbidden-symbols) collect i)
     (loop for i from ?0 to ?9 unless (member i forbidden-symbols) collect i)))
"safe characters for file names: everuthing allowed except for what is forbidden")
(defun rand-str(N)
  (apply #'concat
     (loop repeat N collect (string (nth (random (length *good-chars*)) *good-chars*)))))

(defun land(args)
"'and' for a list"
  (reduce #'(lambda(x y) (and x y)) args :initial-value t))

(defun sforward-line()
"safe forward-line"
  (if (< (line-end-position) (point-max))
     (forward-line)
     (move-end-of-line 1)))
(defun read-line()
"returns current string of a buffer"
(prog1 
  (buffer-substring-no-properties (line-beginning-position) (line-end-position))
  (sforward-line)))
(defvar *log-level* 0)

(defvar *log-buffer* nil)

(let (last-FLD); saves last day printed to the log file
(defun log-flush()
  "save log messages to file for debugging"
  (when (= 0 *log-level*)
    (with-temp-buffer
      (let ((today-str (format-time-string "%04Y-%02m-%02d" (current-time))))
	(unless (string= today-str last-FLD)
	  (setf last-FLD today-str)
	  (insert today-str) (newline))
	(dolist (msg (reverse *log-buffer*))
	  (insert msg) (newline)))
      (append-to-file (point-min) (point-max) (concat emacs-d "elisp.log")))
    (setf *log-buffer* nil))))

(defun clog(level fstr &rest args)
  "simple logging function" ; level is one of → :debug :info :warning :error
(let ((log-push (lambda(msg)
  (push msg *log-buffer*)
  (when (< 30 (length *log-buffer*)) (log-flush)))))

(when (<= *log-level* (or (pos level '(:debug :info :warning :error)) 0))
  (let ((log-msg
	   (cons
	    (concat "%s " (format-time-string "%H:%M:%S "
(apply 'encode-time (butlast (decode-time (current-time)) 3)))
		    fstr)
	    (cons (symbol-name level) args))))
      (funcall log-push (apply #'format log-msg))
      (apply #'message log-msg)))
 nil))

(defun on-emacs-exit()
  (clog :debug "flushing comments before quiting emacs")
  (log-flush))

(add-hook 'kill-emacs-hook 'on-emacs-exit)
;; -*-  lexical-binding: t; -*-
(defmacro string-from-macro(m)
`(format "%s" (print (macroexpand-1 ,m) #'(lambda(x) (format "%s" x)))))

(require 'subr-x)

(unless (< 25 (car (emacs-ver)))
(defmacro when-let-key (key vars &rest body)
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
  (let ((vardef (car vardefs)))
    (if (and (listp vardef) (not (or (special-form-p (car vardef)) (functionp (car vardef)) (macrop (car vardef)))))
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

(defmacro define-vars (varDefs)
  "to make switching between local/global variables easier"
(cons 'progn
(mapcar #'(lambda(VD)
  (if (consp VD)
      `(defvar ,@VD)
      `(defvar ,VD nil)))
varDefs)))

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
  (let ((if-true (s-gensym "it")) (result (s-gensym "r")))
    `(let (,if-true ,result)
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

(defmacro while-let(var-defs while-cond &rest body)
  `(let* (,@var-defs)
     (while ,while-cond
       ,@body)))

(defmacro ifn (test ifnot &rest ifyes)
`(if (not ,test) ,ifnot ,@ifyes))

(defmacro end-push (what where)
"adds an item to the end of the list, resembles 'add-to-list'"
  `(if ,where (push ,what (cdr (last ,where)))
      (push ,what ,where)))
(provide 'shalaev)
;;; shalaev.el ends here

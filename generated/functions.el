;; -*- mode: Emacs-Lisp;  lexical-binding: t; -*-
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

(defun firstN(lista N)
  "returning first N elments of the list"
  (when (and (< 0 N) (car lista))
    (cons (car lista) (firstN (cdr lista) (1- N)))))

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

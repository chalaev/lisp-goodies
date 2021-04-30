;; -*-  lexical-binding: t; -*-
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

(defun together(strings)
"concatenates list of strings"
(if strings
  (mapconcat 'identity strings " ")
  ""))

(defun echo-to-file(FN &optional str)
 (write-region (or str "") nil (untilde FN))
 (tilde FN))

(defun firstN(lista N)
  "returning first N elments of the list"
  (when (and (< 0 N) (car lista))
    (cons (car lista) (firstN (cdr lista) (1- N)))))

(require 'cl-lib)
(defvar *good-chars*
(let ((forbidden-symbols '(?! ?@ ?# ?$ ?% ?& ?* ?\( ?\) ?+ ?= ?/ ?{ ?} ?\[ ?\] ?: ?\; ?< ?> ?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\")))
    (append
     (cl-loop for i from ?A to ?Z unless (member i forbidden-symbols) collect i)
     (cl-loop for i from ?a to ?z unless (member i forbidden-symbols) collect i)
     (cl-loop for i from ?0 to ?9 unless (member i forbidden-symbols) collect i)))
"safe characters for file names: everuthing allowed except for what is forbidden")
(defun rand-str(N)
  (apply #'concat
     (cl-loop repeat N collect (string (nth (random (length *good-chars*)) *good-chars*)))))

(defun land(args)
"'and' for a list"
  (cl-reduce #'(lambda(x y) (and x y)) args :initial-value t))

(defun sforward-line()
"safe forward-line"
  (if (< (line-end-position) (point-max))
     (forward-line)
     (move-end-of-line 1)))
(defun read-line(&optional max-size)
"returns current string of a buffer"
(let((max-size(or max-size 1024000)))
(if(< max-size (point-max))
   (clog :error "read-line> max-size= %d limit is too small for this large (%d) buffer" max-size (point-max))

(prog1
  (buffer-substring-no-properties (line-beginning-position) (min max-size(line-end-position)))
  (sforward-line)))))

(defun nth-column(n matrix)
  "returns n-th column of a matrix (n starts from zero)"
  (mapcar #'(lambda(line) (nth n line)) matrix))
(defun transpose(table)
(let((M(length(car table))) result)
(while(<= 0 (decf M))
(push(nth-column M table) result))
result))

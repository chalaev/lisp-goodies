;; -*-  lexical-binding: t; -*-
(unless (functionp 'caddr) (defun caddr(x) (car(cddr x)))); for emacs versions <26
(unless (functionp 'cadar) (defun cadar(x) (car (cdar x))))

(defun emacs-ver()
  (mapcar #'string-to-number (split-string
   (caddr (split-string (emacs-version))) "\\.")))

(unless (< 25 (car (emacs-ver)))
  (defun upgrade-make-temp-file(old-function PREFIX &optional DIR-FLAG SUFFIX TEXT)
    (let((FN (funcall old-function PREFIX DIR-FLAG SUFFIX)))
      (when (and TEXT (stringp TEXT))
      (write-region TEXT nil FN))
    FN))
(add-function :around (symbol-function 'make-temp-file) #'upgrade-make-temp-file))

(defvar ~ (file-name-as-directory (getenv "HOME")))
(defun tilde(x &optional HOME)
(let((H(or HOME ~)))

(replace-regexp-in-string (concat "^" H) "~/" x)))
(defun untilde(x &optional home-dir)
 (replace-regexp-in-string "^~/" 
   (or home-dir ~); do not use =file-name-as-directory= here as =home-dir= might be an *arbitrary* string (expression)
 x))
(defvar *emacs-d* (concat "~/" (file-name-as-directory ".emacs.d")))

(require 'package)
(unless (assoc "local-packages" package-archives)
  (push (cons  "local-packages" (concat *emacs-d* (file-name-as-directory "local-packages")))
	package-archives))
(make-directory (cdr (assoc "local-packages" package-archives)) t)

(unless(member(cdr(assoc "local-packages" package-archives)) load-path)
  (add-to-list 'load-path (cdr(assoc "local-packages" package-archives))))

(defun select (from-where match-test)
  "select items matching the test"
    (let (collected wasted)
       (dolist (list-item from-where)
	 (if (funcall match-test list-item)
	   (push list-item collected)
	   (push list-item wasted)))
(cons (reverse collected) (reverse wasted))))
(defun CPU-cores()
  "return number of CPU cores in linux"
  (length(car(select(process-lines "cat" "/proc/cpuinfo")
      (lambda(str) (string-match "processor[[:blank:]]+:[[:blank:]]+[[:digit:]]+$" str))))))

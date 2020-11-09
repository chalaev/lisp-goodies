;; -*- mode: Lisp; -*-
;; generated from https://notabug.org/shalaev/elisp-goodies/src/master/goodies.org
(defmacro iff (test-form then &rest else)
  "elisp-kind of if"
  (if (cdr else)
      `(if ,test-form ,then (progn ,@else))
      (if (car else)
	  `(if ,test-form ,then ,@else)
	  `(when ,test-form ,then))))

(defmacro ifn (test ifnot &rest ifyes)
`(iff (not ,test) ,ifnot ,@ifyes))

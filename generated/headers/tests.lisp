;;(in-package :cl-user)
(defpackage :shalaev/tests
  (:use :cl :shalaev/macros :shalaev/files :sb-rt)
  (:export :N-failed)

(:import-from :sb-rt :*compile-tests* :*expected-failures*))
(in-package :shalaev/tests)
(defvar N-failed 0 "how many tests failed")

;; generated from https://notabug.org/shalaev/lisp-goodies/src/master/goodies.org
(defun run-tests (&key ((:compiled *compile-tests*)))
  (unless (do-tests) (incf N-failed)))

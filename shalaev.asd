(asdf:defsystem "shalaev"
  :class :package-inferred-system
  :description "my lisp goodies"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version "0"
  :depends-on (:shalaev/files :shalaev/macros)
  :components ((:file "shalaev"))
  :in-order-to ((test-op (test-op "shalaev/tests"))))

(asdf:defsystem "shalaev/macros"
  :class :package-inferred-system
  :description "my lisp goodies"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version "0"
  :components ((:file "macros")))

(asdf:defsystem "shalaev/files"
  :class :package-inferred-system
  :description "my lisp goodies"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version "0"
  :depends-on (:shalaev/macros)
  :components ((:file "files")))

(asdf:defsystem "shalaev/tests"
  :class :package-inferred-system
  :description "testing"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version "0"
  :depends-on (:shalaev :sb-rt)
  :components ((:file "tests"))
  :perform (test-op (o c)
(flet ((run-tests (&rest args)
         (apply (intern (string '#:run-tests) '#:shalaev/tests) args)))
  (run-tests :compiled nil)
  (run-tests :compiled t))))

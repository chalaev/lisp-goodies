(defsystem "shalaev"
  :class :package-inferred-system
  :description "my lisp goodies"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :depends-on (:shalaev/files :shalaev/macros)
  :components ((:file "shalaev"))
  :in-order-to ((test-op (test-op "shalaev/tests"))))

(defsystem "shalaev/macros"
  :class :package-inferred-system
  :description "my lisp goodies"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :components ((:file "macros")))

(defsystem "shalaev/files"
  :class :package-inferred-system
  :description "my lisp goodies"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :depends-on (:shalaev/macros)
  :components ((:file "files")))

(defsystem "shalaev/tests"
  :class :package-inferred-system
  :description "testing"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :depends-on (:shalaev :sb-rt)
  :components ((:file "tests"))
  :perform (test-op (o c)
(flet ((run-tests (&rest args)
         (apply (intern (string '#:run-tests) '#:shalaev/tests) args)))
  (run-tests :compiled nil)
  (run-tests :compiled t))))

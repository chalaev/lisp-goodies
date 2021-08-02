(defsystem :shalaev
  :description "my lisp goodies"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :depends-on (:shalaev/files :shalaev/macros)
  :components ((:file "shalaev"))
  :in-order-to ((test-op (test-op "shalaev/tests"))))

(defsystem :shalaev/macros
  :description "my lisp macros"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :components ((:file "macros")))

(defsystem :shalaev/files
  :description "my lisp file functions"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :depends-on (:shalaev/macros)
  :components ((:file "files")))

(defsystem :shalaev/conf
  :description "reading configuration files"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :depends-on (:cl-ppcre :cl-ppcre-unicode :shalaev/macros)
  :components ((:file "conf")))

(defsystem :shalaev/tests
  :description "testing my lisp goodies"
  :author "Oleg Shalaev"
  :mailto "oleg@chalaev.com"
  :licence "MIT"
  :version (:read-file-line "version.org")
  :depends-on (:shalaev :shalaev/conf :sb-rt)
  :components ((:file "tests"))
  :perform (test-op (o c)
(flet ((run-tests (&rest args)
         (apply (intern (string '#:run-tests) '#:shalaev/tests) args)))
  (run-tests :compiled nil)
  (run-tests :compiled t))))

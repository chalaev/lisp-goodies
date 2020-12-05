(defpackage :shalaev/files
  (:use :cl :shalaev/macros)
  (:export :merge-paths :safe-mkdir :rmdir :echo-to-file :directory-lock))
(in-package :shalaev/files)

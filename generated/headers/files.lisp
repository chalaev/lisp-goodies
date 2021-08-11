(uiop:define-package :shalaev/files
  (:use :cl :shalaev/macros)
  (:export :without :drop ; ← this function(s)/macro(s) are not about files; should I rename this whole packafe or create another one?
:without-key :drop-key
:merge-paths :rmdir :echo-to-file :directory-lock))
(in-package :shalaev/files)

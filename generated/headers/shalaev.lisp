;;(in-package :cl-user)
(defpackage :shalaev
  (:use :cl :shalaev/macros :shalaev/files))
(in-package :shalaev)
(declaim (optimize (speed 0) (safety 3) (debug 3)))
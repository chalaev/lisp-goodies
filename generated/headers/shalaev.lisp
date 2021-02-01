;;(in-package :cl-user)
(uiop:define-package :shalaev
(:nicknames "sh") (:use :cl)
(:use-reexport :shalaev/macros :shalaev/files))
(in-package :shalaev)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

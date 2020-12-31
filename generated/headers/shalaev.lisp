;;(in-package :cl-user)
(defpackage :shalaev
(:nicknames "SH")
(:use :cl :shalaev/macros :shalaev/files)
 (:export
:end-push :iff :ifn :concat :aset :hset
:when-let
:if-let :ifn-let :cond-let
:needs))
(in-package :shalaev)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

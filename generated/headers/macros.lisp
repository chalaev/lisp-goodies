(uiop:define-package :shalaev/macros
(:nicknames "SHM")
  (:use :cl)
  (:export
:end-push :iff :ifn :concat :aset :hset
:case*  :when-let :case-expand :case-f
:if-let :ifn-let :cond-let
:needs))
(in-package :shalaev/macros)

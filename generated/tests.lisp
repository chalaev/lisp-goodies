(declaim (notinline id))
(defun id(x) x)

(deftest when-let
    (when-let ((x (id :ok)))
      (setf x (cons x x))
      x)
  (:ok . :ok))

(deftest if-let
    (if-let ((x (id :ok)))
            x
            :bad)
  :ok)
(deftest ifn-let
    (ifn-let ((x (id :ok)))
            :bad
x )
  :ok)

(deftest ifn
(let(a)
(multiple-value-bind (x y) (floor 44 11)
  (ifn(= y 0)
      (list 1 x y)
(push 1 a)
(push 2 a)
a)))
(2 1))

(deftest concat
  (concat "/etc/" "dqoE.tmp")
"/etc/dqoE.tmp")

(deftest aset
(aref
(let ((container (make-array 5)))
  (aset container 2 23987)
  container)
2)
23987)

(deftest hset
(gethash 'one-entry
(let ((container (make-hash-table)))
  (hset container 'one-entry 23987)
  container))
23987 t)

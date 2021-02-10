(deftest when-let
    (when-let ((x (identity :ok)))
      (setf x (cons x x))
      x)
  (:ok . :ok))

(deftest if-let
    (if-let ((x (identity :ok)))
            x
            :bad)
  :ok)
(deftest ifn-let
    (ifn-let ((x (identity :ok)))
            :bad
x )
  :ok)

(deftest without.1
(let((ll '(1 2 3 4)))
  (without ll 2))
(1 3 4))
(deftest drop.1
(let((ll '(1 2 3 4)))
  (drop ll '(2 4))
  ll)
(1 3))
(deftest without.2
(let((ll '((1 2) (3 4 5) (6 7 8 9))))
  (without ll '(2 3 7) :key #'cadr))
((3 4 5)))
(deftest drop.2
(let((ll '((1 2) (3 4 5) (6 7 8 9))))
  (drop ll '(2 3 7) :key #'cadr)
ll)
((3 4 5)))

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

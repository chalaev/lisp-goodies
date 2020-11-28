(deftest when-let.1
    (when-let ((x (id :ok)))
      (setf x (cons x x))
      x)
  (:ok . :ok))

(deftest if-let.1
    (if-let ((x (id :ok)))
            x
            :bad)
  :ok)

(deftest end-push.1
(let (container)
  (end-push 1 container)
  container)
(1))
(deftest end-push.2
(let (container)
  (end-push 1 container)
  (end-push 2 container)
  container)
(1 2))

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
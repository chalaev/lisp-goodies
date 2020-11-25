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

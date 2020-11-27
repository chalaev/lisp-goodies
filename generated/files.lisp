(defun safe-mkdir (dirname)
(handler-case (cons t (sb-posix:mkdir dirname #o770))
  (sb-posix:syscall-error (c)
    (cons nil
      (case (sb-posix:syscall-errno c)
        (13 :permission)
        (17 :exists)
        (2 :parent)
        (otherwise (cons :unknown (sb-posix:syscall-errno c))))))))

(defun rmdir(DN)
  (handler-case (cons t (sb-posix:rmdir DN))
    (sb-posix:syscall-error (c)
    (cons nil
      (case (sb-posix:syscall-errno c)
        (13 :permission)
        (2 :absent)
        (39 :occupied)
        (otherwise (cons :unknown (sb-posix:syscall-errno c))))))))

(defun echo-to-file (FN str)
  (with-open-file (stream FN
    :if-exists :overwrite
    :direction :output
    :if-does-not-exist :create)
(format stream "~a~%" str)))

(defun merge-paths(root-dir &rest sub-dirs)
  (reduce
   #'(lambda(DN FN) (merge-pathnames FN (uiop:ensure-directory-pathname DN)))
   sub-dirs
   :initial-value root-dir))

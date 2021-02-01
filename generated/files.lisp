(defun without-key(source wrong-items &key (key #'identity))
  "returns (copy of) source without wrong-items"
  (let((WI(if(listp wrong-items) wrong-items (list wrong-items))))
    (remove-if #'(lambda(x) (find (funcall key x) WI)) source)))
(defun without(source &rest WI)
  "returns (copy of) source without wrong-items"
    (remove-if #'(lambda(x) (find x WI)) source))
(define-modify-macro drop    (place &rest what) without)
(define-modify-macro drop-key(place &rest what) without-key)

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

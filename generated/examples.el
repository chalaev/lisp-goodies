
(let ((x 33) (choice-1 22)  (choice-2 33))
  (case= (1+ x)
         (choice-1 (message "choice-1"))
         (choice-2 (message "choice-2"))
         (otherwise (message "no match"))))

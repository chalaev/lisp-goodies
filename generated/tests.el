;; -*-  lexical-binding: t; -*-
(unless (< 25 (car (emacs-ver)))
(ert-deftest when-let-key()
(should (string= "aba" (when-let-key  #'identity ((a "a") (b (concat a "b")))
  (concat b "a"))))
(should (not (when-let-key  #'identity ((a "a") (b nil)) (concat "z" "a"))))
(should (not (when-let-key  #'identity ((c nil) (a "a") (b nil)) (concat "z" "a"))))))

(unless (< 25 (car (emacs-ver)))
(ert-deftest when-let*()
(should (string= "aba" 
   (when-let* ((a "a") (b (concat a "b")))
      (concat b "a"))))
(should (not (when-let* ((a "a") (b)) (concat "z" "a"))))
(should (not (when-let* (c (a "a") b) (concat "z" "a"))))))

(ert-deftest when-let()
  "Testing here when-let and when-let* defined in subr-x.el"
(should (string= "ba" (when-let (a "a") (concat "b" a))))
(should (string= "ba" (when-let ((a "a")) (concat "b" a))))
(should (string= "ba" (when-let ((a "a") (b "b")) (concat b a))))
(should (string= "aba" (when-let ((a "a") (b (concat a "b"))) (concat b "a"))))
(should (string= "aba" (when-let* ((a "a") (b (concat a "b"))) (concat b "a")))))

(ert-deftest if-let*()
  "Testing here if-let* defined in subr-x.el"
:expected-result (if (< 25 (car (emacs-ver))) :passed :failed)
(should (= 1 (if-let* ((a 3)) 1 2)))
(should (= 12 (if-let* ((a 3) (b (* 3 a))) (+ a b) (- a b)))))

(ert-deftest needs()
(should(string= "(let ((a (identity 1))) (when a (1+ a)))" (format "%s" (macroexpand-1 `(needs((a (identity 1))) (1+ a))))))
(should(string= "(let ((a (identity 1))) (if a (progn (1+ a)) (alarm)))" (format "%s" (macroexpand-1 `(needs((a (identity 1) (alarm))) (1+ a))))))
(should(= 2 (needs((a (identity 1) 100)) 2)))
(should(= 200 (needs((a (identity 1) 100) (b (identity nil) 200) (c (+ a 1) 300)) 55))))

(ert-deftest s-find.1()
  (should (equal '(3 4) (s-find 4 '((1 2) (3 4) (5 6)) #'cadr)))
  (should (= 3 (s-find 3 '(1 2 3 4 5))))
(let ((cumbersome-list '(141 142 143 144)))
  (should (= (s-find (* 12 12) cumbersome-list nil #'=) 144))
  (should (= (s-find (/ 144 2) cumbersome-list nil #'(lambda(x y) (= (* 2 x) y))) 144))
  (should (= (s-find 12 cumbersome-list nil #'(lambda(x y) (= (* x x) y))) 144)))
(let ((cumbersome-list '((141 142) (143 144))))
  (should (equal (s-find 12 cumbersome-list #'cadr #'(lambda(x y) (= y (* x x)))) '(143 144)))))
(ert-deftest s-find.2()
"this code comes from cloud project"
(let((plain(lambda(FR)(aref FR 0))) (cipher(lambda(FR)(aref FR 6)))
    (file-DB(list
  ["/home/user/proj/chat/chat.org" "shalaev" "shalaev" (24559 50916 0 0) 48756 420 "U3j"]
  ["~/proj/lisp-goodies/shalaev.org" "shalaev" "shalaev" (24552 57834 0 0) 432 61533 "Q8T"])))
(should(string= "/home/user/proj/chat/chat.org"   (funcall plain (s-find "U3j" file-DB cipher #'string=))))
(should(string= "~/proj/lisp-goodies/shalaev.org" (funcall plain (s-find "Q8T" file-DB cipher #'string=))))))

(ert-deftest lett()
(should(eval(let((fName(s-gensym)))
`(lett(z (a 2) (defun ,fName(x)(1+ x)) w)
 (functionp ,fName)))))

(should(not(let((fName(s-gensym)))
(functionp fName))))

(let((fName(s-gensym)))
(should(not(or (special-form-p fName) (functionp fName) (macrop fName) (boundp fName)))))

(should (= 6 (lett(z (a 2) (defun sw(x)(1+ x)) w)
  (unless (or z w)
    (setf z (funcall sw a))
    (* z a))))))

(ert-deftest select()
(let ((test-list  '(4 22 11 33 12 24 77)))
  (should (not (car (select test-list #'zerop))))
  (should (equal '(11 33 77) (car (select test-list #'cl-oddp))))
  (should (equal '(4 22 12 24) (car (select test-list #'cl-evenp))))))

(ert-deftest without()
(let ((test-list  '(4 22 11 33 12 24 77)))
  (should (equal '(4 22 11 33 77) (without test-list 12 24)))))

(ert-deftest drop()
(let ((test-list  '(4 22 11 33 12 24 77)))
  (drop test-list 12 24)
  (should (equal '(4 22 11 33 77) test-list))))

(ert-deftest perms-from-str()
  (should (= 432 (perms-from-str "-rw-rw----"))))

(ert-deftest perms-to-str()
  (should (string= "rw-rw-rwx" (perms-to-str #o667))))

(ert-deftest typeof-expr()
(should(eql 'integer (typeof-expr 1)))
(should(equal '(integer) (typeof-expr '(1 2))))
(should(eql 'string (typeof-expr "abc")))
(should(equal '(string) (typeof-expr '("abc" "def"))))
(should(not(typeof-expr '(incf z)))))

(ert-deftest end-push()
(should (equal '(1)
(let (container)
  (end-push 1 container)
  container)))
(should (equal '(1 2)
(let (container)
  (end-push 1 container)
  (end-push 2 container)
  container))))

(ert-deftest land()
  (should (land '(t t t t 1 2)))
  (should (not (land '(t t t nil 1 2)))))

;; LispTest.lisp
(defstruct person name)

(defun greet (p)
  (format t "Hello, ~A" (person-name p)))


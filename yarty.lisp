;;;; yarty.lisp

(in-package #:yarty)

(defvar *tests* ()
  "alist of ((package . (test-function-names*))*)")

(defvar *handle-errors* t
  "Handle errors within deftest and test/and by failing the test and
  printing a message.")

(defun run-tests (&optional package)
  "Runs all the tests defined by deftest. 
Returns output suitable for use by cl-test-grid."
  (let (failing-tests
        (package (find-package package)))
    (declare (special failing-tests))
    (mapc #'funcall (reverse (cdr (assoc package *tests*))))
    (print (if failing-tests
               (cons :failed-tests failing-tests)
               :ok))))

(defmacro test-and (&body forms)
  "Test that each form returns truthy.
If any don't, set the current test to failing."
  (let ((f (gensym)))
    (cond ((null forms) t)
          (t `(let (,f)
                (declare (special current-test failing-tests))
                (block handler
                  (handler-bind 
                      ((error (lambda (c)
                                (when *handle-errors*
                                  (return-from handler
                                    (format t "~&   test-and threw ~A~&" c))))))
                    (unwind-protect (setq ,f ,(car forms))
                      (when  (not ,f)
                        (progn (format t "~&  Failing Form ~S" ',(car forms))
                               (pushnew current-test failing-tests))))))
                (test-and ,@(cdr forms)))))))

(defmacro def-deftest (name obody documentation)
  (alexandria:with-gensyms (cons)
    `(defmacro ,name (name &body body)
       ,documentation
       `(let ((,',cons (or (assoc *package* *tests*)
                           (car (push (cons *package* ()) *tests*)))))
          (pushnew ',name (cdr ,',cons))
          (defun ,name ()
            (let ((current-test ',name))
              (declare (special current-test failing-tests))
              (handler-bind ((error
                              (lambda (c)
                                (pushnew current-test failing-tests)
                                (when *handle-errors*
                                  (return-from ,name 
                                    (format t "~&   toplevel threw ~A~&" c))))))
                (,',obody ,@body))))))))

(def-deftest deftest progn
  "Define a function that will be called during run-tests. Insert forms
which should return truthy inside a TEST-AND.")

(def-deftest deftest/and test-and
  "Like DEFTEST but wraps its body in a test-and.")

(defmacro signals-a (condition &body body)
  "Returns true if body signals the condition."
  `(handler-case (prog1 () ,@body)
     (,condition (c) (declare (ignore c)) t)))

(defun clear-tests (&optional (package *package*))
  (assocf (find-package package) () *tests*))

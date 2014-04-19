;;;; yarty.lisp

(in-package #:yarty)

(defvar *tests* ()
  "An alist of ((package . (test-function-names*))*)")

(defvar *handle-errors* t
  "t: handle errors in tests; nil: decline to handle. Default is t.")

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
  (cond ((null forms) t)
        (t
         (let* ((f (gensym))
                (len (if (listp (car forms))
                         (length (car forms))
                         ()))
                (args (alexandria:make-gensym-list
                       (if (and len (plusp len)) (1- len) 0))))
           `(let (,f
                  ,@args)
              (declare (ignorable ,@args))
              (declare (special current-test failing-tests))
              (block handler
                (handler-bind
                    ((error (lambda (c)
                              (when *handle-errors*
                                (return-from handler
                                  (format t "~&   test-and threw ~A~&" c))))))
                  (unwind-protect
                       ,(if (and (listp (car forms))
                                 (symbol-function (caar forms))
                                 (not (special-operator-p (caar forms)))
                                 (not (macro-function (caar forms))))
                            `(setq ,@(alexandria:mappend
                                      #'list args (cdar forms))
                                   ,f (funcall ',(caar forms) ,@args))
                            `(setq ,f ,(car forms)))
                    (when  (not ,f)
                      (progn (format t "~&  Failing Form ~A" ',(car forms))
                             ,(when (and args
                                         (listp (car forms))
                                         (symbol-function (caar forms))
                                         (not (special-operator-p (caar forms)))
                                         (not (macro-function (caar forms))))
                                    `(format t "~&         NULL: (~A~{ ~A~^~})"
                                             ',(caar forms)
                                             (list ,@args)))
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
  "Define a function that will be called during run-tests. 

Insert forms which should return truthy inside a TEST-AND.")

(def-deftest deftest/and test-and
  "Like DEFTEST but wraps its body in a test-and.")

(defmacro signals-a (condition &body body)
  "Returns true if body signals the condition."
  `(handler-case (prog1 () ,@body)
     (,condition (c) (declare (ignore c)) t)))

(defun clear-tests (&optional (package *package*))
  "Clear the tests for the given package, default to *package*.

If nil is given as an explicit argument, clear all tests for all
packages."
  (if package
      (assocf (find-package package) () *tests*)
      (setq *tests* ())))

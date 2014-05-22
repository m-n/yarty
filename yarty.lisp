;;;; yarty.lisp

(in-package #:yarty)

(defvar *tests* ()
  "An alist of ((package . (test-function-names*))*)")

(defvar *handle-errors* t
  "t: handle errors in tests; nil: decline to handle. Default is t.")

(defvar *restart-queue* (lparallel.queue:make-queue :fixed-capacity 1))
(defvar *in-progress-queue* (lparallel.queue:make-queue :fixed-capacity 1))
(defvar *test-system* ())

(defun run-tests (&optional (package *package*))
  "Runs all the tests defined by DEFTEST in a given package.

Returns output suitable for use by cl-test-grid."
  (let (failing-tests
        (package (find-package package)))
    (declare (special failing-tests))
    (flet ((finish ()
             (print (if failing-tests
                        (cons :failed-tests failing-tests)
                        :ok)))
           (restart ()
             (return-from run-tests (test-system *test-system*))))
      (dolist (test (reverse (cdr (assoc package *tests*))) (finish))
        (let ((restartp (lparallel.queue:peek-queue *restart-queue*)))
          (cond (restartp
                 (lparallel.queue:try-pop-queue *restart-queue*)
                 (restart))
                (t
                 (funcall test))))))))

(defmacro each (&body forms)
  "Test that each form returns truthy.

If any don't, set the current test to failing."
  (cond ((null forms) t)
        (t
         (let* ((f (gensym))
                (errp (gensym))
                (len (if (listp (car forms))
                         (length (car forms))
                         ()))
                (args (alexandria:make-gensym-list
                       (if (and len (plusp len)) (1- len) 0))))
           `(let (,f
                  ,errp
                  ,@args)
              (declare (ignorable ,@args))
              (ensure-dynamic-bindings (current-test failing-tests)
                (block handler
                  (handler-bind
                      ((error (lambda (c)
                                (setq ,errp t)
                                (when *handle-errors*
                                  (return-from handler
                                    (format t "~&   each in ~A threw ~A~&"
                                            current-test
                                            c))))))
                    (unwind-protect
                         ,(if (and (listp (car forms))
                                   (function-name-p (caar forms)))
                              `(setq ,@(alexandria:mappend
                                        #'list args (cdar forms))
                                     ,f (funcall ',(caar forms) ,@args))
                              `(setq ,f ,(car forms)))
                      (when  (not ,f)
                        (pushnew current-test failing-tests)
                        (cond ((not ,errp)
                               (format t "~&  In ~A" current-test)
                               (format t "~&  Failing Form ~A"
                                       ',(car forms))
                               ,(when (and args
                                           (listp (car forms))
                                           (function-name-p (caar forms)))
                                      `(format
                                        t
                                        "~&               (~A~{ ~S~^~})"
                                        ',(caar forms)
                                        (list ,@args))))
                              (t
                               (format t "~&  Erroring Form ~A"
                                       ',(car forms)))))))))
              (each ,@(cdr forms)))))))

(defmacro def-deftest (name obody documentation)
  (alexandria:with-gensyms (cons)
    `(defmacro ,name (name &body body)
       ,documentation
       `(let ((,',cons (or (assoc *package* *tests*)
                           (car (push (cons *package* ()) *tests*)))))
          (pushnew ',name (cdr ,',cons))
          (defun ,name ()
            (let ((current-test ',name))
              (declare (special current-test))
              (ensure-dynamic-bindings (failing-tests)
                (handler-bind ((error
                                (lambda (c)
                                  (pushnew current-test failing-tests)
                                  (when *handle-errors*
                                    (return-from ,name
                                      (format t "~&   ~A's toplevel threw ~A~&"
                                              current-test
                                              c))))))
                  (,',obody ,@body)
                  (if failing-tests
                      (cons :failed-tests failing-tests)
                      :ok)))))))))

(def-deftest deftest progn
  "Define a function that will be called during RUN-TESTS.

Insert forms which should return truthy inside an EACH.")

(def-deftest deftest/each each
  "Like DEFTEST but wraps its body in an EACH.")

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

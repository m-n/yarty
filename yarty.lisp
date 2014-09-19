;;;; yarty.lisp

(in-package #:yarty)

(defvar *tests* ()
  "An alist of ((package . (test-function-names*))*)")

(defvar *handle-errors* t
  "t: handle in tests; nil: decline to handle. Default is t.")

(defvar *output* (make-synonym-stream '*standard-output*)
  "The stream testing info is print to.

This defaults to a *standard-output* synonym-stream.")

(defvar *restart-queue* (lparallel.queue:make-queue :fixed-capacity 1))
(defvar *in-progress-queue* (lparallel.queue:make-queue :fixed-capacity 1))
(defvar *test-system* ())

(define-condition test-results ()
  ((results :initarg :results :reader results))
  (:documentation
   "RUN-TESTS signals this with its result before returning."))

(defun test-system (system &key quit)
  "Test the system. Either return the last value of RUN-TESTS or quit the image.

Internally calls ASDF:TEST-SYSTEM. If quit is nil, then this records
the result of any calls to RUN-TESTS made during ASDF:TEST-SYSTEM and
returns the last one.

If QUIT is true then it exits the image after testing. In this case
the exit code of the process indicates the status of the tests.

        Exit Code    Status
        0            Tests Succeeded
        1            Tests Failed
        125          Could Not Test"
  (let (res)
    (handler-bind ((test-results
                    (lambda  (c)
                      (setq res (results c))))
                   (error
                    (lambda (c)
                      (when quit
                        (format *output* "Testing aborted due to error \"~A.\"" c)
                        (quit 125)))))
      (funcall (find-symbol (string :test-system) :asdf) system)
      (if quit
          (case res
            (:ok (quit 0))
            (otherwise (quit 1)))
          res))))

(defun run-tests (&rest packages)
  "Runs all the tests defined by DEFTEST in the given packages.

Returns output suitable for use by cl-test-grid."
  (let (failing-tests
        (packages (if packages
                      (mapcar (lambda (p)
                                (let ((pac (find-package p)))
                                  (if pac
                                      pac
                                      (error "Package ~A not found." p))))
                              packages)
                      (list *package*))))
    (declare (special failing-tests))
    (flet ((finish ()
             (let ((res (if failing-tests
                            (list :failed-tests (mapcar #'string-downcase
                                                        failing-tests))
                            :ok)))
               (print res *output*)
               (signal (make-condition 'test-results :results res))
               res))
           (restart ()
             (declare (ftype function autorun-test-system))
             (return-from run-tests (autorun-test-system *test-system*))))
      (dolist (test
                  (alexandria:mappend (lambda (p)
                                        (reverse (cdr (assoc p *tests*))))
                                      packages)
                (finish))
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
                  (handler-bind ((error
                                  (lambda (c)
                                    (setq ,errp t)
                                    (format *output* "~&   each in ~A threw ~A~&"
                                            current-test
                                            c)
                                    (if *handle-errors*
                                        (return-from handler)
                                        (restart-case (invoke-debugger c)
                                          (continue ()
                                            :report "Continue testing."
                                            (return-from handler)))))))
                    (unwind-protect
                         ,(if (and (listp (car forms))
                                   (function-name-p (caar forms)))
                              `(setq ,@(alexandria:mappend
                                        #'list args (cdar forms))
                                     ,f (funcall #',(caar forms) ,@args))
                              `(setq ,f ,(car forms)))
                      (when  (not ,f)
                        (pushnew current-test failing-tests)
                        (cond ((not ,errp)
                               (when current-test
                                 (format *output* "~&  In ~A" current-test))
                               (format *output* "~&  Failing Form ~A"
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
                               (format *output* "~&  Erroring Form ~A"
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
                (block handler
                  (handler-bind ((error
                                  (lambda (c)
                                    (pushnew current-test failing-tests)
                                    (format *output* "~&   ~A's toplevel threw ~A~&"
                                            current-test
                                            c)
                                    (if *handle-errors*
                                        (return-from handler)
                                        (restart-case (invoke-debugger c)
                                          (continue ()
                                            :report "Continue testing."
                                            (return-from handler)))))))
                    (,',obody ,@body)))
                (if failing-tests
                    (cons :failed-tests failing-tests)
                    :ok))))))))

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

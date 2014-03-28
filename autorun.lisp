;;;; autorun.lisp

(in-package #:yarty)

(defun monitor (filename fn &optional (monitor-fn 'fam-monitor-file))
  (bt:make-thread
   (lambda ()
     (funcall monitor-fn filename)
     (let ((channel (lparallel:make-channel)))
       (loop (let ((fam-event (cl-fam:fam-next-event)))
               (handler-case (lparallel.kernel:try-receive-result channel)
                 (error () ()))
               ;; abort current attempt, if in progress
               (lparallel:kill-tasks :default)
               (lparallel:submit-task channel fn fam-event)))))
   :name (namestring filename)
   :initial-bindings  `((cl-fam:*fam* . (cl-fam:fam-open
                                         (namestring ,filename) ()))
                        (*package* . ,*package*)
                        (lparallel:*kernel* . ,(lparallel:make-kernel
                                                1
                                                :name
                                                (with-output-to-string (s)
                                                  (princ filename s)))))))

(defmacro with-watched-filesystem
    ((var path &optional (path-type (or :directory :file)))
               &body body)
  `(monitor ,path (lambda (,var) ,@body)
            (case ,path-type
              ((:directory) #'cl-fam:fam-monitor-directory)
              ((:file)      #'cl-fam:fam-monitor-file)
              (otherwise
               (error
                "~A not one of :directory, :file in with-watched-filesystem"
                ,path-type)))))

(defun test (&optional (system (make-symbol (package-name *package*))))
  (asdf:test-system system))

(defun test-on-change
    (&key (system (make-symbol (package-name *package*)))
          (location (directory-namestring
                     (asdf/system:system-source-file
                      system)))
          (location-type (or :directory :file)))
  "Start a background thread which runs tests when location is modified.

System should be a string or symbol designating a package
name. Location defaults to the directory containing the
asdf:system-source-file. location-type is either :directory or :file,
defaulting to :directory."
  (unless cl-fam:*fam* (cl-fam:fam-init "tests"))
  (when (stringp system)
    (setq system (make-symbol (string-upcase system))))
  (with-watched-filesystem (event location location-type)
    (when (eq (cl-fam:fam-code event) :fam-changed)
      ;; gutted from quicklisp's call-with-quiet-compilation
      (let ((*compile-verbose* nil)
            (*compile-print* nil)
            (*load-verbose* nil)
            (*load-print* nil))
        (handler-bind ((warning #'muffle-warning))
          (handler-case (test system)
            (error (c)
              (pprint c *error-output*)
              (print '(:tests-errored)))))))))

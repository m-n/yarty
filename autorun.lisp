;;;; autorun.lisp

(in-package #:com.bareyak.test)

(defun monitor (filename fn &optional (monitor-fn 'fam-monitor-file))
  (bt:make-thread
   (lambda ()
     (funcall monitor-fn filename)
     (loop (let ((fam-event (cl-fam:fam-next-event)))
             (funcall fn fam-event))))
   :name (namestring filename)
   :initial-bindings  `((*fam* . (cl-fam:fam-open (namestring ,filename) ()))
                        (*package* . ,*package*))))

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
  (asdf/operate:test-system system))

(defun test-on-change
    (&optional (system (make-symbol (package-name *package*)))
               (location (directory-namestring
                          (asdf/system:system-source-file
                           system)))
               (location-type (or :directory :file)))
  "Start a background thread which runs tests when location is modified.

System should be a symbol or a package. Location defaults to the
directory containing the asdf:system-source-file. location-type is
either :directory or :file, defaulting to :directory."
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
          (test system))))))
